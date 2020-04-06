module OpenApi

open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Readers
open System.IO

let inline trimLower (s: string) = s.Trim().ToLowerInvariant()

/// Formats for string type.
/// URI is not standard, but included
/// It could be anything
type StringFormat =
    | String
    | Byte
    | Binary
    | DateString
    | DateTimeString
    | PasswordString
    | Custom of string
    
    static member Parse (maybeFormat: string Option) =
        if maybeFormat.IsNone then String else
        match maybeFormat.Value with
        | "byte" -> Byte
        | "binary" -> Binary
        | "date" -> DateString
        | "date-time" -> DateTimeString
        | "password" -> PasswordString
        | x -> Custom x
        
/// All primitive type kinds for a fields in type definitions
type PrimTypeKind =
    | Int
    | Long
    | Double
    | Bool
    | String of StringFormat
    
    static member Parse (typeKind: string, format: string) =
        
        let format =
            Option.ofObj format
            |> Option.map trimLower
        
        match trimLower typeKind, format with
        | "integer", Some "int64" -> Long 
        | "integer", _ -> Int
        | "number", _ -> Double
        | "boolean", _ -> Bool
        | "string", maybeStringFormat -> String(StringFormat.Parse maybeStringFormat)
        | _ -> failwithf "Unexpected type: %s and format: %A" typeKind format

/// Kind of types for fields in schemas
/// Either primitive or Array<T> or Option<T> or Object with properties
/// Array, Option and Object could recursively contains similar types
and TypeKind =
    | Prim of PrimTypeKind
    | Array of TypeKind
    | Option of TypeKind
    | Object of string option * (string * TypeKind) list
    static member Parse(schema: OpenApiSchema): TypeKind =
        let kind =
            match trimLower schema.Type with
            | "array" ->
                let innerType = TypeKind.Parse schema.Items
                Array innerType

            | "object" ->
                let id =
                    Option.ofObj schema.Reference
                    |> Option.map (fun x -> x.Id)
                let fields =
                    [ for KeyValue(typeName, internalSchema) in schema.Properties ->
                        typeName, TypeKind.Parse(internalSchema) ]
                Object (id, fields)

            | primType ->
                let primType = PrimTypeKind.Parse(primType, schema.Format)
                Prim primType

        if schema.Nullable then
            Option kind
        else kind

/// Representation of schema from OpenAPI
and TypeSchema =
    { Name: string
      Kind: TypeKind }
    static member Parse(name, schema: OpenApiSchema): TypeSchema =
        { Name = name
          Kind = TypeKind.Parse schema }

/// Supported response media types
type MediaType =
    | Json
    | Other of string

/// Typed response with corresponding HTTP code, media type and content type
type Response =
    { Code: int
      MediaType: MediaType
      Kind: TypeKind }

/// Endpoint call with name (should be unique across whole API at the moment) and method attached
type PathMethodCall =
    { Method: string
      Name: string
      Responses: Response list }

/// Representation of OpenApi path with methods attach
/// e.g.: "/v2/item" with GET and SET
type ParsedPath =
    { Route: string
      Methods: PathMethodCall list }

/// Representation of a whole spec
type Api =
    { Name: string
      Paths: ParsedPath list
      Schemas: TypeSchema list }

let reader = OpenApiStringReader()

/// read openApiSpec by filePath
/// returning tuple with parsed document and diagnostic errors
let read file =
    File.ReadAllText file |> reader.Read

let inline private parseCode x =
    let isSuccess, result = System.Int32.TryParse x
    if isSuccess then result
    else failwith "Other types of `responses` map key rather than INTEGER are not supported"

let inline private parseMediaType x =
    match trimLower x with
    | "application/json" -> Json
    | x -> Other x
    
let private parseResponses (operation: OpenApiOperation) =
    [ for KeyValue(code, response) in operation.Responses do
        for KeyValue(mediaType, content) in response.Content do
            yield { Code = parseCode code
                    MediaType = parseMediaType mediaType
                    Kind = TypeKind.Parse content.Schema } ]
    
/// Parse OpenApi document into our representation of it
/// Returning API name and list of ParsedPaths
let parse (doc: OpenApiDocument): Api =
    let title = doc.Info.Title.Replace(" ", "")

    let schemas =
        [ if not (isNull doc.Components) then
            for KeyValue(typeName, schema) in doc.Components.Schemas ->
                TypeSchema.Parse(typeName, schema) ]

    let paths =
        [ for KeyValue(route, path) in doc.Paths do
            let methods =
                [ for KeyValue(method, op) in path.Operations do
                    
                    let responses = parseResponses op
                    if responses.Length > 7 then
                        failwith "There could be only 7 or lower combinations of (mediaType * responseCode) per one path"
                    
                    yield { Method = string method
                            Name = op.OperationId
                            Responses = responses } ]
                
            yield { Route = route
                    Methods = methods } ]    
    
    { Name = title
      Paths = paths
      Schemas = schemas }
