module OpenApi

open Microsoft.OpenApi.Any
open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Readers
open System.IO

let inline trimLower (s: string) =
    if isNull s then None else
    s.Trim().ToLowerInvariant() |> Some

let inline splitLine (str: string) =
    str.Split('\n')
    |> Seq.toList

type Docs =
    { Summary: string list option
      Remarks: string list option
      Example: string list option }
    static member Create(summary, remarks, example: IOpenApiAny) =
        match Option.ofObj summary, Option.ofObj remarks, Option.ofObj example with
        | None, None, None -> None
        | summary, remarks, example ->
            { Summary = summary |> Option.map splitLine
              Remarks = remarks |> Option.map splitLine
              Example = example |> Option.map (string >> splitLine) }
            |> Some
        
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
        
/// Active patterns to quickly determine what type OpenApiSchema represents
let (|ArrayKind|ObjectKind|PrimKind|) (schema: OpenApiSchema) =
    match trimLower schema.Type with
    | Some "array"  | None when not (isNull schema.Items)      -> ArrayKind  schema.Items
    | Some "object" | None when not (isNull schema.Properties) -> ObjectKind schema
    | Some primType -> PrimKind primType
    | _ -> failwithf "unexpected type of schema: %A" schema.Type
        
/// All primitive type kinds for a fields in type definitions
type PrimTypeKind =
    | Int
    | Long
    | Double
    | Bool
    | Any
    | String of StringFormat
    
    static member Parse (typeKind: string, format: string) =
        
        match trimLower typeKind, trimLower format with
        | Some "integer", Some "int64" -> Long 
        | Some "integer", _ -> Int
        | Some "number", _ -> Double
        | Some "boolean", _ -> Bool
        | Some "string", maybeStringFormat -> String(StringFormat.Parse maybeStringFormat)
        | _ -> failwithf "Unexpected type: %s and format: %A" typeKind format

/// IR for object schemas
and ObjectKind =
    { Name: string option
      Properties: (string * TypeKind) list
      Docs: Docs option }
    static member Create(schema: OpenApiSchema) =
        let name =
            Option.ofObj schema.Reference
            |> Option.map (fun x -> x.Id)
            |> Option.orElseWith(fun _ -> Option.ofObj schema.Title)
        let fields =
            [ for KeyValue(typeName, internalSchema) in schema.Properties ->
                typeName, TypeKind.Parse(internalSchema) ]
        
        { Name = name
          Properties = fields
          Docs = Docs.Create(schema.Description, null, schema.Example) }

/// Kind of types for fields in schemas
/// Either primitive or Array<T> or Option<T> or Object with properties
/// Array, Option and Object could recursively contains similar types
and TypeKind =
    | Prim of PrimTypeKind
    | Array of TypeKind
    | Option of TypeKind
    | Object of ObjectKind
    static member Parse(schema: OpenApiSchema): TypeKind =
        
        if isNull schema then Prim PrimTypeKind.Any else

        let kind =
            match schema with
            | ArrayKind items ->
                TypeKind.Parse items
                |> Array
            | ObjectKind schema ->
                ObjectKind.Create(schema)
                |> Object
            | PrimKind primType ->
                PrimTypeKind.Parse(primType, schema.Format)
                |> Prim

        if schema.Nullable then
            Option kind
        else kind

/// Representation of schema from OpenAPI
and TypeSchema =
    { Name: string
      Kind: TypeKind
      Docs: Docs option }
    static member Parse(name, schema: OpenApiSchema): TypeSchema =
        { Name = name
          Kind = TypeKind.Parse schema
          Docs = Docs.Create(schema.Description, null, schema.Example) }

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
      Responses: Response list
      Docs: Docs option }

/// Representation of OpenApi path with methods attach
/// e.g.: "/v2/item" with GET and SET
type ParsedPath =
    { Route: string
      Methods: PathMethodCall list
      Docs: Docs option }

/// Representation of a whole spec
type Api =
    { Name: string
      Paths: ParsedPath list
      Schemas: TypeSchema list
      Docs: Docs option }

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
    | Some "application/json" -> Json
    | Some x -> Other x
    | None -> failwith "Media type can't be null!"
    
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
                    let responses =
                        [ for KeyValue(code, response) in op.Responses do
                            for KeyValue(mediaType, content) in response.Content do
                                yield { Code = parseCode code
                                        MediaType = parseMediaType mediaType
                                        Kind = TypeKind.Parse content.Schema } ]
                    if responses.Length > 7 then
                        failwith "There could be only 7 or lower combinations of (mediaType * responseCode) per one path"
                    
                    yield { Method = string method
                            Name = op.OperationId
                            Responses = responses
                            Docs = Docs.Create(op.Description, op.Summary, null) } ]
                
            yield { Route = route
                    Methods = methods
                    Docs = Docs.Create(path.Description, path.Summary, null) } ]
        
    { Name = title
      Paths = paths
      Schemas = schemas
      Docs = Docs.Create(doc.Info.Description, null, null)} 
