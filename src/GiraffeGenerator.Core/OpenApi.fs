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
/// Either: primitive or Array<T> or Object with properties
/// Array and Object could recursively contains similar types
and TypeKind =
    | Prim of PrimTypeKind
    | Array of TypeDefinition
    | Object of TypeDefinition list

/// Representation of schema from OpenAPI
and TypeDefinition =
    { Name: string
      Nullable: bool
      Kind: TypeKind }
    static member Parse(name, schema: OpenApiSchema) =

        let kind =
            match trimLower schema.Type with
            | "array" ->
                let innerType = TypeDefinition.Parse(name, schema.Items)
                Array innerType

            | "object" ->
                let fields =
                    schema.Properties
                    |> Seq.map (fun (KeyValue(typeName, internalSchema)) ->
                        TypeDefinition.Parse(typeName, internalSchema))
                    |> Seq.toList
                Object fields

            | primType ->
                let primType = PrimTypeKind.Parse(primType, schema.Format)
                Prim primType

        { Name = name
          Nullable = schema.Nullable
          Kind = kind }

/// Endpoint call with name (should be unique across whole API at the moment) and method attached
type PathMethodCall =
    { Method: string
      Name: string }

/// Representation of OpenApi path with methods attach
/// e.g.: "/v2/item" with GET and SET
type ParsedPath =
    { Route: string
      Methods: PathMethodCall list }

/// Representation of a whole spec
type Api =
    { Name: string
      Paths: ParsedPath list
      Schemas: TypeDefinition list }

let reader = OpenApiStringReader()

/// read openApiSpec by filePath
/// returning tuple with parsed document and diagnostic errors
let read file =
    File.ReadAllText file |> reader.Read

/// Parse OpenApi document into our representation of it
/// Returning API name and list of ParsedPaths
let parse (doc: OpenApiDocument): Api =
    let title = doc.Info.Title.Replace(" ", "")

    let paths =
        [ for KeyValue(route, path) in doc.Paths do
            let methods =
                [ for KeyValue(method, op) in path.Operations do
                    yield { Method = string method
                            Name = op.OperationId } ]
            yield { Route = route
                    Methods = methods } ]

    let schemas =
        [ if not (isNull doc.Components) then
            for KeyValue(typeName, schema) in doc.Components.Schemas ->
                TypeDefinition.Parse(typeName, schema) ]

    { Name = title
      Paths = paths
      Schemas = schemas }
