module OpenApi

open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Readers
open System.IO

type PathMethodCall =
    { Method: string
      Name: string }

/// Our naive representation of OpenApi
type ParsedPath =
    { Route: string
      Methods: PathMethodCall list }
    
type Api =
    { Name: string
      Paths: ParsedPath list }
        
let reader = OpenApiStringReader()

/// read openApiSpec by filePath
/// returning tuple with parsed document and diagnostic errors
let read file =
    File.ReadAllText file
    |> reader.Read
    
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

    { Name = title
      Paths = paths }