module OpenApi

open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Readers
open System.IO

/// Our naive representation of OpenApi
type ParsedPath =
    { Route: string
      Method: string
      Name: string }
        
let reader = OpenApiStringReader()

/// read openApiSpec by filePath
/// returning tuple with parsed document and diagnostic errors
let read file =
    File.ReadAllText file
    |> reader.Read
    
/// Parse OpenApi document into our representation of it
/// Returning API name and list of ParsedPaths
let parse (doc: OpenApiDocument) =
    let title = doc.Info.Title.Replace(" ", "")
    
    let routes = 
        doc.Paths
        |> Seq.collect(fun (KeyValue(route, path)) ->
            path.Operations
            |> Seq.map(fun (KeyValue(method, op)) ->
                { Route = route
                  Method = method.ToString().ToUpperInvariant()
                  Name = op.OperationId }
                )
            )
        |> Seq.toList
    
    title, routes