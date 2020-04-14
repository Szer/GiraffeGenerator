module CodeGen

open System.Collections.Generic
open System.IO
open AST
open OpenApi
open Fantomas
open FSharp.Compiler.Ast

let inline (^) f x = f x

module XmlDoc =
    
    /// | <{name}>
    /// | {lines}
    /// | </name>
    let tag name lines =
        [ yield "<" + name + ">"
          yield! lines
          yield "</" + name + ">" ]
    
    /// | <summary>
    /// | {lines}
    /// | </summary>
    let summary lines = tag "summary" lines
    
    /// | <remarks>
    /// | {lines}
    /// | </remarks>
    let remarks lines = tag "remarks" lines
    
    /// | <example>
    /// | {lines}
    /// | </example>
    let example lines = tag "example" lines

/// helper function to create partial Giraffe flow:
/// {method.Method} >=> service.{method.Name}
let createMethod (method: PathMethodCall) =
    method.Method.ToUpperInvariant() |> identExpr >=> service method.Name

/// helper function to create Giraffe flows:
/// route {path.Route} >=> {method.Method} >=> service.{method.Name}
/// or
/// route {path.Route} >=> choose [
///     {method1.Method} >=> service.{method1.Name}
///     {method2.Method} >=> service.{method2.Name}
/// ]
let createRoute (path: ParsedPath) =
    let methods =
        if path.Methods.Length > 1 then
            chooseExpr [ for method in path.Methods -> createMethod method ]
        else
            createMethod path.Methods.[0]
    route path.Route >=> methods

/// matching OpenAPI string IR to SynType
let strFormatMatch = function
    | StringFormat.String
    | PasswordString -> stringType
    | Byte -> arrayOf byteType
    | Binary -> arrayOf byteType
    | DateString
    | DateTimeString -> dateType
    | Custom "uri" | Custom "uriref" -> uriType
    | Custom "uuid" | Custom "guid" | Custom "uid" -> guidType
    | Custom name -> synType name

/// matching OpenAPI primitive type IR to SynType
let primTypeMatch = function
    | Any -> objType
    | Int -> intType
    | Long -> int64Type
    | Double -> doubleType
    | Bool -> boolType
    | String strFormat -> strFormatMatch strFormat

/// matching type kinds in responses to create their syntatic types
let rec extractResponseSynType = function
    | Prim primType -> primTypeMatch primType
    | Array innerType -> arrayOf (extractResponseSynType innerType)
    | Option innerType -> optionOf (extractResponseSynType innerType)
    | Object { Name = Some name } -> synType name
    | Object anonObject ->
        let fields =
            anonObject.Properties
            |> List.map ^fun (name, typeKind) ->
                AST.ident name, extractResponseSynType typeKind
        anonRecord fields

/// Creating AST XML docs from API representation
let xml: Docs option -> PreXmlDoc = function
    | None -> PreXmlDoc.Empty
    | Some docs ->
        xmlDocs [
            if docs.Summary.IsSome then
                XmlDoc.summary docs.Summary.Value
            if docs.Remarks.IsSome then
                XmlDoc.remarks docs.Remarks.Value
            if docs.Example.IsSome then
                XmlDoc.example docs.Example.Value
        ]

/// extract record definitions from
let extractRecords (schemas: TypeSchema list) =
    // store name and fields of records here
    let typesDict = Dictionary<string, SynFields * Docs option>()

    let rec extractSynType (name: string, kind: TypeKind) =
        match kind with
        | Prim primType -> primTypeMatch primType
        | Array innerType -> arrayOf (extractSynType (name, innerType))
        | Option innerType -> optionOf (extractSynType (name, innerType))
        | Object objectKind ->

            // extract field types
            let fields =
                objectKind.Properties
                |> List.map (fun (fieldName, fieldKind) ->
                    extractSynType (fieldName, fieldKind)
                    |> field fieldName)
            
            // add name and fields for later
            typesDict.Add(name, (fields, objectKind.Docs))

            // return SynType with record name
            synType name

    // iterate through schemas
    // records will be stored in dictionary as a side effect
    for schema in schemas do
        extractSynType (schema.Name, schema.Kind) |> ignore

    // create final record expressions
    typesDict
    |> Seq.map ^fun (KeyValue(name, (fields, docs))) ->
        let xmlDocs = xml docs
        record xmlDocs name fields
    |> Seq.toList


/// Creating whole module AST for Giraffe webapp
let giraffeAst (api: Api) =
    moduleDecl (xml api.Docs) api.Name
        [ openDecl "FSharp.Control.Tasks.V2.ContextInsensitive"
          openDecl "Giraffe"
          openDecl "System.Threading.Tasks"
          openDecl "Microsoft.AspNetCore.Http"

          let records = extractRecords api.Schemas
          if not records.IsEmpty then
              types records

          abstractClassDecl "Service"
              [ for path in api.Paths do
                  for method in path.Methods do
                      let responseTypes =
                          [ for response in method.Responses do
                                extractResponseSynType response.Kind ]
                      abstractHttpHandler (xml method.Docs) method.Name
                      match responseTypes with
                      | [] -> ()
                      | [responseType] -> 
                        abstractMemberDfn (method.Name + "Input") (synType "HttpContext" ^-> (taskOf [responseType]))
                        abstractMemberDfn (method.Name + "Output") (responseType ^-> synType "HttpHandler")
                      | responseTypes ->
                        abstractMemberDfn (method.Name + "Input") (synType "HttpContext" ^-> (taskOf [choiceOf responseTypes]))
                        abstractMemberDfn (method.Name + "Output") (choiceOf responseTypes ^-> synType "HttpHandler") ]
                        
          let routes = api.Paths |> List.map createRoute
          
          let routesExpr =
              if routes.Length > 1 then chooseExpr routes
              else routes.[0]

          letHttpHandlerDecl "webApp" ^ taskBuilder ^ letGetServiceDecl ^ returnBang ^ appNextCtx routesExpr ]

/// Creating source code string from provided AST using default Fantomas settings
let sourceCode ast =

    let dummyInput =
        ParsedImplFileInput.ParsedImplFileInput
            ("", false, QualifiedNameOfFile.QualifiedNameOfFile(Ident("", r)), [], [], [ ast ], (true, true))

    let dummyImplFile = ParsedInput.ImplFile(dummyInput)

    CodeFormatter.FormatASTAsync(dummyImplFile, "abc.fs", [], None, { Fantomas.FormatConfig.FormatConfig.Default with StrictMode = true })
    |> Async.RunSynchronously
