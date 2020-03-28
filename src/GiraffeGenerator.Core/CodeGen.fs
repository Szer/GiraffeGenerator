module CodeGen

open System.Collections.Generic
open AST
open OpenApi
open Fantomas
open FSharp.Compiler.Ast

let inline (^) f x = f x

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
    | Int -> intType
    | Long -> int64Type
    | Double -> doubleType
    | Bool -> boolType
    | String strFormat -> strFormatMatch strFormat

/// extract record definitions from
let extractRecords (schemas: TypeDefinition list) =
    // store namd and fields of records here
    let typesDict = Dictionary<string, SynFields>()
    
    let rec extractSynType (schema: TypeDefinition) =
        let synType =
            match schema.Kind with
            | Prim primType -> primTypeMatch primType
                
            | Array innerType -> arrayOf(extractSynType innerType)

            | Object fieldDefinitions ->
                
                // extract field types
                let fields =
                    fieldDefinitions
                    |> List.map ^fun typeDef ->
                        extractSynType typeDef
                        |> field typeDef.Name
                
                // add name and fields for later
                typesDict.Add(schema.Name, fields)
                
                // return SynType with record name
                synType schema.Name
                
        if schema.Nullable then
            optionOf synType
        else
            synType
            
    // iterate through schemas
    // records will be stored in dictionary as a side effect
    schemas
    |> List.iter (extractSynType >> ignore)
    
    // create final record expressions
    typesDict
    |> Seq.map (fun (KeyValue (name, fields)) ->
        record name fields)
    |> Seq.toList
            

/// Creating whole module AST for Giraffe webapp
let giraffeAst (api: Api) =
    moduleDecl api.Name
        [ openDecl "FSharp.Control.Tasks.V2.ContextInsensitive"
          openDecl "Giraffe"

          types (extractRecords api.Schemas)    
              
          typeObjectDecl "Service"
              [ for path in api.Paths do
                  for method in path.Methods do
                      yield abstractHttpHandler method.Name ]

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

    CodeFormatter.FormatASTAsync(dummyImplFile, "abc.fs", [], None, Fantomas.FormatConfig.FormatConfig.Default)
    |> Async.RunSynchronously
