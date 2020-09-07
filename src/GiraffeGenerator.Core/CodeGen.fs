module CodeGen

open System.Collections.Generic
open AST
open OpenApi
open Fantomas
open FSharp.Compiler.XmlDoc
open FSharp.Compiler.SyntaxTree

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
    method.Method.ToUpperInvariant()
    |> identExpr
    >=> service method.Name

let private createRouteHandler path method =
    let verb =
        method.Method.ToUpperInvariant() |> identExpr

    let serviceCall = service method.Name
    if method.Parameters.IsSome then verb >=> routeBind path serviceCall else verb >=> route path >=> serviceCall

/// helper function to create Giraffe flows:
/// {method.Method} >=> route {path.Route} >=> service.{method.Name}
/// or
/// {method.Method} >=> routeBind {path.Route} service.{method1.Name}
/// or
/// choose [
///     {method1.Method} >=> route {path.Route} >=> service.{method.Name}
///     {method2.Method} >=> routeBind {path.Route} service.{method1.Name}
/// ]
let createRoute (path: ParsedPath) =
    if path.Methods.Length > 1 then
        chooseExpr
            (path.Methods
             |> List.map (createRouteHandler path.Route))
    else
        createRouteHandler path.Route (path.Methods.[0])

/// matching OpenAPI string IR to SynType
let strFormatMatch =
    function
    | StringFormat.String
    | PasswordString -> stringType
    | Byte -> arrayOf byteType
    | Binary -> arrayOf byteType
    | DateString
    | DateTimeString -> dateType
    | Custom "uri"
    | Custom "uriref" -> uriType
    | Custom "uuid"
    | Custom "guid"
    | Custom "uid" -> guidType
    | Custom name -> synType name

/// matching OpenAPI primitive type IR to SynType
let primTypeMatch =
    function
    | Any -> objType
    | Int -> intType
    | Long -> int64Type
    | Double -> doubleType
    | Bool -> boolType
    | String strFormat -> strFormatMatch strFormat

/// matching type kinds in responses to create their syntatic types
let rec extractResponseSynType =
    function
    | Prim primType -> primTypeMatch primType
    | Array innerType -> arrayOf (extractResponseSynType innerType)
    | Option innerType -> optionOf (extractResponseSynType innerType)
    | Object { Name = Some name } -> synType name
    | Object anonObject ->
        let fields =
            anonObject.Properties
            |> List.map
            ^ fun (name, typeKind) -> AST.ident name, extractResponseSynType typeKind

        if fields.IsEmpty then objType else anonRecord fields
    | NoType -> unitType

/// Creating AST XML docs from API representation
let xml: Docs option -> PreXmlDoc =
    function
    | None -> PreXmlDoc.Empty
    | Some docs ->
        xmlDocs [ if docs.Summary.IsSome then XmlDoc.summary docs.Summary.Value
                  if docs.Remarks.IsSome then XmlDoc.remarks docs.Remarks.Value
                  if docs.Example.IsSome then XmlDoc.example docs.Example.Value ]

/// extract record definitions from
let extractRecords (schemas: TypeSchema list) =
    // store name and fields of records here
    let typesDict =
        Dictionary<string, SynField list * Docs option>()

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
        | NoType -> failwith "Field without types are not supported for record schemas"

    // iterate through schemas
    // records will be stored in dictionary as a side effect
    for schema in schemas do
        extractSynType (schema.Name, schema.Kind)
        |> ignore

    // create final record expressions
    typesDict
    |> Seq.map
    ^ fun (KeyValue (name, (fields, docs))) ->
        let xmlDocs = xml docs
        record xmlDocs name fields
    |> Seq.toList


/// Creating whole module AST for Giraffe webapp
let giraffeAst (api: Api) =
    moduleDecl
        (xml api.Docs)
        api.Name
        [ openDecl "FSharp.Control.Tasks.V2.ContextInsensitive"
          openDecl "Giraffe"
          openDecl "System.Threading.Tasks"
          openDecl "Microsoft.AspNetCore.Http"

          let allSchemas =
              [ for path in api.Paths do
                  for method in path.Methods do
                      if method.Parameters.IsSome then method.Parameters.Value
                yield! api.Schemas ]

          if not allSchemas.IsEmpty then types (extractRecords allSchemas)

          abstractClassDecl
              "Service"
              [ for path in api.Paths do
                  for method in path.Methods do
                      let responseTypes =
                          [ for response in method.Responses do
                              extractResponseSynType response.Kind ]

                      let maybeParams =
                          method.Parameters
                          |> Option.map (fun param -> extractResponseSynType param.Kind)















                      
                      // emitting httpHandler abstract method or property
                      maybeParams
                      |> Option.map ^ fun param ->
                             // abstract {method.Name}: {method.Parameters} -> HttpHandler  
                             abstractMemberDfn (xml method.Docs) method.Name (param ^-> synType "HttpHandler")
                      |> Option.defaultWith ^ fun _ ->
                             // abstract {method.Name}: HttpHandler
                             abstractGetterDfn (xml method.Docs) method.Name (synType "HttpHandler")
                             


                      if not responseTypes.IsEmpty then
                          let returnType =
                              if responseTypes.Length > 1 then choiceOf responseTypes else responseTypes.Head

                          let fullReturnType =
                              maybeParams
                              |> Option.map (fun param -> tuple [ param; synType "HttpContext" ])
                              |> Option.defaultValue (synType "HttpContext")
                              
                          // helper for emitting:                                                                             
                          // override {implDefn} = fun next ctx ->task {                                                      
                          //     let! input = this.{method.Name}Input {argExpr}                                               
                          //     return! this.{method.Name}Output input next ctx                                              
                          // }                                                                                                
                          let defaultImplementationEmitter implDefn argExpr =                                                 
                                implDefn                                                                                      
                                ^ lambdaFunNextCtxExpr                                                                        
                                ^ taskBuilder                                                                                 
                                ^ letBangExpr                                                                                 
                                      "input"                                                                                 
                                      (app (longIdentExpr ^ sprintf "this.%sInput" method.Name) argExpr)                      
                                      (returnBang                                                                             
                                       ^ curriedCall (sprintf "this.%sOutput" method.Name) [ "input"; "next"; "ctx" ])            
                                  
                          // emitting httpHandler default implementation method or property        
                          maybeParams
                          |> Option.map ^ fun _ ->
                                // override this.{method.Name} args = fun next ctx ->task {
                                //     let! input = this.{method.Name}Input(args, ctx)
                                //     return! this.{method.Name}Output input next ctx
                                // }
                                defaultImplementationEmitter
                                    (methodImplDefn method.Name ["args"])
                                    (tupleExpr ["args"; "ctx"])
                          |> Option.defaultWith ^ fun _ ->
                                // override this.{method.Name} = fun next ctx ->task {
                                //     let! input = this.{method.Name}Input ctx
                                //     return! this.{method.Name}Output input next ctx
                                // }
                                defaultImplementationEmitter
                                    (methodImplDefn method.Name [])
                                    (identExpr "ctx")
                          
                          abstractMemberDfn xmlEmpty (method.Name + "Input") (fullReturnType ^-> taskOf [ returnType ])
                          abstractMemberDfn xmlEmpty (method.Name + "Output") (returnType ^-> synType "HttpHandler") ]

          let routes = api.Paths |> List.map createRoute

          let routesExpr =
              if routes.Length > 1 then chooseExpr routes else routes.[0]

          letHttpHandlerDecl "webApp"
          ^ taskBuilder
          ^ letGetServiceDecl
          ^ returnBang
          ^ appNextCtx routesExpr ]

/// Creating source code string from provided AST using default Fantomas settings
let sourceCode ast =

    let dummyInput =
        ParsedImplFileInput.ParsedImplFileInput
            ("", false, QualifiedNameOfFile.QualifiedNameOfFile(Ident("", r)), [], [], [ ast ], (true, true))

    let dummyImplFile = ParsedInput.ImplFile(dummyInput)

    CodeFormatter.FormatASTAsync
        (dummyImplFile,
         "abc.fs",
         [],
         None,
         { Fantomas.FormatConfig.FormatConfig.Default with
               StrictMode = true })
    |> Async.RunSynchronously
