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
        let routeExpr = createRouteHandler path.Route (path.Methods.[0])
        // Without explicit parenthesizing two bad things happen:
        // 1. `>=>` ignores SynExpr.App isInfix flag and generates prefix application for some obscure reason
        // 2. application is ambiguous because the lack of parenthesis
        // e.g. generated code is
        // >=> POST bindRoute "route/{args}" service.Post next ctx
        // instead of
        // (POST >=> bindRoute "route/{args}" service.Post) next ctx
        // or at least
        // ((>=>) POST (bindRoute "route/{args}" service.Post) next ctx
        paren routeExpr

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
    | BuiltIn builtIn -> synType builtIn
    | Object { Name = Some name } -> synType name
    | Object anonObject ->
        let fields =
            anonObject.Properties
            |> List.map
            ^ fun (name, typeKind) -> AST.ident name, extractResponseSynType typeKind

        if fields.IsEmpty then objType else anonRecord fields
    | DU du -> synType du.Name
    | NoType -> unitType

/// Creating AST XML docs from API representation
let xml: Docs option -> PreXmlDoc =
    function
    | None -> PreXmlDoc.Empty
    | Some docs ->
        xmlDocs [ if docs.Summary.IsSome then XmlDoc.summary docs.Summary.Value
                  if docs.Remarks.IsSome then XmlDoc.remarks docs.Remarks.Value
                  if docs.Example.IsSome then XmlDoc.example docs.Example.Value ]

/// Gets own name of the type instead of the name set upwards
let getOwnName kind def =
    match kind with
    | DU du -> Some du.Name
    | Object o -> o.Name
    | _ -> None
    |> Option.defaultWith def

/// extract record definitions from
let extractRecords (schemas: TypeSchema list) =
    // store name and fields of records here
    let recordsDict =
        Dictionary<string, SynField list * Docs option>()
    // store name and cases of records here
    let duDict =
        Dictionary<string, (string * SynType * PreXmlDoc) list * Docs option>()
    
    let rec extractSynType (name: string, kind: TypeKind) =
        match kind with
        | Prim primType -> primTypeMatch primType
        | BuiltIn builtIn -> synType builtIn
        | Array innerType -> arrayOf (extractSynType (getOwnName innerType (fun () -> name), innerType))
        | Option innerType -> optionOf (extractSynType (getOwnName innerType (fun () -> name), innerType))
        | Object objectKind ->

            // extract field types
            let fields =
                objectKind.Properties
                |> List.map (fun (fieldName, fieldKind) ->
                    extractSynType (fieldName, fieldKind)
                    |> field fieldName)

            // add name and fields for later
            if not ^ recordsDict.ContainsKey name then
                recordsDict.Add(name, (fields, objectKind.Docs))

            // return SynType with record name
            synType name
        | DU du ->
            let cases =
                du.Cases
                |> List.mapi
                   (
                       fun idx case ->
                           let name = case.CaseName |> Option.defaultWith (fun _ -> sprintf "Case%d" (idx + 1))
                           let subtypeName = getOwnName case.Kind (fun _ -> name + "CaseValue")
                           name,
                           extractSynType(subtypeName, case.Kind),
                           xml case.Docs
                   )
            if cases.Length > 0 && not ^ duDict.ContainsKey du.Name then
                duDict.Add(du.Name, (cases, du.Docs))
            synType name
        | NoType -> failwith "Field without types are not supported for record schemas"

    // iterate through schemas
    // records will be stored in dictionary as a side effect
    for schema in schemas do
        extractSynType (schema.Name, schema.Kind)
        |> ignore
    
    // create final DU expressions
    let dus =    
        duDict
        |> Seq.map
        ^ fun (KeyValue(name, (cases, docs))) ->
            discriminatedUnion (xml docs) name cases

    // create final record expressions
    let records =
        recordsDict
        |> Seq.map
        ^ fun (KeyValue (name, (fields, docs))) ->
            let xmlDocs = xml docs
            record xmlDocs name fields
    
    dus
    |> Seq.append records
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
          
          // generate binding errors DU and stringifiers
          let errInnerTypeName = "ArgumentError"
          let errInnerGiraffeBinding = "GiraffeBindingError"
          let errInnerFormatterBindingExn = "FormatterBindingException"
          let errInnerModelBindingUnexpectedNull = "ModelBindingUnexpectedNull"
          let errInnerCombined = "CombinedArgumentErrors"
          let errInnerType =
              DU {
                  Name = errInnerTypeName
                  Docs = Some { Summary = Some ["Represents some concrete error somewhere in arguments"]; Example = None; Remarks = None }
                  Cases =
                      [
                          {
                              CaseName = Some errInnerGiraffeBinding
                              Docs = Some { Summary = Some ["Giraffe error returned in Result.Error of tryBindXXX method"]; Example = None; Remarks = None }
                              Kind = Prim <| PrimTypeKind.String StringFormat.String
                          }
                          {
                              CaseName = Some errInnerFormatterBindingExn
                              Docs = Some { Summary = Some [ "Represents exception occured during IFormatter bind" ]; Example = None; Remarks = None }
                              Kind = BuiltIn "exn"
                          }
                          {
                              CaseName = Some errInnerModelBindingUnexpectedNull
                              Docs = Some { Summary = Some ["For IFormatter bind (JSON, for example), represents a null that happens to exist because no value was provided for a property which is required"]; Example = None; Remarks = None }
                              Kind = TypeKind.Object
                                  {
                                      Name = None
                                      Properties =
                                          [
                                              "TypeName", Prim (PrimTypeKind.String StringFormat.String), None
                                              "PropertyPath", TypeKind.Array (Prim <| PrimTypeKind.String StringFormat.String), None
                                          ]
                                      Docs = None
                                  }
                          }
                          {
                              CaseName = Some errInnerCombined
                              Docs = Some { Summary = Some ["Represents multiple errors occured in one location"]; Example = None; Remarks = None }
                              Kind = TypeKind.Array (DU { Name = errInnerTypeName; Docs = None; Cases = [] })
                          }
                      ]
              }
          let errOuterTypeName = "ArgumentLocationedError"
          let errOuterBody = "BodyBindingError"
          let errOuterQuery = "QueryBindingError"
          let errOuterPath = "PathBindingError"
          let errOuterCombined = "CombinedArgumentLocationError"
          let errOuterType =
              DU {
                  Name = errOuterTypeName
                  Docs = Some { Summary = Some ["Represents error in arguments of some location"]; Example = None; Remarks = None; }
                  Cases =
                      [
                          {
                              CaseName = Some errOuterBody
                              Docs = Some { Summary = Some ["Represents error in body"]; Example = None; Remarks = None; }
                              Kind = errInnerType
                          }
                          {
                              CaseName = Some errOuterQuery
                              Docs = Some { Summary = Some ["Represents error in query"]; Example = None; Remarks = None; }
                              Kind = errInnerType
                          }
                          {
                              CaseName = Some errOuterPath
                              Docs = Some { Summary = Some ["Represents error in path"]; Example = None; Remarks = None; }
                              Kind = errInnerType
                          }
                          {
                              CaseName = Some errOuterCombined
                              Docs = Some { Summary = Some ["Represents errors in multiple locations"]; Example = None; Remarks = None; }
                              Kind = TypeKind.Array (DU { Name = errOuterTypeName; Docs = None; Cases = [] })
                          }
                      ]
              }
          [ errInnerTypeName, errInnerType; errOuterTypeName, errOuterType ]
          |> List.map (fun (name, kind) -> { DefaultValue = None; Name = name; Kind = kind; Docs = None })
          |> extractRecords
          |> types
          
          let innerErrToStringName = "argErrorToString"
          let levelParam = "level"
          let valueParam = "value"
          let sepVar = "sep"
          let err = "err"
          let nextLevel = app (appI (identExpr "op_Addition") (identExpr levelParam)) (SynConst.Int32 1 |> constExpr)
          let letSep =
              letOrUseDecl
                  sepVar
                  []
                  (
                      app
                          (longIdentExpr "System.String")
                          (
                              tupleComplexExpr
                                [
                                    SynConst.Char ' ' |> constExpr
                                    app (appI (identExpr "op_Multiply") (identExpr levelParam)) (SynConst.Int32 2 |> constExpr)
                                ]
                          )
                  )
          
          let innerErrToStringDecl =
              letDecl true innerErrToStringName [levelParam; valueParam] None
              ^ letSep
              ^ simpleValueMatching valueParam
                [
                    errInnerGiraffeBinding, err, sprintfExpr "%sGiraffe binding error: %s" [identExpr sepVar; identExpr err]
                    errInnerModelBindingUnexpectedNull, err,
                        sprintfExpr "%sUnexpected null was found at path %s.%s"
                        ^ [identExpr sepVar; longIdentExpr (sprintf "%s.TypeName" err); paren (app (app (longIdentExpr "String.concat") (strExpr ".")) (longIdentExpr (sprintf "%s.PropertyPath" err))) ]
                    errInnerFormatterBindingExn, err, longIdentExpr (sprintf "%s.Message" err)
                    errInnerCombined, err,
                        sprintfExpr "%sMultiple errors:\\n%s"
                        ^ [identExpr sepVar
                           app
                            (app (longIdentExpr "String.concat") (strExpr "\\n"))
                            (
                                app // Seq.map (recCall (level + 1))
                                    (app (longIdentExpr "Seq.map") (paren(app (identExpr innerErrToStringName) (paren(nextLevel)))))
                                    (identExpr err)
                                |> paren
                            )
                           |> paren
                        ]
                ]
          innerErrToStringDecl
          
          let callInnerWithFormat format var =
              sprintfExpr format [ identExpr sepVar; paren ( app (app (identExpr innerErrToStringName) (paren nextLevel)) (identExpr var)) ]
          
          let outerErrToStringName = "argLocationErrorToString"
          let outerErrToStringDecl =
              letDecl true outerErrToStringName [levelParam; valueParam] None
              ^ letSep
              ^ simpleValueMatching valueParam
                [
                    let common =
                        [
                            errOuterBody, "body"
                            errOuterPath, "path"
                            errOuterQuery, "query"
                        ]
                    for (case, var) in common do
                        let uppercase = System.String([| System.Char.ToUpperInvariant var.[0]; yield! var |> Seq.skip 1 |])
                        let format = sprintf "%%s%s binding error:\\n%%s" uppercase
                        case, var, (callInnerWithFormat format var)
                        
                    errOuterCombined, err,
                         sprintfExpr "%sMultiple binding errors:\\n%s"
                         ^ [identExpr sepVar
                            app
                             (app (longIdentExpr "String.concat") (strExpr "\\n\\n"))
                             (
                                 app // Seq.map (recCall (level + 1))
                                     (app (longIdentExpr "Seq.map") (paren(app (identExpr outerErrToStringName) (paren(nextLevel)))))
                                     (identExpr err)
                                 |> paren
                             )
                            |> paren
                         ]
                ]
          outerErrToStringDecl
          
          // generate helper functions for binding
          let checkForUnexpectedNullsName = "checkForUnexpectedNulls"
          let checkForUnexpectedNulls =
              let checkers = "checkers"
              let errType = "errType"
              let value = "value"
              let mapCheckers = "mapCheckers"
              let isNull = "isNull"
              letDecl false checkForUnexpectedNullsName [checkers; errType; value] None
              ^ letOrUseDecl isNull ["v"] (app (appI (identExpr "op_Equals") (identExpr "v")) (SynExpr.Null(r)))
                    (
                         letOrUseComplexParametersDecl mapCheckers (Pats [SynPat.Paren(tuplePat ["typeName"; "path"; "accessor"], r)])
                            (
                                 letOrUseComplexParametersDecl "v" (Pats [SynPat.Wild(r)])
                                    (
                                        app (identExpr errInnerModelBindingUnexpectedNull)
                                            (
                                                recordExpr
                                                    [
                                                        "TypeName", "typeName"
                                                        "PropertyPath", "path"
                                                    ]
                                            )
                                    )
                                    (
                                        app (identExpr "accessor") (identExpr value)
                                        ^|> app (longIdentExpr "Option.map") (identExpr isNull)
                                        ^|> app (longIdentExpr "Option.filter") (identExpr "id")
                                        ^|> app (longIdentExpr "Option.map") (identExpr "v")
                                    )
                            )
                            (
                                identExpr checkers
                                ^|> app (longIdentExpr "Seq.map") (identExpr mapCheckers)
                                ^|> app (longIdentExpr "Seq.choose") (identExpr "id")
                                ^|> longIdentExpr "Seq.toArray"
                                ^|> lambda (simplePats[simplePat "v"])
                                    (
                                        ifElseExpr
                                            (app (appI (identExpr "op_GreaterThan") (longIdentExpr "v.Length")) (constExpr (SynConst.Int32 1)))
                                            (identExpr "v" ^|> identExpr errInnerCombined)
                                            (identExpr "v" ^|> longIdentExpr "Array.head")
                                    )
                                ^|> identExpr errType
                            )
                    )
          checkForUnexpectedNulls

          let tryExtractErrorName = "tryExtractError"
          let tryExtractError =
              let value = "value"
              letDecl false tryExtractErrorName [value] None
              ^ simpleValueMatching value
                    [
                        "Ok", "whatever", identExpr "None"
                        "Error", "err", app (identExpr "Some") (identExpr "err") 
                    ]
          tryExtractError

          let parametersLocationNameMapping =
              seq {
                  for path in api.Paths do
                      for method in path.Methods do
                          if method.Parameters.IsSome then
                              let locationNameMapping =
                                method.Parameters.Value
                                |> Map.toSeq
                                |> Seq.filter (fst >> (function | Body _ -> false | _ -> true))
                                |> Seq.collect
                                     (
                                         fun (key, value) ->
                                             let nameNType =
                                                 match value.Kind with
                                                 | TypeKind.Object obj -> obj.Properties |> Seq.map (fun (nm,_,_) -> nm)
                                                 | _ -> seq { value.Name }
                                             nameNType |> Seq.map (fun t -> t, key)
                                     )
                                 |> Seq.groupBy fst
                                 |> Seq.map (fun (name, values) -> name, values |> Seq.map (fun (_, v) -> v) |> Seq.toArray)
                                 |> Seq.collect
                                     (
                                         fun (name, locations) ->
                                             if locations.Length = 1 then
                                                 seq { locations.[0], (name, name) }
                                             else
                                                 let unnameableCount =
                                                     locations
                                                     |> Seq.countBy id
                                                     |> Seq.map snd
                                                     |> Seq.filter ((<>) 1)
                                                     |> Seq.length
                                                 if unnameableCount > 0 then
                                                     let err = sprintf "Unable to generate distinct input property name: property \"%s\" is duplicated by location and type" name
                                                     failwith err
                                                 locations |> Seq.map (fun loc -> loc, (name, name + "From" +  (loc.ToString())))
                                     )
                                 |> Seq.groupBy fst
                                 |> Seq.map (fun (l, v) -> l, v |> Seq.map snd |> Map)
                                 |> Map
                              (method.Method, method.Name), locationNameMapping
                          
              } |> Map

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
                             // <summary>{method.Docs}</summary>
                             // abstract {method.Name}: {param} -> HttpHandler  
                             abstractMemberDfn (xml method.Docs) method.Name (param ^-> synType "HttpHandler")
                      |> Option.defaultWith ^ fun _ ->
                             // <summary>{method.Docs}</summary>
                             // abstract {method.Name}: HttpHandler
                             abstractGetterDfn (xml method.Docs) method.Name (synType "HttpHandler")

                      if not responseTypes.IsEmpty then
                          // Choice<{responseTypes[0]}, {responseTypes[1]}, ...>
                          // Or {responseType}
                          let returnType =
                              if responseTypes.Length > 1
                              then choiceOf responseTypes
                              else responseTypes.Head

                          let fullReturnType =
                              maybeParams
                              // ({param} * HttpContext)
                              |> Option.map (fun param -> tuple [ param; synType "HttpContext" ])
                              // HttpContext
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
                                    
                          let a = 1
                          
                          // abstract {method.Name}Input: {fullReturnType} -> Task<{returnType}>
                          abstractMemberDfn xmlEmpty (method.Name + "Input") (fullReturnType ^-> taskOf [ returnType ])
                          // abstract {method.Name}Output: {returnType} -> HttpHandler
                          abstractMemberDfn xmlEmpty (method.Name + "Output") (returnType ^-> synType "HttpHandler")
                          
                          // giraffe handler for particular response
                          let response2handler (response: Response) inputBinding =
                                let contentHandler =
                                    // currently everything goes to json
                                    match response.MediaType with
                                    | Json
                                    | NotSpecified -> app (identExpr "json" ) (identExpr inputBinding)
                                    | Other x -> failwithf "Emitting of code for media type %s currently not supported" x
                                
                                if response.Code = 200
                                then
                                    contentHandler
                                else
                                    setStatusCodeExpr response.Code >=> contentHandler
                          
                          // match generator for different responses
                          let methodResponseToClause (responses: Response list): SynMatchClause list =
                              let patName n = sprintf "Choice%dOf%d" n responses.Length
                              responses
                              |> List.mapi ^fun i response ->
                                  let i = i + 1
                                  let patBinding = sprintf "responseOn%d" response.Code
                                  synLongPat (patName i) patBinding ^=> response2handler response patBinding
                          
                          // body for default implementation of Output method
                          let body = 
                              if method.Responses.Length > 1 then
                                matchExpr "input" ^methodResponseToClause method.Responses
                              else
                                response2handler method.Responses.Head "input"
                          
                          // override this.{method.Name}Output input =
                          //    match input with
                          //    | Choice1Of2 array -> json array
                          //    | Choice2Of2 () -> setStatusCode 404 }
                          methodImplDefn (method.Name + "Output") ["input"] body ]

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
