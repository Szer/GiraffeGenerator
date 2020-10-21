module CodeGen

open System.Globalization
open AST
open ASTExt
open OpenApi
open OpenApiToAstTypesMatchingAndConversion
open Fantomas
open FSharp.Compiler.SyntaxTree

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
    let hasPathParameters =
        method.Parameters
        |> Option.map (Map.containsKey Path)
        |> Option.defaultValue false
    if hasPathParameters then verb >=> routeBind path serviceCall else verb >=> route path >=> serviceCall

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

let requestCommonInputTypeName (method: PathMethodCall) = method.Name + method.Method + "Input"
let sourceSorting (source: PayloadLocation) = source = Path, source = Query, source = Body(Json), source = Body(Form)
let isBody = function | Body _ -> true | _ -> false
let isNotBody = isBody >> not
let isNotPath = (<>) Path

let extractParameterObjectNamesWithLocations (parameterLocation, (parameterSchema: TypeSchema)) =
    let names =
        match parameterSchema.Kind with
        | TypeKind.Object obj -> obj.Properties |> Seq.map (fun (nm,_,_) -> nm)
        | _ -> seq { parameterSchema.Name }
    names |> Seq.map (fun name -> name, parameterLocation)

let getUniqueParameterNameForLocationOrFail ((parameterName, parameterLocations): string * (PayloadLocation[])) =
    if parameterLocations.Length = 1 then
        seq { parameterName, (parameterLocations.[0], parameterName) }
    else
        let hasUnnameable =
            parameterLocations.Length <> (Set parameterLocations).Count
        if hasUnnameable then
            failwithf "Unable to generate distinct input property name: property \"%s\" is duplicated by location" parameterName
        parameterLocations |> Seq.map (fun loc -> parameterName + "From" + (loc.ToString()), (loc, parameterName))

type CombinedRecordPropertyNamesMapping =
    { CombinedRecordPropertyName: string
      OriginalLocation: PayloadLocation
      OriginalName: string }
let getCombinedRecordPropertyNamesFrom parameters =
    parameters
    |> Map.toSeq
    |> Seq.filter (fst >> isNotBody)
    |> Seq.collect extractParameterObjectNamesWithLocations
    |> Seq.groupBy fst
    |> Seq.map (fun (parameterName, group) -> parameterName, group |> Seq.map (fun (_, parameterLocation) -> parameterLocation) |> Seq.toArray)
    |> Seq.collect getUniqueParameterNameForLocationOrFail
    |> Seq.groupBy fst
    // implicit failwith just in case getUniqueParameterNameForLocation fail to do it's job
    |> Seq.map (fun (combinedTypePropertyName, v) -> combinedTypePropertyName, v |> Seq.map snd |> Seq.exactlyOne)
    |> Seq.map (fun (combinedPropertyName, (originalLocation, originalName)) ->
        {
            CombinedRecordPropertyName = combinedPropertyName
            OriginalLocation = originalLocation
            OriginalName = originalName
        })
let generateCombinedRecordTypeDefnFor parameters name =
    let mapping =
        getCombinedRecordPropertyNamesFrom parameters
        |> Seq.map ^ fun mapping -> mapping.OriginalLocation, (mapping.OriginalName, mapping.CombinedRecordPropertyName)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> k, v |> Seq.map snd |> Map)
        |> Map
    { // generate combined input record from every source of input 
        Name = name
        DefaultValue = None
        Docs = None
        Kind =
            {
                Name = Some name
                Docs = None
                Properties =
                    parameters
                    |> Map.toSeq
                    |> Seq.filter (fst >> isNotBody)
                    |> Seq.collect
                        (
                            fun (location, schema) ->
                                let mapping = mapping |> Map.find location
                                match schema.Kind with
                                | Object o ->
                                    [for name, kind, def in o.Properties do
                                        mapping |> Map.find name, kind, def]
                                | _ -> [mapping |> Map.find (getOwnName schema.Kind ^ fun () -> schema.Name), schema.Kind, schema.DefaultValue]
                        )
                    |> Seq.toList
            } |> TypeKind.Object
    }
    
let hasMultipleNonBodyParameters parameters =
    parameters
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.filter isNotBody
    |> Seq.length > 1

let hasNullableValuesPossible api =
    seq {
        for path in api.Paths do
            for method in path.Methods do
                if method.Parameters.IsSome then
                    for KeyValue(loc, schema) in method.Parameters.Value do
                        if isNotPath loc then
                            CodeGenNullChecking.schemaHasNullableValues schema
    } |> Seq.contains true

let hasErrorsPossible api =
    seq {
        for path in api.Paths do
            for method in path.Methods do
                if method.Parameters.IsSome then
                    method.Parameters.Value
                    |> Map.toSeq
                    |> Seq.exists (fst >> isNotPath)
    } |> Seq.contains true


/// generates arbitrary number of nested Result.bind applications
/// given
///   bound = "bound"
///   leftToBind = ["a";"b";"c"]
///   generateFinal = fun () -> tupleExpr ["a";"b";"c"]
/// it would generate following code:
/// bound
/// |> Result.bind (fun a ->
///     a |> Result.bind (fun b ->
///            b |> Result.map (fun c -> a,b,c)))
let rec generateBinds bound leftToBind generateFinal =
    if List.length leftToBind > 0 then
        identExpr bound
        ^|> Result.bindExpr
            ^ lambda ([simplePat bound] |> simplePats)
                ^ generateBinds leftToBind.Head leftToBind.Tail generateFinal
    else
        identExpr bound
            ^|> Result.mapExpr
                ^ lambda ([simplePat bound] |> simplePats) ^ generateFinal()

/// generates mapping from all non-body parameters to combined record
/// (combined records are described in module generation)
/// see also: generateBinds
let generateInputsCombination combinedRecordPropertyToOriginalValue synt (schema: TypeSchema) bindings =
    let finalGenerator () =
        letExprComplex
            (SynPat.Typed(SynPat.Named(SynPat.Wild r, ident "v", false, None, r), synt, r))
            (
                let bindings = Map bindings
                match schema.Kind with
                | TypeKind.Object o ->
                    [
                        for (name, _, _) in o.Properties do
                            let (sourceLocation, sourceName) = combinedRecordPropertyToOriginalValue |> Map.find name
                            let sourceBinding = bindings |> Map.find sourceLocation
                            name, longIdentExpr (sourceBinding + "." + sourceName)
                    ]
                | _ -> failwith "combined record should be a record, you know"
                |> recordExpr
            )
            (identExpr "v")
    let onlyNames = bindings |> List.map snd
    generateBinds onlyNames.Head onlyNames.Tail finalGenerator

/// Creating whole module AST for Giraffe webapp
let giraffeAst (api: Api) =
    moduleDecl
        (xml api.Docs)
        api.Name
        [ openDecl "FSharp.Control.Tasks.V2.ContextInsensitive"
          openDecl "Giraffe"
          openDecl "System.Threading.Tasks"
          openDecl "Microsoft.AspNetCore.Http"
          
          let temporarySchemasForBindingBeforeDefaultsAppliance =
              [ for path in api.Paths do
                  for method in path.Methods do
                      if method.Parameters.IsSome then
                        for KeyValue(source, schema) in method.Parameters.Value do
                            if source = Query || isBody source then // non-query and non-body bindings don't support default values
                                let generatedTypes = generateOptionalType schema.Kind schema.DefaultValue (Some schema.Name)
                                yield! generatedTypes ]
          let temporarySchemasForBindingBeforeDefaultsApplianceMap =
              temporarySchemasForBindingBeforeDefaultsAppliance
              |> Seq.collect (fun v -> [ SourceType v.OriginalName, v; GeneratedType v.GeneratedName, v ])
              |> Map

          let nullsPossible = hasNullableValuesPossible api
          let errorsPossible = nullsPossible || hasErrorsPossible api
          
          let allSchemas =
              [
                if errorsPossible then
                    yield! CodeGenErrorsDU.typeSchemas
                yield! api.Schemas
                yield! temporarySchemasForBindingBeforeDefaultsAppliance |> Seq.map ^ fun v -> { Kind = v.Generated; Name = v.GeneratedName; Docs = None; DefaultValue = None }
                for path in api.Paths do
                  for method in path.Methods do
                      if method.Parameters.IsSome then
                        for KeyValue(_, schema) in method.Parameters.Value do
                            schema
                        if hasMultipleNonBodyParameters method.Parameters.Value then
                            let name = requestCommonInputTypeName method
                            generateCombinedRecordTypeDefnFor method.Parameters.Value name]

          if not allSchemas.IsEmpty then
              yield! [
                 types (extractRecords allSchemas)

                 // generate helper functions for error handling
                 if errorsPossible then
                    yield! CodeGenErrorsDU.generateHelperFunctions()

                 // generate helper functions for null checking
                 if nullsPossible then
                    yield! CodeGenNullChecking.generateNullCheckingHelpers()
              ]

          abstractClassDecl
              "Service"
              [ for path in api.Paths do
                  for method in path.Methods do
                      let responseTypes =
                          [ for response in method.Responses do
                              extractResponseSynType None response.Kind ]
                      
                      // Codegened service is generated to be included into giraffe pipeline like this:
                      // `giraffe >=> service.OpenAPIEndpoint`
                      // where OpenAPIEndpoint is implemented like `service.GetInputFromRequest >> service.ProcessInput >> service.GetResponseFromOutput`
                      // If there may be errors in request processing (almost every case with parametrized requests)
                      // then service.ProcessInput is replaced in pipeline with
                      // function | Ok args -> service.ProcessInput args | Error e -> service.ProcessError e
                      // For OpenAPIEndpoint there are 2 possible signatures:
                      // A. there are path parameters: pathParametersBoundObject -> HttpHandler
                      // B. HttpHandler
                      //
                      // GetInputFromRequest is currently inlined into OpenAPIEndpoint
                      // (it seems that there's no sense in allowing to override it 'cause user could customize it's behavior by customizing giraffe serialization/binding)
                      // ProccessInput has multiple possible signatures:
                      // 1. No parameters at all: HttpContext -> Task<'Output>
                      // 2. Single source of parameters: ParametersBoundFromSource * HttpContext -> Task<'Output> 
                      // 3. Multiple sources of parameters none of which is body: CombinedParametersRecord * HttpContext -> Task<'Output>
                      // 4. Exactly two sources of parameters one of which is body: ParametersBoundFromOtherSource * ParametersBoundFromBody * HttpContext -> Task<'Output>
                      // 5. Three or more sources of parameters one of which is body: CombinedParametersRecord * ParametersBoundFromBody * HttpContext -> Task<'Output>
                      // 'Output here is method output which is not considered important in this comment
                      // ParametersBoundFrom(Other)Source here is a record generated for all parameters from source specified
                      // e.g. /path/{outerId}/somethingInner/{innerId} would generate a record like
                      // type ParametersFromPath =
                      //   { outerId: int
                      //     innerId: int }
                      // CombinedParametersRecord is generated for any case when there are more than one non-body source of parameters
                      // Considering the example above, if there are type ParametersFromQuery = { innerId: Guid }
                      // CombinedParametersRecord would be generated like
                      // type CombinedParametersRecord =
                      //   { outerId: int
                      //     innerIdFromPath: int
                      //     innerIdFromQuery: Guid }

                      // cases 3. and 5.:
                      // get Option<TypeSchema> for CombinedParametersRecord. Option.isNone = true if there should be no combined record
                      let combinedTypeName = requestCommonInputTypeName method
                      let maybeCombinedTypeOpenApi =
                          allSchemas |> List.tryFind (fun x -> x.Name = combinedTypeName)  
                      let maybeCombinedType =
                          maybeCombinedTypeOpenApi
                          |> Option.map (fun tp -> tp.Kind |> extractResponseSynType (Some tp.Name))
                          
                      // case 2.:
                      // get Option<TypeSchema> for the only parameters source
                      let maybeSingleNonBodyParameterOpenApi =
                          method.Parameters
                          |> Option.bind (fun p -> p |> Map.toSeq |> Seq.filter (fst >> isNotBody) |> Seq.tryExactlyOne)
                      let maybeSingleNonBodyParameter =
                          maybeSingleNonBodyParameterOpenApi
                          |> Option.map snd
                          |> Option.map (fun x -> x.Kind |> extractResponseSynType (Some x.Name))
                          
                      // cases 4. and 5. - get Option<TypeSchema> for body parameters record
                      // TODO: Body binding into DU case per content-type instead of record for some of content-types
                      let maybeBodyOpenApi =
                          method.Parameters
                          |> Option.bind (fun p -> p |> Map.toSeq |> Seq.filter (fst >> isBody) |> Seq.tryExactlyOne)
                      let maybeBody =
                          maybeBodyOpenApi
                          |> Option.map snd
                          |> Option.map (fun x -> x.Kind |> extractResponseSynType (Some x.Name))
                      
                      // case A. - get Option<TypeSchema> for path parameters record
                      let maybePathOpenApi =
                          method.Parameters
                          |> Option.bind (Map.tryFind Path)
                      let maybePath = maybePathOpenApi |> Option.map (fun x -> x.Kind |> extractResponseSynType (Some x.Name))
                          
                      // cases 2. - 5. - get non-body parameters schema option
                      let maybeNonBody = maybeCombinedType |> Option.orElse maybeSingleNonBodyParameter
                          
                      // cases 1. - 5. - get all parameters SynType for ProcessInput
                      let maybeParams =
                          maybeBody
                          |> Option.map2 (fun a b -> [a; b]) maybeNonBody
                          |> Option.map tuple // cases 4. - 5. - both body and non-body
                          |> Option.orElse maybeBody // case 2. - single source of parameters
                          |> Option.orElse maybeNonBody // case 2. - single source of parameters
                          // case 1 if Option.isNone - no parameters

                      // emitting OpenAPIEndpoint - httpHandler abstract method or property
                      maybePath
                      |> Option.map ^ fun pathParam ->
                             // <summary>{method.Docs}</summary>
                             // abstract {method.Name}: {pathParam} -> HttpHandler  
                             abstractMemberDfn (xml method.Docs) method.Name (pathParam ^-> synType "HttpHandler")
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
                              
                          let respondsUnit = method.Responses |> List.map (fun x -> x.Kind) = [NoType]
                          
                          let fullInputReturnType =
                              maybeParams
                              // ({param} * HttpContext)
                              |> Option.map (fun param -> tuple [ param; synType "HttpContext" ])
                              // HttpContext
                              |> Option.defaultValue (synType "HttpContext")
                              
                          // helper for emitting OpenAPIEndpoint:                                                                             
                          // override {implDefn} = fun next ctx ->task {                                                      
                          //     (*optional parameters binding*)
                          //     let! input = this.{method.Name}Input {argExpr}
                          //     return! this.{method.Name}Output input next ctx                                              
                          // }
                          // parameters binding is the most verbose part
                          // every parameter type (InputFromXXX) may have InputFromXXXForBinding counterpart
                          // if it has OpenAPI default value specified on any level of nesting.
                          // InputFromXXXForBinding is generated with Option<_> for properties and values with defaults.
                          // Query binding is generated like
                          //
                          // let queryArgs =
                          //   httpContext.TryBindQueryString<InputFromQueryForBinding>(CultureInfo.InvariantCulture)
                          //   |> Result.mapError (GiraffeBindingError >> QueryBindingError)
                          //
                          // Where GiraffeBindingError and QueryBindingError are cases of DU generated by CodeGenErrorsDU.typeSchemas
                          // If there is ForBinding type for handled parameter source, code like the following is added:
                          //   |> Result.map (fun (bound: InputFromQueryStringForBinding) ->
                          //       let output: InputFromQueryString =
                          //         { objectWithNoDefaultOnAnyNesting = bound.objectWithNoDefaultOnAnyNesting
                          //           objectWithDefaultPrimitiveProperty =
                          //             { propertyWithDefault =
                          //                 bound.objectWithDefaultPrimitiveProperty.propertyWithDefault
                          //                 |> Option.defaultValue ("this is default") }
                          //           primitiveWithDefault =
                          //             bound.primitiveWithDefault
                          //             |> Option.defaultValue (42)
                          //           array2dWithDefaultItemValue =
                          //             bound.array2dWithDefaultItemValue
                          //             |> Array.map (fun (src: int option array) ->
                          //                  src |> Array.map(fun (src: int option) ->
                          //                            src |> Option.defaultValue (42)
                          //                          )
                          //                )
                          //           optionalObjectWithDefaultPrimitiveProperty =
                          //             bound.optionalObjectWithDefaultPrimitiveProperty
                          //               |> Option.map (fun (src: optionalObjectWithDefaultPrimitivePropertyForBinding) ->
                          //                     let output: optionalObjectWithDefaultPrimitiveProperty =
                          //                       { optionalObjectWithDefaultPrimitiveProperty =
                          //                           src.optionalObjectWithDefaultPrimitiveProperty
                          //                           |> option.defaultValue (new DateTime(123243241231123L)) }
                          //                     output
                          //                   )
                          //         }
                          //       output
                          // Mapper function may be referenced as "defaulter" and is generated by DefaultsGeneration.generateDefaultMappingFunFromSchema
                          // If there are possible nulls in binding (e.g. input type contains or is itself a reference type), code like the following one is added:
                          //  |> Result.bind
                          //    (checkForUnexpectedNulls
                          //      [| ("InputFromQueryString",
                          //           [| "objectWithDefaultPrimitiveProperty"; "propertyWithDefault" |],
                          //           fun (query: InputFromQuery) ->
                          //             Seq.replicate 1 query
                          //             |> Seq.map (fun v -> v.objectWithDefaultPrimitiveProperty)
                          //             |> Seq.map (fun v -> v.propertyWithDefault)
                          //             |> Seq.map isNullReference)
                          //        ("InputFromQueryString",
                          //           [| "array2dWithDefaultItemValue"; "[n]" |],
                          //           fun (query: InputFromQuery) ->
                          //             Seq.replicate 1 query
                          //             |> Seq.collect (fun v -> v.array2dWithDefaultItemValue)
                          //             |> Seq.map isNullReference) 
                          //        ("InputFromQueryString",
                          //           [| "optionalObjectWithDefaultPrimitiveProperty"; ".Value" |],
                          //           fun (query: InputFromQuery) ->
                          //             Seq.replicate 1 query
                          //             |> Seq.choose (fun v -> v.array2dWithDefaultItemValue)
                          //             |> Seq.map isNullReference) |] 
                          //      QueryBindingError)
                          // This would raise an exception if any item in property path is null
                          // (should be handled by `checkForUnexpectedNulls` and wrapped into `QueryBindingError(ModelBindingUnexpectedNull)`)
                          // And would produce the same error if `isNullReference` returns true
                          // Binder function may be referenced as "nullChecker"
                          // This whole null-checking thing is here to ensure that no unexpected null would be passed to user code
                          // because there should be no nulls if passed message conforms to spec by which the server is generated
                          // (all forms of optionality are generated as Option<_>)
                          // Null-checking helper functions are generated by CodeGenNullChecking.generateNullCheckingHelpers()
                          // The `Result.bind nullChecker` itself is generated by CodeGenNullChecking.bindNullCheckingIntoResult
                          //
                          // The above example considered binding from the query.
                          // While defaulter and nullChecker sections are common to any source of binding, the binding itself differs by source
                          // Path binding occurs during routeBind "route/with/{param}" >=> service.OpenAPIEndpoint
                          // Therefore it's accepted as parameter by OpenAPIEndpoint
                          // But every binding is considered as Result<_, ArgumentLocationedError> and null-checking is used on path inputs too
                          // Side note: this consideration is made for validation which is still to be added 
                          // (no defaulter because OpenAPI spec forces path parameters to be required which leads to no meaning for defaults)
                          // So following code is generated:
                          // let pathArgs =
                          //   Result<PathInput, ArgumentLocationedError>.Ok pathArgs
                          //   |> Result.bind nullChecker
                          //
                          // Body binding is slightly different because it's binding uses ASP.NET IFormatter under the hood
                          // So httpContext.bindJsonAsync/bindFormAsync/bindXmlAsync have <T>() -> Task<T> signature, without the Result<_,_>
                          // It's handled by the following generated code:
                          // let! bodyArgs = task {
                          //   try
                          //     let! bodyArgs = httpContext.bindFormatAsync<BodyInputForBinding>()
                          //     return Ok bodyArgs
                          //     |> Result.map defaulter
                          //     |> Result.bind nullChecker
                          //   with e -> FormatterBindingException e
                          //   |> BodyBindingError
                          //   |> Error
                          let defaultImplementationEmitter implDefn (maybePath: TypeSchema option) =
                                // get query input type if present
                                let maybeQuery =
                                    method.Parameters
                                    |> Option.bind (Map.tryFind Query)
                                // get QueryInputForBinding if present
                                let maybeQueryBindingType =
                                    maybeQuery
                                    |> Option.map ^ fun q ->
                                        let tmpSchema = temporarySchemasForBindingBeforeDefaultsApplianceMap |> Map.tryFind (SourceType q.Name)
                                        tmpSchema, q
                                    
                                // generate query binding if there is query input
                                let queryBinding = "queryArgs"
                                let maybeBindQuery =
                                    maybeQueryBindingType
                                    |> Option.map ^ fun (queryForBindingSchema, querySchema) ->
                                        // choose binding type for query - either InputFromQuery or InputFromQueryForBinding
                                        let bindingType =
                                            queryForBindingSchema
                                            |> Option.map (fun v -> v.GeneratedName)
                                            |> Option.defaultValue querySchema.Name
                                            |> synType
                                        // generate raw binding (call TryBindQuery for bindingType and map error)
                                        let bindRaw =
                                            app
                                                (typeApp (longIdentExpr "ctx.TryBindQueryString") [bindingType])
                                                (longIdentExpr "System.Globalization.CultureInfo.InvariantCulture")
                                            ^|> Result.mapErrorExpr (identExpr CodeGenErrorsDU.errInnerGiraffeBinding ^>> identExpr CodeGenErrorsDU.errOuterQuery)
                                        // generate Result.map defaulter if ForBinding type is being used
                                        queryForBindingSchema
                                        |> Option.map ^ fun v ->
                                            bindRaw
                                            ^|> Result.mapExpr (DefaultsGeneration.generateDefaultMappingFunFromSchema temporarySchemasForBindingBeforeDefaultsApplianceMap v querySchema)
                                        // or just take raw binding if there are no defaults
                                        |> Option.defaultValue bindRaw
                                        // and codegen null-checking (always generated because we wrap even a single parameter into a record and the record is a reference type)
                                        |> CodeGenNullChecking.bindNullCheckIntoResult "query" querySchema CodeGenErrorsDU.errOuterQuery
                                    // and apply letExpr to generated binding to generate `let queryArgs = bindQuery()`, leaving continuation not applied 
                                    |> Option.map ^ letExpr queryBinding []
                                
                                // generate path "binding"
                                let pathBinding = "pathArgs"
                                let maybeBindPath =
                                    maybePath
                                    |> Option.map
                                        (
                                            fun p ->
                                                // create generic result expr
                                                let res = typeApp (identExpr "Result") [extractResponseSynType (Some p.Name) p.Kind; synType CodeGenErrorsDU.errOuterTypeName]
                                                // call Result<_,_>.Ok
                                                let okCall = SynExpr.DotGet(res,r,longIdentWithDots "Ok",r)
                                                // apply call to pathArgs
                                                let expr = app okCall (identExpr "pathArgs")
                                                // and generate null-checking
                                                CodeGenNullChecking.bindNullCheckIntoResult "path" p CodeGenErrorsDU.errOuterPath expr
                                        )
                                    // and apply letExpr to generated binding to generate `let pathArgs = bindPath()`, leaving continuation not applied 
                                    |> Option.map ^ letExpr pathBinding []

                                // generate mapping for combined record from cases 3. and 5.
                                let combinedInputs = "combinedArgs"
                                // get the names and sources of bindings
                                let nonCombinedNonBodyArgs =
                                    [
                                        maybeBindPath |> Option.map ^ fun _ -> Path, pathBinding
                                        maybeBindQuery |> Option.map ^ fun _ -> Query, queryBinding
                                    ]
                                    |> List.choose id
                                    
                                let maybeBindCombined =
                                    Option.map2 (fun a b -> a,b) maybeCombinedType maybeCombinedTypeOpenApi
                                    |> Option.bind
                                        (
                                            fun (combinedRecordSynType, combinedRecordSchema) ->
                                                let combinedRecordPropertyToOriginalValue =
                                                    getCombinedRecordPropertyNamesFrom method.Parameters.Value
                                                    |> Seq.map (fun r -> r.CombinedRecordPropertyName, (r.OriginalLocation, r.OriginalName))
                                                    |> Map
                                                // do nothing if there is only one non-body arg
                                                nonCombinedNonBodyArgs
                                                |> fun x -> if x.Length > 1 then Some x else None
                                                // generate combined record mapping otherwise
                                                |> Option.map ^ generateInputsCombination combinedRecordPropertyToOriginalValue combinedRecordSynType combinedRecordSchema
                                        )
                                    // and apply letExpr to generated binding to generate `let combinedNonBodyArgs = combineNonBodyArgs()`, leaving continuation not applied
                                    |> Option.map ^ letExpr combinedInputs []
                                
                                // generate body binding
                                let bodyBinding = "bodyArgs"
                                // try get BodyInputForBinding is there is BodyInput and return both
                                let maybeBodyBindingType =
                                    maybeBodyOpenApi
                                    |> Option.map ^ fun bodyInputLocationAndSchema ->
                                        let bodyInputForBindingMapping = temporarySchemasForBindingBeforeDefaultsApplianceMap |> Map.tryFind (SourceType (snd bodyInputLocationAndSchema).Name)
                                        bodyInputForBindingMapping, bodyInputLocationAndSchema
                                // generate binding itself
                                let maybeBindBody =
                                    maybeBodyBindingType
                                    |> Option.map ^ fun (bodyInputForBindingMapping, (location, bodySchema)) ->
                                        letBangExpr bodyBinding
                                            (
                                                // decide on which binding method to call based on location content-type (not the header value) 
                                                let expr =
                                                    match location with
                                                    | Body contentType ->
                                                        match contentType with
                                                        | MediaType.Form -> "ctx.BindFormAsync"
                                                        | MediaType.Json -> "ctx.BindJsonAsync"
                                                        | v -> failwithf "Content type %A is not supported" v 
                                                    | _ -> failwith "Body should be located in body, you know"
                                                    |> longIdentExpr
                                                // add type parameter to the chosen method
                                                let typeApp = typeApp expr [bodyInputForBindingMapping |> Option.map (fun v -> v.GeneratedName) |> Option.defaultValue bodySchema.Name |> synType]
                                                // and call it without arguments
                                                let call = app typeApp (tupleExpr [])
                                                // and let-bind it and wrap binding into Result<_,_> 
                                                let bindRaw =
                                                    letBangExpr bodyBinding call (returnExpr ^ app (identExpr "Ok") (identExpr bodyBinding))
                                                let bindCallIntoResult =
                                                    // generate `|> Result.map defaulter` if there is BodyInputForBinding
                                                    bodyInputForBindingMapping
                                                    |> Option.map ^ fun v ->
                                                        bindRaw
                                                        ^|> Result.mapExpr (DefaultsGeneration.generateDefaultMappingFunFromSchema temporarySchemasForBindingBeforeDefaultsApplianceMap v bodySchema)
                                                    // or leave binding as is otherwise
                                                    |> Option.defaultValue bindRaw
                                                    // and generate null-checking anyways
                                                    |> CodeGenNullChecking.bindNullCheckIntoResult "body" bodySchema CodeGenErrorsDU.errOuterBody
                                                // generate "with" section body of try..with: 
                                                //   with e -> FormatterBindingException e
                                                //   |> BodyBindingError
                                                //   |> Error
                                                let catchClause =
                                                    [
                                                        returnExpr (app (identExpr CodeGenErrorsDU.errInnerFormatterBindingExn) (identExpr "e"))
                                                        ^|> identExpr CodeGenErrorsDU.errOuterBody ^|> identExpr "Error" 
                                                        |> clause (SynPat.Named(SynPat.Wild r, ident "e", false, None, r))
                                                    ]
                                                // and make it `try {bindBody} with e -> {catchClause}`
                                                taskBuilder
                                                ^ SynExpr.TryWith(bindCallIntoResult, r, catchClause, r, r, DebugPointAtTry.Yes r, DebugPointAtWith.Yes r)
                                            )
                                            
                                // now all bindings and almost every mapping are generated in form of SynExpr -> SynExpr
                                // (let sourceArgs = bindSource() in parameter)
                                // left to do:
                                // - Assemble Result<FinalArgs, ArgumentLocationedError> from inputs
                                // This result is used to decide if we call service.ProcessInput or service.ProcessError
                                // - Generate the call to service 
                                // - Apply all bindings in right order
                                // - Return the resulting expression
                                            
                                let nonCombinedBodyArgs =
                                    [
                                        maybeBodyOpenApi |> Option.map fst |> Option.map (fun l -> l, bodyBinding)
                                    ]
                                    |> List.choose id
                                
                                let finalArgsBinding = "args"
                                
                                // try to get either single non-body binding name or combined non-body inputs binding name
                                let nonBody =
                                    nonCombinedNonBodyArgs
                                    |> List.tryExactlyOne
                                    |> Option.map snd
                                    |> Option.orElse (maybeCombinedType |> Option.map (fun _ -> combinedInputs))
                                // try to get body binding name 
                                let body = nonCombinedBodyArgs |> List.tryExactlyOne |> Option.map snd
                                                                
                                let maybeBindFinalArgs =
                                    // if we've got both body and non-body
                                    Option.map2 (fun a b -> a,b) nonBody body
                                    |> Option.map ^ fun (nonBody, body) ->
                                        // tuplify them
                                        generateBinds nonBody [body] ^ fun () -> tupleExpr [nonBody; body]
                                    // or use the single arguments object 
                                    |> Option.orElse (body |> Option.orElse nonBody |> Option.map identExpr)
                                    // create combinable SynExpr -> SynExpr from the result
                                    |> Option.map ^ letExpr finalArgsBinding []
                                    // and combine errors preserving the signature
                                    // Errors combination is required because all result combinations use nested Result.bind without error handling
                                    |> Option.map ^ fun f continuation ->
                                        f
                                        ^
                                            // try get raw binding errors (combined bindings may have errors muted, so use the raw ones)
                                            // CodeGenErrorsDU.tryExtractError = function | Ok _ -> None | Error e -> Some e
                                            let allRawBindings =
                                                nonCombinedBodyArgs
                                                |> Seq.append nonCombinedNonBodyArgs
                                                |> Seq.map snd
                                                |> Seq.map ^ fun result -> app (identExpr CodeGenErrorsDU.tryExtractErrorName) (identExpr result)
                                                |> Seq.toList
                                            // if there is only one raw binding, no error may be muted
                                            if allRawBindings.Length = 1 then
                                                continuation
                                            else
                                                // let finalArgs =
                                                letExpr finalArgsBinding []
                                                    (
                                                        simpleValueMatching finalArgsBinding
                                                            [
                                                                "Ok", "v", app (identExpr "Ok") (identExpr "v")
                                                                "Error", "e",
                                                                    let errExpr =
                                                                        /// let errs =
                                                                        ///   [ tryExtractError queryArgs
                                                                        ///     tryExtractError pathArgs
                                                                        ///     tryExtractError bodyArgs ]
                                                                        ///   |> Seq.choose id
                                                                        ///   |> Seq.toArray
                                                                        letExpr "errs" []
                                                                            (
                                                                                SynExpr.ArrayOrList(false, allRawBindings, r)
                                                                                ^|> seqChooseId
                                                                                ^|> longIdentExpr "Seq.toArray"
                                                                            )
                                                                        /// if errs.Length > 1 then
                                                                        ///   CombinedArgumentLocationError errs
                                                                        /// else Array.head errs
                                                                            (
                                                                                paren
                                                                                ^ ifElseExpr
                                                                                      (
                                                                                          app (appI (identExpr "op_GreaterThan") (longIdentExpr "errs.Length")) (constExpr ^ SynConst.Int32 1)
                                                                                      )
                                                                                      (app (identExpr CodeGenErrorsDU.errOuterCombined) (identExpr "errs"))
                                                                                      (app (longIdentExpr "Array.head") (identExpr "errs"))
                                                                            )
                                                                        /// |> Error
                                                                    errExpr ^|> identExpr "Error"
                                                            ]
                                                    ) continuation
                                
                                // generate the call to service
                                
                                let argExpr = identExpr "ctx"
                                
                                let finalCall =
                                    // let! logicOutput =
                                    letBangExpr                                                                                 
                                      "logicOutput"                                                                                 
                                      (
                                          // service.ProcessInput method expr
                                          let sucMethod = sprintf "this.%sInput" method.Name |> longIdentExpr
                                          // service.ProcessInputError method expr
                                          let errMethod = sprintf "this.%sInputError" method.Name |> longIdentExpr
                                          // get final arguments list
                                          let allBindings = [ nonBody; body ] |> List.choose id
                                          /// no arguments = no errors, so
                                          /// service.ProcessInput ctx
                                          if allBindings.Length = 0 then
                                              app sucMethod argExpr
                                          else
                                              /// we've got an error possible, so
                                              /// match finalArgs with
                                              matchExpr finalArgsBinding
                                                [
                                                    /// | Ok (bodyArgs) ->
                                                    /// or
                                                    /// | Ok (queryArgs) ->
                                                    /// or
                                                    /// | Ok (pathArgs) ->
                                                    /// or
                                                    /// | Ok (combinedArgs) ->
                                                    /// or
                                                    /// | Ok (combinedArgs, bodyArgs) ->
                                                    clause (SynPat.LongIdent(longIdentWithDots "Ok", None, None, Pats <| [tuplePat allBindings], None, r))
                                                    /// service.ProcessInput(combinedArgs, bodyArgs, ctx)
                                                    ^ app sucMethod (tupleExpr [ yield! allBindings; "ctx" ])
                                                    /// | Error e -> service.ProcessError(e, ctx)
                                                    clause (synLongPat "Error" "e")
                                                    ^ app errMethod (tupleExpr ["e"; "ctx"])
                                                ]
                                      )
                                      /// and generate return expr:
                                      /// return! service.GetResponseFromOutput logicOutput next ctx
                                      (returnBang                                                                             
                                       ^ curriedCall (sprintf "this.%sOutput" method.Name)
                                             [
                                                 if respondsUnit then
                                                     "()" // logic has no output
                                                 else
                                                    "logicOutput"
                                                 "next"
                                                 "ctx"
                                             ])

                                /// Utility for combining Option<SynExpr -> SynExpr> where first SynExpr is a continuation for the logic contained in function
                                let makeCall maybeBind finalCall =
                                    maybeBind
                                    |> Option.map (fun b -> b ^ finalCall)
                                    |> Option.defaultValue finalCall
                                
                                let finalCall =
                                    // the code below works like backpipe because each next call is applied as a callback to the previous one
                                    // below are code samples of what are intermediate results like if all options are Some
                                    
                                    /// return! doSomething finalArgs ctx next
                                    finalCall
                                    /// let! finalArgs = getFinalArgs combinedArgs bodyArgs
                                    /// return! doSomething finalArgs ctx next
                                    |> makeCall maybeBindFinalArgs
                                    /// let! bindBody = getBodyArgs()
                                    /// let! finalArgs = getFinalArgs combinedArgs bodyArgs
                                    /// return! doSomething finalArgs ctx next
                                    |> makeCall maybeBindBody
                                    /// let combinedArgs = getCombinedArgs pathArgs queryArgs
                                    /// let! bindBody = getBodyArgs()
                                    /// let! finalArgs = getFinalArgs combinedArgs bodyArgs
                                    /// return! doSomething finalArgs ctx next
                                    |> makeCall maybeBindCombined
                                    /// let pathArgs = getPathArgs()
                                    /// let combinedArgs = getCombinedArgs pathArgs queryArgs
                                    /// let! bindBody = getBodyArgs()
                                    /// let! finalArgs = getFinalArgs combinedArgs bodyArgs
                                    /// return! doSomething finalArgs ctx next
                                    |> makeCall maybeBindPath
                                    /// let queryArgs = getQueryArgs()
                                    /// let pathArgs = getPathArgs()
                                    /// let combinedArgs = getCombinedArgs pathArgs queryArgs
                                    /// let! bindBody = getBodyArgs()
                                    /// let! finalArgs = getFinalArgs combinedArgs bodyArgs
                                    /// return! doSomething finalArgs ctx next
                                    |> makeCall maybeBindQuery
                                
                                /// override {methodDef} = fun ctx next -> task {
                                ///   let queryArgs = getQueryArgs()
                                ///   let pathArgs = getPathArgs()
                                ///   let combinedArgs = getCombinedArgs pathArgs queryArgs
                                ///   let! bindBody = getBodyArgs()
                                ///   let! finalArgs = getFinalArgs combinedArgs bodyArgs
                                ///   return! doSomething finalArgs
                                /// }
                                implDefn                                                                                      
                                ^ lambdaFunNextCtxExpr                                                                        
                                ^ taskBuilder                                                                                 
                                ^ finalCall
                                  
                          // emitting httpHandler default implementation method or property
                          let defaultHttpHandler =        
                              maybePath
                              |> Option.map ^ fun _ ->
                                    // override this.{method.Name} pathArgs = fun next ctx ->task {
                                    //     let! input = this.{method.Name}Input(args, ctx)
                                    //     return! this.{method.Name}Output input next ctx
                                    // }
                                    defaultImplementationEmitter
                                        (methodImplDefn method.Name ["pathArgs"])
                              |> Option.defaultWith ^ fun _ ->
                                    // override this.{method.Name} = fun next ctx ->task {
                                    //     let! input = this.{method.Name}Input ctx
                                    //     return! this.{method.Name}Output input next ctx
                                    // }
                                    defaultImplementationEmitter
                                        (methodImplDefn method.Name [])
                          defaultHttpHandler maybePathOpenApi
                          
                          // abstract {method.Name}Input: {fullReturnType} -> Task<{returnType}>
                          abstractMemberDfn xmlEmpty (method.Name + "Input") (fullInputReturnType ^-> taskOf [ returnType ])
                          if maybeParams.IsSome then
                              let errorHandlerName = method.Name + "InputError"
                              // abstract {method.Name}InputError: error * HttpContext -> Task<{returnType}>
                              abstractMemberDfn xmlEmpty errorHandlerName (tuple [synType CodeGenErrorsDU.errOuterTypeName; synType "HttpContext"] ^-> taskOf [ returnType ])
                              methodImplDefn errorHandlerName ["t"]
                                (
                                    letExprComplex (tuplePat ["err"; "http"]) (identExpr "t")
                                    ^ letExpr "err" [] (app (app (identExpr CodeGenErrorsDU.outerErrToStringName) (constExpr <| SynConst.Int32 0)) (identExpr "err"))
                                    ^ app (typeApp (longIdentExpr "Task.FromException") [returnType]) (paren <| app (identExpr "exn") ("err" |> identExpr))
                                )
                          // abstract {method.Name}Output: {returnType} -> HttpHandler
                          abstractMemberDfn xmlEmpty (method.Name + "Output") (returnType ^-> synType "HttpHandler")
                          
                          // giraffe handler for particular response
                          let response2handler (response: Response) inputBinding =
                                let contentHandler =
                                    // currently everything goes to json
                                    match response.MediaType with
                                    | Json
                                    | NotSpecified -> app (identExpr "json" ) (identExpr inputBinding)
                                    | Form -> app (identExpr "form" ) (identExpr inputBinding)
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
                                response2handler method.Responses.Head (if respondsUnit then "()" else "input")
                          
                          // override this.{method.Name}Output input =
                          //    match input with
                          //    | Choice1Of2 array -> json array
                          //    | Choice2Of2 () -> setStatusCode 404 }
                          methodImplDefn (method.Name + "Output") [if not respondsUnit then "input" else "()"] body ]

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
