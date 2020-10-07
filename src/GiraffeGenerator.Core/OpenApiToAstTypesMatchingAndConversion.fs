module OpenApiToAstTypesMatchingAndConversion

open System.Collections.Generic
open System.Globalization
open AST
open ASTExt
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
open OpenApi

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

/// matching OpenAPI string IR to SynType
let strFormatMatch =
    function
    | StringFormat.String
    | PasswordString -> stringType
    | Byte -> arrayOf byteType
    | Binary -> arrayOf byteType
    | DateString -> dateType
    | DateTimeString -> dateTimeType
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
    | PrimTypeKind.Long -> int64Type
    | PrimTypeKind.Double -> doubleType
    | Bool -> boolType
    | PrimTypeKind.String strFormat -> strFormatMatch strFormat

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
            ^ fun (name, typeKind, def) -> AST.ident name, extractResponseSynType typeKind

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
                |> List.map (fun (fieldName, fieldKind, def) ->
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
let mutable private optionalTypes = 0
let rec generateOptionalType kind def nameFromSchema =
    match kind with
    | TypeKind.Object o ->
      let mutable hasPropertiesWithDefault = false
      
      let props =
          o.Properties
          |> List.map ^ fun (name, kind, def) ->
              let (hasDefault, kind, _), (subGenerated: {| GeneratedName:string; Generated:TypeKind; OriginalName:string; Original: TypeKind|} list) = generateOptionalType kind def (Some name)
              if hasDefault then
                  hasPropertiesWithDefault <- true
              let maybeObj = match kind with | TypeKind.Option (TypeKind.Object v) -> TypeKind.Object v |> Some | _ -> None
              if hasDefault && maybeObj.IsSome then
                  let (isNew, newKind, _), subGenerated = generateOptionalType maybeObj.Value None (Some name)
                  if isNew then
                      subGenerated, (name, TypeKind.Option newKind, None)
                  else subGenerated, (name, TypeKind.Option kind, def)
              else
                subGenerated, (name, kind, def)
      
      let kind =
          {
              o with
                  Name = o.Name
                  |> Option.orElse nameFromSchema
                  |> Option.map (fun x -> x + "ForBinding")
                  |> Option.defaultWith
                         (
                             fun _ ->
                                 sprintf "TemporaryTypeForBinding%d" <| System.Threading.Interlocked.Increment(ref optionalTypes)
                         ) |> Some
                  Properties = props |> List.map snd
          }
      let generated =
          [
              if kind.Name.IsSome then
                  {|
                      OriginalName = o.Name |> Option.orElse nameFromSchema |> Option.get
                      Original = TypeKind.Object o
                      GeneratedName = kind.Name.Value
                      Generated = TypeKind.Object kind
                  |}
              yield! props |> Seq.collect fst
          ]
      (hasPropertiesWithDefault, TypeKind.Object kind, kind.Name), generated
    | TypeKind.Option opt ->
        (true, TypeKind.Option opt, None), []
    | v ->
      let isDefaultable = Option.isSome def
      let kind = if isDefaultable then TypeKind.Option v else v
      (isDefaultable, kind, None), []
      

let rec defaultToExpr v =
    let inline cnst syn v =
        constExpr (syn v)
    match v with
    | DefaultableKind.Boolean b -> cnst SynConst.Bool b
    | DefaultableKind.Date d ->
        let components =
            [
              d.Year
              d.Month
              d.Day
            ] |> List.map (cnst SynConst.Int32)
        app (longIdentExpr "System.DateTime") (tupleComplexExpr components)
    | DefaultableKind.Double d -> cnst SynConst.Double d
    | DefaultableKind.Guid g ->
        let gString = g.ToString("D", CultureInfo.InvariantCulture)
        app (longIdentExpr "System.Guid.ParseExact") (tupleComplexExpr [strExpr gString; strExpr "D"])
    | DefaultableKind.Integer i -> cnst SynConst.Int32 i
    | DefaultableKind.Long l -> cnst SynConst.Int64 l
    | DefaultableKind.String s -> strExpr s
    | DefaultableKind.Uri u ->
        let uString = u.ToString()
        app (longIdentExpr "System.Uri") (strExpr uString)
    | DefaultableKind.DateTime dt ->
        let dtTicks = dt.DateTime.Ticks |> DefaultableKind.Long |> defaultToExpr
        let tsTicks = dt.Offset.Ticks |> DefaultableKind.Long |> defaultToExpr
        let ts = app (longIdentExpr "System.TimeSpan.FromTicks") (tupleComplexExpr [ tsTicks ])
        app (longIdentExpr "System.DateTimeOffset") (tupleComplexExpr [ dtTicks; ts ])
    |> paren
    
module rec DefaultsGeneration =
    let rec generateDefaultRecordMapping defaultsMap instanceName source =
        let source = match source with TypeKind.Object o -> o | _ -> failwith "source should be an object"
        recordExpr
            [
                for name, kind, def in source.Properties do
                    let propPath = instanceName + "." + name
                    let indented = longIdentExpr propPath
                    match kind with
                    | TypeKind.Object _ ->
                        name, generateDefaultRecordMapping defaultsMap propPath kind
                    | TypeKind.Option v ->
                        let obj = match v with | TypeKind.Object o -> Some o | _ -> None
                        if def.IsSome then
                            name, indented ^|> Option.defaultValueExpr (defaultToExpr def.Value)
                        elif obj.IsSome then
                            let _, originalKind = defaultsMap |> Map.find (obj.Value.Name |> Option.defaultValue name)
                            name, indented ^|> Option.mapExpr (generateDefaultMappingFun defaultsMap v originalKind)
                        else name, indented
                    | _ ->
                        if def.IsSome then
                            name, indented ^|> longIdentExpr "Option.ofObj" ^|> Option.defaultValueExpr (defaultToExpr def.Value)
                        else
                            name, indented
            ]

    let generateDefaultMappingFun defaultsMap source outType =
        let param = "src"
        let recordExpr = generateDefaultRecordMapping defaultsMap param source
        let bindWithTypeAndReturn =
            letOrUseComplexDecl (SynPat.Typed(SynPat.Named(SynPat.Wild r, ident "v", false, None, r), extractResponseSynType outType, r))
                recordExpr (identExpr "v")
        lambda (simplePats [simplePat param]) bindWithTypeAndReturn |> paren