﻿module OpenApiToAstTypesMatchingAndConversion

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
let strFormatDefaultMatch =
    function
    | StringFormat.String _
    | PasswordString -> stringType
    | Byte -> arrayOf byteType
    | Binary -> arrayOf byteType
    | DateString -> dateTimeType
    | DateTimeString -> dateTimeOffsetType
    | Custom "uri"
    | Custom "uriref" -> uriType
    | Custom "uuid"
    | Custom "guid"
    | Custom "uid" -> guidType
    | Custom name -> synType name

let strFormatNodaTimeDateTimeMatch =
    function
    | Configuration.DateTimeGeneratedType.ZonedDateTime -> nodaTypes.ZonedDateTime
    | Configuration.DateTimeGeneratedType.OffsetDateTime -> nodaTypes.OffsetDateTime
    | Configuration.DateTimeGeneratedType.LocalDateTime -> nodaTypes.LocalDateTime
    | Configuration.DateTimeGeneratedType.Instant -> nodaTypes.Instant

let strFormatNodaTimeMatch =
    function
    | Custom "local-date"
    | DateString -> Some nodaTypes.LocalDate
    | DateTimeString -> strFormatNodaTimeDateTimeMatch Configuration.value.MapDateTimeInto |> Some
    | Custom "instant" -> Some nodaTypes.Instant
    | Custom "time"
    | Custom "local-time" -> Some nodaTypes.LocalTime
    | Custom "local-date-time" -> Some nodaTypes.LocalDateTime
    | Custom "offset-date-time" -> Some nodaTypes.OffsetDateTime
    | Custom "zoned-date-time" -> Some nodaTypes.ZonedDateTime
    | Custom "offset"
    | Custom "time-offset" -> Some nodaTypes.Offset
    | Custom "duration" -> Some nodaTypes.Duration
    | Custom "period" -> Some nodaTypes.Period
    | Custom "time-zone"
    | Custom "date-time-zone" -> Some nodaTypes.DateTimeZone
    | _ -> None
    
let strFormatMatch format =
    Some Configuration.value
    |> Option.filter ^ fun x -> x.UseNodaTime
    |> Option.bind (fun _ -> strFormatNodaTimeMatch format)
    |> Option.defaultWith ^ fun _ -> strFormatDefaultMatch format

/// matching OpenAPI primitive type IR to SynType
let primTypeMatch =
    function
    | Any -> objType
    | Int _ -> intType
    | PrimTypeKind.Long _ -> int64Type
    | PrimTypeKind.Double _ -> doubleType
    | Bool -> boolType
    | PrimTypeKind.String strFormat -> strFormatMatch strFormat

/// matching type kinds in responses to create their syntatic types
let rec extractResponseSynType externalName =
    function
    | Prim primType -> primTypeMatch primType
    | Array (innerType, _, _) -> arrayOf (extractResponseSynType externalName innerType)
    | Option innerType -> optionOf (extractResponseSynType externalName innerType)
    | BuiltIn builtIn -> synType builtIn
    | Object { Name = Some name } -> synType name
    | Object _ when (Option.isSome externalName) -> synType (Option.get externalName)
    | Object anonObject ->
        let fields =
            anonObject.Properties
            |> List.map
            ^ fun (name, typeKind, def) -> AST.ident name, extractResponseSynType (Some name) typeKind

        if fields.IsEmpty then objType else anonRecord fields
    | DU du -> synType du.Name
    | NoType -> unitType
    
let rec extractBodyBindingSynType defaultTypeName nameFromSchema kind =
    match defaultTypeName with
    | None -> extractResponseSynType (Some nameFromSchema) kind
    | Some generatedName ->
        let rec extract kind =
            match kind with
            | Array (kind, _, _) -> extract kind |> arrayOf
            | Option kind -> extract kind |> optionOf
            | Object _ -> synType generatedName
            | _ -> failwith "no generated name can exist for non-object type"
        extract kind
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
    
[<Struct>]
type GeneratedOptionalTypeMappingNameKind =
    | GeneratedType of generatedTypeName:string
    | SourceType of sourceTypeName:string
    
type GeneratedOptionalTypeMapping =
    {
        GeneratedName: string
        Generated: TypeKind
        OriginalName: string
        Original: TypeKind
    }
let mutable private optionalTypes = 0
let rec private generateOptionalTypeInternal kind def nameFromSchema =
    match kind with
    | TypeKind.Object o ->
      let mutable hasPropertiesWithDefault = false
      
      let props =
          o.Properties
          |> List.map ^ fun (name, kind, def) ->
              let (hasDefault, generatedKind), (subGenerated: GeneratedOptionalTypeMapping list) = generateOptionalTypeInternal kind def (Some name)
              if hasDefault then
                  hasPropertiesWithDefault <- true
              let kind = if hasDefault then generatedKind else kind
              subGenerated, (name, kind, def)
      
      let kind =
          {
              o with
                  Name = o.Name
                  |> Option.orElse nameFromSchema
                  |> Option.map (fun x -> x + "ForBinding")
                  |> Option.orElseWith
                         (
                             fun _ ->
                                 sprintf "TemporaryTypeForBinding%d" (System.Threading.Interlocked.Increment(ref optionalTypes)) |> Some
                         )
                  Properties = props |> List.map snd
          }
      let generated =
          [
              if hasPropertiesWithDefault then
                  {
                      OriginalName = o.Name |> Option.orElse nameFromSchema |> Option.get
                      Original = TypeKind.Object o
                      GeneratedName = kind.Name.Value
                      Generated = TypeKind.Object kind
                  }
              yield! props |> Seq.collect fst
          ]
      (hasPropertiesWithDefault, TypeKind.Object kind), generated
    | TypeKind.Option opt ->
        let (hasDef, kind), generated = generateOptionalTypeInternal opt def nameFromSchema
        let kind = if hasDef then kind else opt
        (hasDef, TypeKind.Option kind), generated
    | TypeKind.Array (item, def, validation) ->
        let (hasDef, kind), generated = generateOptionalTypeInternal item def nameFromSchema
        let kind = if hasDef then kind else item
        (hasDef, TypeKind.Array (kind, def, validation)), generated 
    | v ->
      let isDefaultable = Option.isSome def
      let kind = if isDefaultable then TypeKind.Option v else v
      (isDefaultable, kind), []
let generateOptionalType kind def nameFromSchema =
    generateOptionalTypeInternal kind def nameFromSchema
    |> snd

let rec defaultToExpr v =
    let inline cnst syn v =
        constExpr (syn v)
    match v with
    | DefaultableKind.Noda nt ->
        if not Configuration.value.UseNodaTime then
            failwith "No NodaTime types should be used if user doesn't ask for NodaTime"
        let calendarSystem (s: NodaTime.CalendarSystem) =
            app (longIdentExpr "NodaTime.CalendarSystem.ForId") (strExpr s.Id)
        let era =
            function
            | x when x = NodaTime.Calendars.Era.Bahai -> "NodaTime.Calendars.Era.Bahai"
            | x when x = NodaTime.Calendars.Era.Common -> "NodaTime.Calendars.Era.Common"
            | x when x = NodaTime.Calendars.Era.AnnoHegirae -> "NodaTime.Calendars.Era.AnnoHegirae"
            | x when x = NodaTime.Calendars.Era.AnnoMartyrum -> "NodaTime.Calendars.Era.AnnoMartyrum"
            | x when x = NodaTime.Calendars.Era.AnnoMundi -> "NodaTime.Calendars.Era.AnnoMundi"
            | x when x = NodaTime.Calendars.Era.AnnoPersico -> "NodaTime.Calendars.Era.AnnoPersico"
            | x when x = NodaTime.Calendars.Era.BeforeCommon -> "NodaTime.Calendars.Era.BeforeCommon"
            | e -> failwithf "Unknown era %s" e.Name
            >> longIdentExpr
            
        match nt with
        | Instant i -> app (longIdentExpr "NodaTime.Instant.FromUnixTimeTicks") (cnst SynConst.Int64 (i.ToUnixTimeTicks()))
        | LocalDate d ->
            let components =
                [
                  d.YearOfEra
                  d.Month
                  d.Day
                ] |> List.map (cnst SynConst.Int32)
            let era = era d.Era
            let calendar = calendarSystem d.Calendar
            let components = [ era; yield! components; calendar ]
            app (longIdentExpr "NodaTime.LocalDate") (tupleComplexExpr components)
        | LocalTime t ->
            app (longIdentExpr "NodaTime.LocalTime.FromTicksSinceMidnight") (cnst SynConst.Int64 t.TickOfDay)
        | LocalDateTime ldt -> 
            let instant = ldt.InUtc().ToInstant() |> Instant |> Noda |> defaultToExpr
            let calendar = calendarSystem ldt.Calendar
            let utc = longIdentExpr "NodaTime.DateTimeZone.Utc"
            let components = [utc; calendar]
            letExpr "inst" [] instant
                ^ letExpr "zdt" []
                    (app (longIdentExpr "inst.InZone") (tupleComplexExpr components))
                    (longIdentExpr "zdt.LocalDateTime")
        | OffsetDateTime odt ->
            let ldt = defaultToExpr (Noda ^ LocalDateTime odt.LocalDateTime)
            let offset = defaultToExpr (Noda ^ Offset odt.Offset)
            app (longIdentExpr "NodaTime.OffsetDateTime") (tupleComplexExpr [ldt; offset])
        | ZonedDateTime zdt ->
            let instant = defaultToExpr (Noda ^ Instant ^ zdt.ToInstant())
            let zone = defaultToExpr (Noda ^ DateTimeZone zdt.Zone)
            let calendar = calendarSystem zdt.Calendar
            app (longIdentExpr "NodaTime.ZonedDateTime") (tupleComplexExpr [instant; zone; calendar])
        | Offset o ->
            app (longIdentExpr "NodaTime.Offset.FromTicks") (cnst SynConst.Int64 o.Ticks)
        | Duration d ->
            app (longIdentExpr "NodaTime.Duration.FromTicks") (cnst SynConst.Double d.TotalTicks)
        | Period p ->
            let assigns =
                // not something like "FromTicks" because period P1H1M is different from P61M 
                [
                    "Years", SynConst.Int32 p.Years
                    "Months", SynConst.Int32 p.Months
                    "Weeks", SynConst.Int32 p.Weeks
                    "Days", SynConst.Int32 p.Days
                    "Hours", SynConst.Int64 p.Hours
                    "Minutes", SynConst.Int64 p.Minutes
                    "Seconds", SynConst.Int64 p.Seconds
                    "Milliseconds", SynConst.Int64 p.Milliseconds
                    "Ticks", SynConst.Int64 p.Ticks
                    "Nanoseconds", SynConst.Int64 p.Nanoseconds
                ]
            let builder = app (longIdentExpr "NodaTime.PeriodBuilder") (tupleExpr [])
            let var = "builder"
            let doAssigns =
                assigns
                |> List.filter (snd >> function | SynConst.Int32 v -> v <> 0 | SynConst.Int64 v -> v <> 0L | _ -> true)
                |> List.map (fun (n,v) -> sprintf "%s.%s" var n, v)
                |> List.map (fun (n,v) -> SynExpr.LongIdentSet(longIdentWithDots n, constExpr v, r))
                |> List.append
            let doAssigns =
                doAssigns [app (longIdentExpr (sprintf "%s.Build" var)) (tupleExpr [])]
                |> seqExprs
            letExpr var [] builder doAssigns
        | DateTimeZone z ->
            app (longIdentExpr "NodaTime.DateTimeZoneProviders.Tzdb.Item") (strExpr z.Id)
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
    
/// rec module here is for cross-recursion mapping <-> fun x -> mapping
/// mapping -> fun... dependency is required for mapping of options and arrays
/// e.g.
/// Result.map (fun (x: SomeTypeForBinding) ->
///   let v: SomeType =
///     { intWithDefault = x.intWithDefault |> Option.defaultValue 100
///       optionalArrayOfArrayOption =
///         x.optionalArrayOfArrayOption
///         |> Option.map (fun (x: AnotherTypeForBinding array option array) ->
///              x |> Array.map (fun (x: AnotherTypeForBinding array option) ->
///                     x |> Option.map (fun (x: AnotherTypeForBinding array) ->
///                            x |> Array.map (fun (x: AnotherTypeForBinding) ->
///                                   let v: AnotherType =
///                                     { forGodsSakeWhy = x.forGodsSakeWhy |> Option.defaultValue "pa$$word"
///                                       andNonDefaultableToo = x.andNonDefaultableToo}
///                                 )
///                          )
///                   )
///            )
/// )
/// where `fun (x[: Type]) -> [let v = ](mapping);[v]` are generated by generateDefaultMappingFun* family of functions
/// and (mapping) itself is generated by generateDefaultMapping function 
module rec DefaultsGeneration =
    let private generateBestPossibleFunc kind defaultsMap def nameFromSchema =
        getOwnName kind ^ fun _ -> Unchecked.defaultof<string>
        |> Option.ofObj
        |> Option.map GeneratedType
        |> Option.map Map.tryFind
        |> Option.bind ((|>) defaultsMap)
        |> Option.map (generateDefaultMappingFunFromMapping defaultsMap def)
        |> Option.defaultWith (fun _ -> generateDefaultMappingFunFromKind defaultsMap kind def nameFromSchema)
    
    let rec private generateDefaultMapping defaultsMap source def nameFromSchema instanceName =
        let indented = longIdentExpr instanceName
        match source with
            | TypeKind.Object source ->
                let mutable hasDefaultProps = false
                let record =
                    recordExpr
                        [
                            for name, kind, def in source.Properties do
                                let propPath = instanceName + "." + name
                                let hasDefaults, subMapping = generateDefaultMapping defaultsMap kind def name propPath
                                hasDefaultProps <- hasDefaultProps || hasDefaults
                                name, subMapping
                        ]
                if hasDefaultProps then
                    true, record
                else false, indented
            | TypeKind.Array (item, def, _) ->
                let hasDefaults, func = generateBestPossibleFunc item defaultsMap def nameFromSchema
                if hasDefaults then
                    true, indented ^|> app (longIdentExpr "Array.map") func
                else false, indented
            | TypeKind.Option v ->
                if def.IsSome then
                    true, indented ^|> Option.defaultValueExpr (defaultToExpr def.Value)
                else
                    let hasDefaults, func = generateBestPossibleFunc v defaultsMap def nameFromSchema
                    if hasDefaults then
                        true, indented ^|> Option.mapExpr func
                    else false, indented
            | _ ->
                if def.IsSome then
                    failwith "should be handled by option"
                else
                    false, indented

    let private generateDefaultMappingFunFromKind defaultsMap kind sourceDef nameFromSchema =
        let param = "src"
        let hasDefaults, recordExpr = generateDefaultMapping defaultsMap kind sourceDef nameFromSchema param
        hasDefaults, lambda (singleTypedPat param <| extractResponseSynType (Some nameFromSchema) kind) recordExpr

    let private generateDefaultMappingFunFromMapping defaultsMap sourceDef (mapping: GeneratedOptionalTypeMapping) =
        let param = "src"
        let hasDefaults, recordExpr = generateDefaultMapping defaultsMap mapping.Generated sourceDef mapping.OriginalName param
        let bindWithTypeAndReturn =
            letExprComplex (SynPat.Typed(SynPat.Named(SynPat.Wild r, ident "v", false, None, r), synType mapping.OriginalName, r))
                recordExpr (identExpr "v")
        hasDefaults, lambda (singleTypedPat param <| synType mapping.GeneratedName) bindWithTypeAndReturn
        
    let generateDefaultMappingFunFromSchema defaultsMap mapping (schema: TypeSchema) =
        let hasDefault, fn = generateDefaultMappingFunFromMapping defaultsMap schema.DefaultValue mapping
        if hasDefault then fn
        else _id