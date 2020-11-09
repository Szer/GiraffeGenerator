module OpenApi

open System
open Microsoft.OpenApi.Any
open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Readers
open System.IO
open System.Text.RegularExpressions
open NodaTime.Text
open OpenApiValidation

let inline trimLower (s: string) =
    if isNull s then None else
    s.Trim().ToLowerInvariant() |> Some

let inline splitLine (str: string) =
    str.Split('\n')
    |> Seq.toList

type Docs =
    { Summary: string list option
      Remarks: string list option
      Example: string list option }
    static member Create(summary, remarks, example: IOpenApiAny) =
        match Option.ofObj summary, Option.ofObj remarks, Option.ofObj example with
        | None, None, None -> None
        | summary, remarks, example ->
            { Summary = summary |> Option.map splitLine
              Remarks = remarks |> Option.map splitLine
              Example = example |> Option.map (string >> splitLine) }
            |> Some
        
/// Formats for string type.
/// URI is not standard, but included
/// It could be anything
type StringFormat =
    | String of StringValidation option
    | Byte
    | Binary
    | DateString
    | DateTimeString
    | PasswordString
    | Custom of string
    
    static member Parse schema (maybeFormat: string Option) =
        if maybeFormat.IsNone then String (StringValidation.TryParse schema) else
        match maybeFormat.Value with
        | "byte" -> Byte
        | "binary" -> Binary
        | "date" -> DateString
        | "date-time" -> DateTimeString
        | "password" -> PasswordString
        | x -> Custom x
        
/// Active patterns to quickly determine what type OpenApiSchema represents
let (|ArrayKind|ObjectKind|PrimKind|) (schema: OpenApiSchema) =
    match trimLower schema.Type with
    | Some "array"  | None when not (isNull schema.Items)      -> ArrayKind  schema.Items
    | Some "object" | None when not (isNull schema.Properties) -> ObjectKind schema
    | Some primType -> PrimKind primType
    | _ -> failwithf "unexpected type of schema: %A" schema.Type
        
/// All primitive type kinds for a fields in type definitions
type PrimTypeKind =
    | Int of IntValidation option
    | Long of LongValidation option
    | Double of FloatValidation option
    | Bool
    | Any
    | String of StringFormat
    
    static member Parse (typeKind: string, schema: OpenApiSchema) =
        
        match trimLower typeKind, trimLower schema.Format with
        | Some "integer", Some "int64" -> Long (LongValidation.TryParse schema) 
        | Some "integer", _ -> Int (IntValidation.TryParse schema)
        | Some "number", _ -> Double (FloatValidation.TryParse schema)
        | Some "boolean", _ -> Bool
        | Some "string", maybeStringFormat -> String(StringFormat.Parse schema maybeStringFormat)
        | _ -> failwithf "Unexpected type: %s and format: %A" typeKind schema.Format
        
    member s.GetDefaultLiteral (value: IOpenApiPrimitive) =
        let fw t = failwithf "%s literal has been found for non-%s type" t t
        let inline oneWay kind (value: ^v) (toDefault: ^t -> ^o) =
            match s with
            | x when x = kind ->
                let v = (^v:(member Value: ^t) value)
                toDefault v
            | _ -> fw (value.GetType().Name)
        
        let nodaMatch () =
            let inline matchDateTime dt =
                let inline oneWay f = oneWay (String DateTimeString) dt (f >> Some)
                match Configuration.value.MapDateTimeInto with
                | Configuration.DateTimeGeneratedType.ZonedDateTime -> oneWay (NodaTime.ZonedDateTime.FromDateTimeOffset >> ZonedDateTime) 
                | Configuration.DateTimeGeneratedType.OffsetDateTime -> oneWay (NodaTime.OffsetDateTime.FromDateTimeOffset >> OffsetDateTime)
                | Configuration.DateTimeGeneratedType.LocalDateTime -> oneWay (fun dto -> NodaTime.LocalDateTime.FromDateTime dto.DateTime |> LocalDateTime)
                | Configuration.DateTimeGeneratedType.Instant -> oneWay (fun dto -> NodaTime.OffsetDateTime.FromDateTimeOffset(dto).ToInstant() |> Instant)
            match value with
            | :? OpenApiDateTime as dt -> matchDateTime dt
            | :? OpenApiDate as d -> oneWay (String DateString) d (NodaTime.LocalDate.FromDateTime >> LocalDate >> Some)
            | :? OpenApiString as str ->
                match s with
                | String f ->
                    let inline extract (v: ParseResult<_>) = v.Value
                    let inline oneWay kind fn = oneWay (String f) str (fn >> kind >> Some)
                    let inline oneWayP kind (pattern: ^p) =
                        let inline parse v = (^p:(member Parse : string -> ParseResult<_>) pattern, v)
                        oneWay kind (parse >> extract)
                    // use the same format strings as NodaTime serialization libraries define
                    match f with
                    | Custom "local-date"
                    | DateString -> oneWayP LocalDate LocalDatePattern.Iso
                    | DateTimeString -> {| Value = DateTimeOffset.Parse str.Value |} |> matchDateTime
                    | Custom "instant" -> oneWayP Instant InstantPattern.ExtendedIso
                    | Custom "time"
                    | Custom "local-time" -> oneWayP LocalTime LocalTimePattern.GeneralIso
                    | Custom "local-date-time" -> oneWayP LocalDateTime LocalDateTimePattern.ExtendedIso
                    | Custom "offset-date-time" -> oneWayP OffsetDateTime OffsetDateTimePattern.Rfc3339
                    | Custom "offset"
                    | Custom "time-offset" -> oneWayP Offset OffsetPattern.GeneralInvariant
                    | Custom "duration" -> oneWayP Duration DurationPattern.JsonRoundtrip
                    | Custom "period" -> oneWayP Period PeriodPattern.Roundtrip
                    | Custom "time-zone"
                    | Custom "date-time-zone" ->
                        let provider = NodaTime.DateTimeZoneProviders.Tzdb
                        oneWay DateTimeZone (fun zone -> provider.[zone])
                    | _ -> None
                | _ -> None
            | _ -> None
        
        let inline failIfInvalid validation = failIfInvalid "Default value" validation
        
        let defaultMatch () =    
            match value with
            | :? OpenApiInteger as int ->
                match s with
                | Int validation ->
                    do failIfInvalid validation int.Value
                    DefaultableKind.Integer int.Value
                | Long validation ->
                    let value = int64 int.Value
                    do failIfInvalid validation value
                    DefaultableKind.Long value 
                | _ -> fw "integer"
            | :? OpenApiLong as long ->
                match s with
                | Int validation ->
                    do failIfInvalid validation (int32 long.Value)
                    DefaultableKind.Long long.Value
                | Long validation ->
                    do failIfInvalid validation long.Value
                    DefaultableKind.Long long.Value
                | _ -> fw "int64"
            | :? OpenApiDouble as double ->
                match s with
                | Double validation ->
                    do failIfInvalid validation double.Value
                    DefaultableKind.Double double.Value
                | _ -> fw "double" 
            | :? OpenApiBoolean as bool -> oneWay Bool bool DefaultableKind.Boolean
            | :? OpenApiDateTime as dateTime -> oneWay (String DateTimeString) dateTime DefaultableKind.DateTime
            | :? OpenApiDate as date -> oneWay (String DateString) date DefaultableKind.Date
            | :? OpenApiPassword as pwd -> oneWay (String PasswordString) pwd DefaultableKind.String
            | :? OpenApiString as str ->
                match s with
                | String s ->
                    let inline oneWay x = oneWay (String s) x
                    match s with
                    | PasswordString -> oneWay str DefaultableKind.String
                    | StringFormat.String validation ->
                        do failIfInvalid validation str.Value
                        oneWay str DefaultableKind.String
                    | Custom "uri"
                    | Custom "uriref" -> oneWay {| Value = System.Uri str.Value |} DefaultableKind.Uri
                    | Custom "uuid"
                    | Custom "guid"
                    | Custom "uid" -> oneWay {| Value = Guid.Parse str.Value |} DefaultableKind.Guid
                    | _ -> oneWay str DefaultableKind.String
                | _ -> fw "string"
            | v -> failwith (sprintf "%A literals aren't supported for kind %A" v s)
        
        Some Configuration.value
        |> Option.filter (fun c -> c.UseNodaTime)
        |> Option.bind (fun _ -> nodaMatch())
        |> Option.map Noda
        |> Option.defaultWith defaultMatch

and NodaTimeDefaultableKind =
    | Instant of NodaTime.Instant
    | LocalDate of NodaTime.LocalDate
    | LocalTime of NodaTime.LocalTime
    | LocalDateTime of NodaTime.LocalDateTime
    | OffsetDateTime of NodaTime.OffsetDateTime
    | ZonedDateTime of NodaTime.ZonedDateTime
    | Offset of NodaTime.Offset
    | Duration of NodaTime.Duration
    | Period of NodaTime.Period
    | DateTimeZone of NodaTime.DateTimeZone
and DefaultableKind =
    | String of string
    | Integer of int
    | Long of int64
    | Date of DateTime
    | DateTime of DateTimeOffset
    | Double of double
    | Boolean of bool
    | Uri of Uri
    | Guid of Guid
    | Noda of NodaTimeDefaultableKind

/// IR for object schemas
and ObjectKind =
    { Name: string option
      Properties: (string * TypeKind * Option<DefaultableKind>) list
      Docs: Docs option }
    
    static member private OptionIfNotRequired isRequired kind =
        if isRequired then kind
        else
            match kind with
            | TypeKind.Option v -> TypeKind.Option v
            | v -> TypeKind.Option v 
    
    static member Create(schema: OpenApiSchema) =
        let name =
            Option.ofObj schema.Reference
            |> Option.map (fun x -> x.Id)
            |> Option.orElseWith(fun _ -> Option.ofObj schema.Title)
        let fields =
            [
                for KeyValue(typeName, internalSchema) in schema.Properties ->
                    let internalType, def = TypeKind.Parse internalSchema
                    let internalType = ObjectKind.OptionIfNotRequired (schema.Required.Contains typeName || def.IsSome) internalType
                    typeName, internalType, def
            ]
        
        { Name = name
          Properties = fields
          Docs = Docs.Create(schema.Description, null, schema.Example) }
        
    static member Create(name: string, parameters: OpenApiParameter seq) =
        let fields =
            [ for param in parameters ->
                let kind, def = TypeKind.Parse param.Schema
                let kind = ObjectKind.OptionIfNotRequired (param.Required || def.IsSome) kind
                param.Name, kind, def ]
            
        { Name = Some name
          Properties = fields
          Docs = None }
and DUCaseKind =
    {
        Docs: Docs option
        CaseName: string option
        Kind: TypeKind
    }
and DUKind =
    {
        Name: string
        Cases: DUCaseKind list
        Docs: Docs option
    }
/// Kind of types for fields in schemas
/// Either primitive or Array<T> or Option<T> or Object with properties or DU
/// Array, Option and Object could recursively contains similar types
and TypeKind =
    | Prim of PrimTypeKind
    | Array of TypeKind * Option<DefaultableKind> * Option<ArrayValidation>
    | Option of TypeKind
    | Object of ObjectKind
    | DU of DUKind
    | BuiltIn of string
    | NoType
    static member Parse (schema: OpenApiSchema): TypeKind * DefaultableKind option =
        if isNull schema then Prim PrimTypeKind.Any, None
        else
            let kind, def =
                match schema with
                | ArrayKind items ->
                    let arrItm, def = TypeKind.Parse items
                    let isObj = match arrItm with | TypeKind.Object _ -> true | _ -> false 
                    if def.IsSome && isObj then
                        failwith "Default values aren't supported for entire objects"
                    Array (arrItm, def, ArrayValidation.TryParse schema), None
                | ObjectKind schema ->
                    if schema.Default <> null then
                        failwith "Default values aren't supported for entire objects"
                    else
                        ObjectKind.Create(schema) |> Object, None
                | PrimKind primType ->
                    let prim = PrimTypeKind.Parse(primType, schema)
                    let def = schema.Default |> Option.ofObj |> Option.map (fun x -> x:?>IOpenApiPrimitive) |> Option.map (prim.GetDefaultLiteral)
                    Prim prim, def
            if schema.Nullable && def.IsNone then
                (Option kind), None
            else kind, def

/// Representation of schema from OpenAPI
and TypeSchema =
    { Name: string
      Kind: TypeKind
      Docs: Docs option
      DefaultValue: DefaultableKind option }
    
    member private schema.DedupeTypeNames() =
        let obj =
            match schema.Kind with
            | TypeKind.Object o -> Some o
            | _ -> None
        let name =
            obj
            |> Option.bind (fun x -> x.Name)
            |> Option.defaultValue schema.Name
        let kind =
            obj
            |> Option.map (fun x -> { x with Name = Some name }) 
            |> Option.map TypeKind.Object
            |> Option.defaultValue schema.Kind
        {
            schema with
                Name = name
                Kind = kind
        }
    
    static member Parse(name, schema: OpenApiSchema): TypeSchema =
        let kind, def = TypeKind.Parse schema
        { Name = name
          Kind = kind
          DefaultValue = def
          Docs = Docs.Create(schema.Description, null, schema.Example) }.DedupeTypeNames()
        
    static member Parse(name, parameters: OpenApiParameter seq): TypeSchema =
        { Name = name
          Kind = TypeKind.Object (ObjectKind.Create (name, parameters))
          DefaultValue = None
          Docs = None }.DedupeTypeNames()

/// Supported response media types
type MediaType =
    | Json
    | Form
    | NotSpecified
    | Other of string

/// Typed response with corresponding HTTP code, media type and content type
type Response =
    { Code: int
      MediaType: MediaType
      Kind: TypeKind }

/// Source of binding
type PayloadNonBodyLocation =
    // TODO: Cookie (#35)
    | Path
    | Query
    // TODO Header (#35)
    with
        static member FromParameterLocation =
            function
            | ParameterLocation.Query -> Query
            | ParameterLocation.Path -> Path
            | _ -> Query

/// Endpoint call with name (should be unique across whole API at the moment) and method attached
type PathMethodCall =
    { Method: string
      Name: string
      Responses: Response list
      BodyParameters: (MediaType*TypeSchema) array option
      OtherParameters: Map<PayloadNonBodyLocation, TypeSchema> option
      AllParameters: TypeSchema array
      Docs: Docs option }

/// Representation of OpenApi path with methods attach
/// e.g.: "/v2/item" with GET and SET
type ParsedPath =
    { Route: string
      Methods: PathMethodCall list
      Docs: Docs option }

/// Representation of a whole spec
type Api =
    { Name: string
      Paths: ParsedPath list
      Schemas: TypeSchema list
      Docs: Docs option }

let reader = OpenApiStringReader()

/// read openApiSpec by filePath
/// returning tuple with parsed document and diagnostic errors
let read file =
    File.ReadAllText file |> reader.Read

let inline private parseCode x =
    let isSuccess, result = System.Int32.TryParse x
    if isSuccess then result
    else failwith "Other types of `responses` map key rather than INTEGER are not supported"

let inline private parseMediaType x =
    match trimLower x with
    | Some "application/json" -> Json
    | Some "application/x-www-form-urlencoded" -> MediaType.Form
    | Some x -> Other x
    | None -> failwith "Media type can't be null!"
    
let private parseResponses (operation: OpenApiOperation) =
    [ for KeyValue(code, response) in operation.Responses do
        for KeyValue(mediaType, content) in response.Content do
            yield { Code = parseCode code
                    MediaType = parseMediaType mediaType
                    Kind = TypeKind.Parse content.Schema |> fst } ]

let private normalizeName =
    let regex = Regex("[_\.-]([a-z])", RegexOptions.Compiled)
    let firstCharRegex = Regex("(^.)", RegexOptions.Compiled)
    fun str ->
        let s = regex.Replace(str, fun (m: Match) -> m.Groups.[1].Value.ToUpper())
        firstCharRegex.Replace(s, fun m -> m.Groups.[0].Value.ToUpper())

/// Parse OpenApi document into our representation of it
/// Returning API name and list of ParsedPaths
let parse (doc: OpenApiDocument): Api =

    let title = doc.Info.Title.Replace(" ", "")
    
    let schemas =
        [ if not (isNull doc.Components) then
            for KeyValue(typeName, schema) in doc.Components.Schemas ->
                TypeSchema.Parse(typeName, schema) ]

    let paths =
        [ for KeyValue(route, path) in doc.Paths do
            let methods =
                [ for KeyValue(method, op) in path.Operations do
                    
                    let methodName = normalizeName (string method)
                    let opName = normalizeName op.OperationId
                    
                    let pathParameters = Option.ofObj path.Parameters |> Option.map Seq.toArray
                    let operationParameters = Option.ofObj op.Parameters |> Option.map Seq.toArray
                    let allParameters =
                        Option.map2 Array.append pathParameters operationParameters
                        |> Option.orElse operationParameters
                        |> Option.orElse pathParameters
                    let nonBodyParameters =
                        allParameters
                        |> Option.filter (fun x -> x.Length > 0)
                        |> Option.map
                            (fun parameters ->
                                parameters
                                |> Seq.groupBy (fun x -> Option.ofNullable x.In |> Option.defaultValue ParameterLocation.Query |> PayloadNonBodyLocation.FromParameterLocation)
                                |> Seq.map (fun (location, parameters) ->
                                    location, TypeSchema.Parse(methodName + opName + location.ToString(), parameters))
                                |> Seq.toArray
                            )

                    let bodyParameters =
                        op.RequestBody
                        |> Option.ofObj
                        |> Option.map (fun rb ->
                            rb.Content
                            |> Seq.map (fun (KeyValue(mt, body)) ->
                                let mediaType = parseMediaType mt
                                let name = opName + methodName + "Body" + mediaType.ToString()
                                let schema = TypeSchema.Parse(name, body.Schema)
                                mediaType, schema)
                            |> Seq.toArray)
                        
                        
                    let responses =
                        [ for KeyValue(code, response) in op.Responses do
                            if response.Content.Count > 0 then
                                for KeyValue(mediaType, content) in response.Content do
                                    { Code = parseCode code
                                      MediaType = parseMediaType mediaType
                                      Kind = TypeKind.Parse content.Schema |> fst }
                            else
                                { Code = parseCode code
                                  MediaType = NotSpecified
                                  Kind = NoType } ]
                        
                    if responses.Length > 7 then
                        failwith "There could be only 7 or lower combinations of (mediaType * responseCode) per one path"
                    
                    { Method = methodName
                      Name = opName
                      Responses = responses
                      BodyParameters = bodyParameters
                      OtherParameters = nonBodyParameters |> Option.map Map
                      AllParameters =
                          let body = bodyParameters |> Option.map (Seq.map snd)
                          let nonBody = nonBodyParameters |> Option.map (Seq.map snd)
                          Option.map2 Seq.append body nonBody
                          |> Option.orElse body
                          |> Option.orElse nonBody
                          |> Option.map Seq.toArray
                          |> Option.defaultValue Array.empty
                      Docs = Docs.Create(op.Description, op.Summary, null) } ]
                
            yield { Route = route
                    Methods = methods
                    Docs = Docs.Create(path.Description, path.Summary, null) } ]
        
    { Name = title
      Paths = paths
      Schemas = schemas
      Docs = Docs.Create(doc.Info.Description, null, null)} 
