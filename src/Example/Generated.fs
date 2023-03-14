///<summary>
///This is very simple API
///</summary>
[<RequireQualifiedAccess>]
module SimpleAPIoverview

open System.ComponentModel.DataAnnotations
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

///<summary>replaces any generated validation rules for type</summary>
[<Interface>]
type 'model IGiraffeValidator =
    abstract Validate: ('model * ValidationContext) -> ValidationResult array

///<summary>
///This is apis
///</summary>
[<CLIMutable>]
type apis =
    { apiKey: string option
      apiVersionNumber: string option
      apiUrl: System.Uri option
      apiCount: int64 option
      apiAvg: double option
      isInternal: bool option
      start: System.DateTime option
      apiHash: byte array option }

///<summary>
///This is data set list
///</summary>
and [<CLIMutable>] dataSetList =
    { total: int option
      apis: apis array option }

///<summary>
///Input binding error
///</summary>
and ArgumentError = ///<summary>
    ///Bound argument is not valid
    ///</summary>
    ArgumentValidationError of ValidationResult array

///<summary>
///Location argument error
///</summary>
and ArgumentLocationedError =
    ///<summary>
    ///Body error
    ///</summary>
    | BodyBindingError of ArgumentError
    ///<summary>
    ///Query error
    ///</summary>
    | QueryBindingError of ArgumentError
    ///<summary>
    ///Path error
    ///</summary>
    | PathBindingError of ArgumentError
    ///<summary>
    ///Multiple locations errors
    ///</summary>
    | CombinedArgumentLocationError of ArgumentLocationedError array

let rec argErrorToString level value =
    let sep = System.String(' ', level * 2)
    match value with
    | ArgumentValidationError err ->
        let errStrings =
            Option.ofObj err
            |> Option.defaultValue Array.empty
            |> Array.map (fun v ->
                let path =
                    Option.ofObj v.MemberNames |> Option.map (String.concat ".")

                let error = Option.ofObj v.ErrorMessage
                Option.map2 (sprintf "%s (at %s)") error path
                |> Option.orElse error
                |> Option.orElse path
                |> Option.defaultValue "unknown validation error")

        if errStrings |> Array.length = 0 then
            sprintf "%sUnknown validation error" sep
        else if errStrings |> Array.length = 1 then
            errStrings
            |> Array.head
            |> sprintf "%sValidation error: %s" sep
        else
            let sepInner = sprintf "\n%s " sep
            errStrings
            |> String.concat sepInner
            |> sprintf "%sValidation errors:%s%s" sep sepInner

let rec argLocationErrorToString level value =
    let sep = System.String(' ', level * 2)
    match value with
    | BodyBindingError body -> sprintf "%sBody binding error:\n%s" sep (argErrorToString (level + 1) body)
    | PathBindingError path -> sprintf "%sPath binding error:\n%s" sep (argErrorToString (level + 1) path)
    | QueryBindingError query -> sprintf "%sQuery binding error:\n%s" sep (argErrorToString (level + 1) query)
    | CombinedArgumentLocationError err ->
        sprintf
            "%sMultiple binding errors:\n%s"
            sep
            (String.concat "\n\n" (Seq.map (argLocationErrorToString (level + 1)) err))

let tryExtractError value =
    match value with
    | Ok _ -> None
    | Error err -> Some err

let isObjectValid boxed errors validationContext =
    Validator.TryValidateObject(boxed, validationContext, errors, true)

let isValueValid validationAttributes boxed errors validationContext =
    Validator.TryValidateValue(boxed, validationContext, errors, validationAttributes)

let validateInner isValid (ctx: HttpContext) validationContext (value: 'model) =
    let customValidator =
        ctx.RequestServices.GetService<IGiraffeValidator<'model>>()

    let errs =
        if System.Object.ReferenceEquals(customValidator, null) then
            let errs = System.Collections.Generic.List()
            if isValid value errs validationContext then Array.empty else errs |> Seq.toArray
        else
            customValidator.Validate(value, validationContext)

    errs

let withValue (validationContext: ValidationContext) value =
    let ctx =
        ValidationContext(value, validationContext.Items)

    ctx.InitializeServiceProvider(fun t -> validationContext.GetService t)
    ctx.MemberName <- null
    ctx


let withMemberAndValue (validationContext: ValidationContext) name value =
    let ctx = withValue validationContext value
    ctx.MemberName <- name
    ctx


let rec validate ctx (validationContext: ValidationContext) =
    let instance = validationContext.ObjectInstance
    [| match instance with
       | v -> failwithf "Unknown type came to validation: %A" (v.GetType()) |]

let bindValidation (ctx: HttpContext) location (value: 'model) =
    let validationContext =
        ValidationContext(value, ctx.RequestServices, ctx.Items)

    let errs = validate ctx validationContext
    if (errs |> Array.length) = 0 then
        Ok value
    else
        errs
        |> ArgumentValidationError
        |> location
        |> Error

[<AbstractClass>]
type Service() =
    ///<summary>
    ///This is very cool API for list API versions
    ///</summary>
    ///<remarks>
    ///List API versions
    ///</remarks>
    abstract ListVersionsv2: HttpHandler

    override this.ListVersionsv2 =
        fun next ctx ->
            task {
                let! logicOutput = this.ListVersionsv2Input ctx
                return! this.ListVersionsv2Output logicOutput next ctx
            }

    abstract ListVersionsv2Input: HttpContext -> Task<Choice<dataSetList, bool>>
    abstract ListVersionsv2Output: Choice<dataSetList, bool> -> HttpHandler

    override this.ListVersionsv2Output input =
        match input with
        | Choice1Of2 responseOn200 -> json responseOn200
        | Choice2Of2 responseOn300 -> setStatusCode 300 >=> json responseOn300

    ///<remarks>
    ///List API version details
    ///</remarks>
    abstract GetVersionDetailsv2: HttpHandler

    override this.GetVersionDetailsv2 =
        fun next ctx ->
            task {
                let! logicOutput = this.GetVersionDetailsv2Input ctx
                return! this.GetVersionDetailsv2Output logicOutput next ctx
            }

    abstract GetVersionDetailsv2Input: HttpContext -> Task<Choice<{| subscriptionId: string option |}, bool>>
    abstract GetVersionDetailsv2Output: Choice<{| subscriptionId: string option |}, bool> -> HttpHandler

    override this.GetVersionDetailsv2Output input =
        match input with
        | Choice1Of2 responseOn200 -> json responseOn200
        | Choice2Of2 responseOn203 -> setStatusCode 203 >=> json responseOn203

let webApp: HttpHandler =
    fun next ctx ->
        task {
            let service = ctx.GetService<Service>()
            return! choose
                        [ (GET >=> route "/" >=> service.ListVersionsv2)
                          (GET
                           >=> route "/v2"
                           >=> service.GetVersionDetailsv2)
                           ]
                        next
                        ctx
        }
