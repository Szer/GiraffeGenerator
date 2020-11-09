module OpenApiValidation
    open System.Text.RegularExpressions
    open AST
    open Microsoft.OpenApi.Any
    open Microsoft.OpenApi.Models

    type NumberBoundary<'n> =
        {
            Value: 'n
            Exclusive: bool
        }
    // the following block contains functions used for design-time values (e.g. defaults) validation
    
    // numerics validation
    let private checkBoundary op message value boundary =
        let isValid = not boundary.Exclusive && boundary.Value = value || op boundary.Value value
        if isValid then None
        else
            let subMessage = sprintf "%s %A" (if not boundary.Exclusive then "or equal to" else "") boundary.Value
            message subMessage |> Some
    let inline private checkMinBoundary value = checkBoundary (<) (sprintf "should be greater than %s") value
    let inline private checkMaxBoundary value = checkBoundary (>) (sprintf "should be lesser than %s") value
    
    let inline private checkMultipleOf value divisor =
        if value % divisor = LanguagePrimitives.GenericZero then None
        else sprintf "should be a multiple of %d" divisor |> Some
    
    // numerics or string validation
    
    let inline private checkEnum value enum =
        if enum |> Array.contains value then None
        else
            enum
            |> Array.map string
            |> String.concat "; "
            |> sprintf "should be one of [%s]"
            |> Some
    
    // array or string length validation
    let inline private getLength value = (^v: (member Length: int) value)
    let inline private checkLength boundaryChecker value measure length =
        {
            Value = length
            Exclusive = false
        }
        |> boundaryChecker (getLength value)
        |> Option.map ^ sprintf "%s count %s" measure
    let inline private checkMinLength value = checkLength checkMinBoundary value
    let inline private checkMaxLength value = checkLength checkMaxBoundary value

    let inline private failIfInvalidImpl name value errs =
        let errs =
            errs
            |> List.choose id
            |> String.concat "; "
        if errs.Length > 0 then
            failwithf "%s %A is invalid: %s" name value errs       
    
    // utilities for validation rules validation
    
    let inline private failIfMultipleOfInvalidOrReturnIt (schema: OpenApiSchema) converter =
        let multipleOf =
            schema.MultipleOf
            |> Option.ofNullable
            |> Option.map converter
            |> Option.filter ((<>) LanguagePrimitives.GenericOne)
        do
            multipleOf
            |> Option.filter ((=) LanguagePrimitives.GenericZero)
            |> Option.map (failwithf "multipleOf must not be equal to %d")
            |> Option.defaultValue ()
        multipleOf
    
    let validateLengths min max =
        do
            min
            |> Option.filter ((>) 0)
            |> Option.map (failwithf "minItems must be greater than or equal to 0. %d is not")
            |> Option.defaultValue ()
        do
            max
            |> Option.filter ((>) 0)
            |> Option.map (failwithf "maxItems must be greater than or equal to 0. %d is not")
            |> Option.defaultValue ()
        do
            Option.map2 (fun min max -> min, max) min max
            |> Option.filter (fun (min, max) -> min > max)
            |> Option.map (fun (min, max) -> failwithf "maxItems (%d) should be greater than minItems (%d)" max min)
            |> Option.defaultValue ()
    
    // and here come the validation rule models themselves
    
    type IntValidation =
        {
            MultipleOf: int option
            Minimum: NumberBoundary<int> option
            Maximum: NumberBoundary<int> option
            EnumValues: int array option
        }
        with
        member s.FailIfInvalid (name, value) =
            failIfInvalidImpl name value
                [
                    s.MultipleOf |> Option.bind (checkMultipleOf value)
                    s.Minimum |> Option.bind (checkMinBoundary value)
                    s.Maximum |> Option.bind (checkMaxBoundary value)
                    s.EnumValues |> Option.bind (checkEnum value)
                ]         
        static member TryParse (schema: OpenApiSchema): IntValidation option =
            let multipleOf = failIfMultipleOfInvalidOrReturnIt schema int
            {
                MultipleOf = multipleOf
                Minimum =
                    schema.Minimum
                    |> Option.ofNullable
                    |> Option.map int
                    |> Option.map ^ fun v ->
                        { Value = v; Exclusive = schema.ExclusiveMinimum |> Option.ofNullable |> Option.defaultValue false }
                Maximum =
                    schema.Maximum
                    |> Option.ofNullable
                    |> Option.map int
                    |> Option.map ^ fun v ->
                        { Value = v; Exclusive = schema.ExclusiveMaximum |> Option.ofNullable |> Option.defaultValue false }
                EnumValues =
                    schema.Enum
                    |> Option.ofObj
                    |> Option.map (Seq.map (fun x -> (x :?> OpenApiInteger).Value) >> Seq.toArray)
                    |> Option.filter (Array.length >> (<>) 0)
            }
            |> Some
            |> Option.filter ^ fun x -> x.MultipleOf.IsSome || x.Minimum.IsSome || x.Maximum.IsSome || x.EnumValues.IsSome
                
    type LongValidation =
        {
            MultipleOf: int64 option
            Minimum: NumberBoundary<int64> option
            Maximum: NumberBoundary<int64> option
            EnumValues: int64 array option
        }
        with
        member s.FailIfInvalid (name, value) =
            failIfInvalidImpl name value
                [
                    s.MultipleOf |> Option.bind (checkMultipleOf value)
                    s.Minimum |> Option.bind (checkMinBoundary value)
                    s.Maximum |> Option.bind (checkMaxBoundary value)
                    s.EnumValues |> Option.bind (checkEnum value)
                ]
        static member TryParse (schema: OpenApiSchema): LongValidation option =
            let multipleOf = failIfMultipleOfInvalidOrReturnIt schema int64
            {
                MultipleOf = multipleOf
                Minimum =
                    schema.Minimum
                    |> Option.ofNullable
                    |> Option.map int64
                    |> Option.map ^ fun v ->
                        { Value = v; Exclusive = schema.ExclusiveMinimum |> Option.ofNullable |> Option.defaultValue false }
                Maximum =
                    schema.Maximum
                    |> Option.ofNullable
                    |> Option.map int64
                    |> Option.map ^ fun v ->
                        { Value = v; Exclusive = schema.ExclusiveMaximum |> Option.ofNullable |> Option.defaultValue false }
                EnumValues =
                    schema.Enum
                    |> Option.ofObj
                    |> Option.map (Seq.map (fun x -> (x :?> OpenApiLong).Value) >> Seq.toArray)
                    |> Option.filter (Array.length >> (<>) 0)
            }
            |> Some
            |> Option.filter ^ fun x -> x.MultipleOf.IsSome || x.Minimum.IsSome || x.Maximum.IsSome || x.EnumValues.IsSome

    type FloatValidation =
        {
            Minimum: NumberBoundary<float> option
            Maximum: NumberBoundary<float> option
            EnumValues: float array option
        }
        with
        member s.FailIfInvalid (name, value) =
            failIfInvalidImpl name value
                [
                    s.Minimum |> Option.bind (checkMinBoundary value)
                    s.Maximum |> Option.bind (checkMaxBoundary value)
                    s.EnumValues |> Option.bind (checkEnum value)
                ]
        static member TryParse (schema: OpenApiSchema): FloatValidation option =
            {
                Minimum =
                    schema.Minimum
                    |> Option.ofNullable
                    |> Option.map float
                    |> Option.map ^ fun v ->
                        { Value = v; Exclusive = schema.ExclusiveMinimum |> Option.ofNullable |> Option.defaultValue false }
                Maximum =
                    schema.Maximum
                    |> Option.ofNullable
                    |> Option.map float
                    |> Option.map ^ fun v ->
                        { Value = v; Exclusive = schema.ExclusiveMaximum |> Option.ofNullable |> Option.defaultValue false }
                EnumValues =
                    schema.Enum
                    |> Option.ofObj
                    |> Option.map (Seq.map (fun x -> (x :?> OpenApiDouble).Value) >> Seq.toArray)
                    |> Option.filter (Array.length >> (<>) 0)
            }
            |> Some
            |> Option.filter ^ fun x -> x.Minimum.IsSome || x.Maximum.IsSome || x.EnumValues.IsSome
                
    type StringValidation =
        {
            /// inclusive: https://tools.ietf.org/html/draft-wright-json-schema-validation-00#section-5.7
            MinLength: int option
            /// inclusive: https://tools.ietf.org/html/draft-wright-json-schema-validation-00#section-5.6
            MaxLength: int option
            Pattern: string option
            EnumValues: string array option
        }
        with
        member s.FailIfInvalid (name, value) =
            failIfInvalidImpl name value
                [
                    s.MinLength |> Option.bind (checkMinLength value "characters")
                    s.MaxLength |> Option.bind (checkMaxLength value "characters")
                    s.EnumValues |> Option.bind (checkEnum value)
                    s.Pattern
                    |> Option.filter (fun pattern -> Regex(pattern, RegexOptions.ECMAScript).IsMatch value)
                    |> Option.map ^ sprintf "should match pattern /%s/"
                ]
        static member TryParse (schema: OpenApiSchema): StringValidation option =
            let minLength = schema.MinLength |> Option.ofNullable
            let maxLength = schema.MaxLength |> Option.ofNullable
            let pattern = schema.Pattern |> Option.ofObj
            do validateLengths minLength maxLength
            do
                pattern
                |> Option.filter (fun pattern ->
                    try
                        do Regex(pattern, RegexOptions.ECMAScript) |> ignore
                        false
                    with | _ -> true)
                |> Option.map (failwithf "pattern /%s/ should be a valid ECMA regexp")
                |> Option.defaultValue ()
            {
                MinLength = minLength
                MaxLength = maxLength
                Pattern = pattern
                EnumValues =
                    schema.Enum
                    |> Option.ofObj
                    |> Option.map (Seq.map (fun x -> (x :?> OpenApiString).Value) >> Seq.toArray)
                    |> Option.filter (Array.length >> (<>) 0)
            }
            |> Some
            |> Option.filter ^ fun x -> x.MinLength.IsSome || x.MaxLength.IsSome || x.Pattern.IsSome || x.EnumValues.IsSome
                
    type ArrayValidation =
        {
            /// inclusive: https://tools.ietf.org/html/draft-wright-json-schema-validation-00#section-5.11
            MinItems: int option
            /// inclusive: https://tools.ietf.org/html/draft-wright-json-schema-validation-00#section-5.10
            MaxItems: int option
            UniqueItems: bool
        }
        with
        member s.FailIfInvalid (name, value) =
            failIfInvalidImpl name value
                [
                    s.MinItems |> Option.bind (checkMinLength value "items")
                    s.MaxItems |> Option.bind (checkMaxLength value "items")
                    if s.UniqueItems && Array.length value <> (Set value).Count then
                        Some "should contain only unique values"
                ]
        static member TryParse (schema: OpenApiSchema): ArrayValidation option =
            let minItems = schema.MinItems |> Option.ofNullable
            let maxItems = schema.MaxItems |> Option.ofNullable
            do validateLengths minItems maxItems
            {
                MinItems = minItems
                MaxItems = maxItems
                UniqueItems = schema.UniqueItems |> Option.ofNullable |> Option.defaultValue false
            }
            |> Some
            |> Option.filter ^ fun x -> x.MinItems.IsSome || x.MaxItems.IsSome || x.UniqueItems
    
    let inline failIfInvalid name validation value =
        validation
        |> Option.map(fun validation ->
            (^validation:(member FailIfInvalid: string -> ^a -> unit) validation, name, value))
        |> Option.defaultValue ()