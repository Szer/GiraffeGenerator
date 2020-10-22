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
    let inline private checkMinBoundary value = checkBoundary (>) (sprintf "should be greater than %s") value
    let inline private checkMaxBoundary value = checkBoundary (<) (sprintf "should be lesser than %s") value
    
    let inline private checkMultiplyOf value divisor =
        if value % divisor = LanguagePrimitives.GenericZero then None
        else sprintf "should be a multiply of %d" divisor |> Some
    
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
                    s.MultipleOf |> Option.bind (checkMultiplyOf value)
                    s.Minimum |> Option.bind (checkMinBoundary value)
                    s.Maximum |> Option.bind (checkMaxBoundary value)
                    s.EnumValues |> Option.bind (checkEnum value)
                ]         
        static member TryParse (schema: OpenApiSchema): IntValidation option =
            {
                MultipleOf =
                    schema.MultipleOf
                    |> Option.ofNullable
                    |> Option.map int
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
                    s.MultipleOf |> Option.bind (checkMultiplyOf value)
                    s.Minimum |> Option.bind (checkMinBoundary value)
                    s.Maximum |> Option.bind (checkMaxBoundary value)
                    s.EnumValues |> Option.bind (checkEnum value)
                ]
        static member TryParse (schema: OpenApiSchema): LongValidation option =
            {
                MultipleOf =
                    schema.MultipleOf
                    |> Option.ofNullable
                    |> Option.map int64
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
            {
                MinLength = schema.MinLength |> Option.ofNullable
                MaxLength = schema.MaxLength |> Option.ofNullable
                Pattern = schema.Pattern |> Option.ofObj
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
            MinItems: int option
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
            {
                MinItems = schema.MinItems |> Option.ofNullable
                MaxItems = schema.MaxItems |> Option.ofNullable
                UniqueItems = schema.UniqueItems |> Option.ofNullable |> Option.defaultValue false
            }
            |> Some
            |> Option.filter ^ fun x -> x.MinItems.IsSome || x.MaxItems.IsSome || x.UniqueItems
    
    let inline failIfInvalid name validation value =
        validation
        |> Option.map(fun validation ->
            (^validation:(member FailIfInvalid: string -> ^a -> unit) validation, name, value))
        |> Option.defaultValue ()