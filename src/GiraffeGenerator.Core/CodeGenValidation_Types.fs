module CodeGenValidation_Types

open OpenApi
open OpenApiValidation

// convert OpenApi representation to something more useful for codegen

type EnumTargets =
    | IntEnum of int array
    | LongEnum of int64 array
    | FloatEnum of float array
    | StringEnum of string array
    with
    member s.TypeEquals v =
        match s,v with
        | IntEnum _, IntEnum _
        | LongEnum _, LongEnum _
        | FloatEnum _, FloatEnum _
        | StringEnum _, StringEnum _ -> true
        | _ -> false

type MultipleOfTargets =
    | IntMultiple of int
    | LongMultiple of int64
    with
    member s.TypeEquals v =
        match s,v with
        | IntMultiple _, IntMultiple _
        | LongMultiple _, LongMultiple _ -> true
        | _ -> false

type RangeValues<'a> =
    | Min of 'a
    | Max of 'a
    | Both of 'a*'a

let inline private toExclusive operator (range: ^v NumberBoundary) =
    if not range.Exclusive then range.Value
    elif not (box range.Value :? float) then operator range.Value LanguagePrimitives.GenericOne
    else ((box operator :?> float -> float -> float) ((box range.Value) :?> float) 1e-8 (*Fabulous doesn't generate values with a really small exponents*) |> box) :?> ^v

let inline private rangeValuesFromRanges a b =
    let a = a |> Option.map (toExclusive (+))
    let b = b |> Option.map (toExclusive (-))
    Option.map2 (fun a b -> Both (a, b)) a b
    |> Option.orElse (a |> Option.map Min)
    |> Option.orElse (b |> Option.map Max)

type BuiltInAttribute =
    | IntRange of int RangeValues
    | FloatRange of float RangeValues
    | MinLength of int
    | MaxLength of int
    | Required
    with
    member s.TypeEquals v =
        match s,v with
        | IntRange _, IntRange _
        | FloatRange _, FloatRange _
        | MinLength _, MinLength _
        | MaxLength _, MaxLength _
        | Required, Required -> true
        | _ -> false

/// as we've got no way to define attribute which depends on the type to which it's applied
/// without code-generating a reflection
/// this attribute should be special-cased to be created with validity as parameter
type SpecialCasedCustomValidationAttribute =
    | UniqueItems
    with
    member s.TypeEquals v =
        match s,v with
        | UniqueItems, UniqueItems -> true

type CustomBasicValidationAttribute =
    | EnumAttribute of EnumTargets
    | MultipleOf of MultipleOfTargets
    | LongRange of int64 RangeValues
    // built-in doesn't work for some reason
    | RegexPattern of string
    with
    member s.TypeEquals v =
        match s,v with
        | EnumAttribute _, EnumAttribute _
        | MultipleOf _, MultipleOf _
        | RegexPattern _, RegexPattern _
        | LongRange _, LongRange _
        | _ -> false

type ValidationAttribute =
    | CustomAttribute of CustomBasicValidationAttribute
    | BuiltInAttribute of BuiltInAttribute
    | SpecialCasedCustomValidationAttribute of SpecialCasedCustomValidationAttribute
    // no type recursion for arrays and options
    // because they should be handled via
    // recursive validator function
    // instead of generating a stuff like
    // ArrayItemOptionValueArrayItemMinLengthAttribute
    with
    member s.TypeEquals v =
        match s,v with
        | CustomAttribute a, CustomAttribute b -> a.TypeEquals b
        | BuiltInAttribute a, BuiltInAttribute b -> a.TypeEquals b
        | SpecialCasedCustomValidationAttribute a, SpecialCasedCustomValidationAttribute b -> a.TypeEquals b
        | _ -> false

module rec Enumeration =
    let enumeratePropertyValidation recurse kind =
        seq {
            let isRequired = match kind with | TypeKind.Option _ -> false | _ -> true
            if isRequired then
                Required |> BuiltInAttribute
            yield! enumerateKindValidation recurse kind
        }

    /// Enumerates every used attribute for a kind (including attribute parameters).
    /// Includes a flag parameter controlling should the enumeration be recursive.
    /// Arrays yield only the validation for arrays themselves (like MinLength/MaxLength).
    /// Arrays, Options and Objects do not yield theirs underlying value validation by themselves.
    /// Underlying values validation is still yielded if recurse: true
    let rec enumerateKindValidation recurse kind =
        seq {
            match kind with
            | TypeKind.Array (kind,_,validation) ->
                if validation.IsSome then
                    let validation = validation.Value
                    if validation.UniqueItems then
                        UniqueItems |> SpecialCasedCustomValidationAttribute |> Some
                    validation.MinItems |> Option.map (MinLength >> BuiltInAttribute)
                    validation.MaxItems |> Option.map (MaxLength >> BuiltInAttribute)
                if recurse then
                    for child in enumerateKindValidation recurse kind do
                        Some child
            | TypeKind.Option kind ->
                if recurse then
                    for child in enumerateKindValidation recurse kind do
                        Some child
            | TypeKind.Object obj ->
                if recurse then
                    for (_, propertyKind, _) in obj.Properties do
                        for child in enumeratePropertyValidation recurse propertyKind do
                            Some child
            | TypeKind.DU _ -> failwith "OneOf validation is not supported yet"
            | TypeKind.Prim prim ->
                match prim with
                | PrimTypeKind.Int (Some validation) ->
                    validation.MultipleOf |> Option.map (IntMultiple >> MultipleOf >> CustomAttribute)
                    validation.EnumValues |> Option.map (IntEnum >> EnumAttribute >> CustomAttribute)
                    rangeValuesFromRanges validation.Minimum validation.Maximum
                    |> Option.map (IntRange >> BuiltInAttribute)
                | PrimTypeKind.Long (Some validation) ->
                    validation.MultipleOf |> Option.map (LongMultiple >> MultipleOf >> CustomAttribute)
                    validation.EnumValues |> Option.map (LongEnum >> EnumAttribute >> CustomAttribute)
                    rangeValuesFromRanges validation.Minimum validation.Maximum
                    |> Option.map (LongRange >> CustomAttribute)
                | PrimTypeKind.Double (Some validation) ->
                    validation.EnumValues |> Option.map (FloatEnum >> EnumAttribute >> CustomAttribute)
                    rangeValuesFromRanges validation.Minimum validation.Maximum
                    |> Option.map (FloatRange >> BuiltInAttribute)
                | PrimTypeKind.String (StringFormat.String (Some validation)) ->
                    validation.MinLength |> Option.map (MinLength >> BuiltInAttribute)
                    validation.MaxLength |> Option.map (MaxLength >> BuiltInAttribute)
                    validation.EnumValues |> Option.map (StringEnum >> EnumAttribute >> CustomAttribute)
                    validation.Pattern |> Option.map (RegexPattern >> CustomAttribute)
                | _ -> ()
            | TypeKind.NoType
            | TypeKind.BuiltIn _ -> ()
        }
        |> Seq.choose id

/// gets a unique identifier of a custom attribute type ignoring any parameters for a type's instance
/// basically converts DU case value to a bitflag
/// 0 indicates a non-custom type which needs no generation
let rec identifyCustomValidationAttributeType value =
    match value with
    | BuiltInAttribute _ -> 0
    | SpecialCasedCustomValidationAttribute specialCased ->
        match specialCased with
        | UniqueItems -> 1
    | CustomAttribute custom ->
        match custom with
        | LongRange _ -> 2
        | RegexPattern _ -> 4
        | MultipleOf target ->
            match target with
            | IntMultiple _ -> 8
            | LongMultiple _ -> 16
        | EnumAttribute enum ->
            match enum with
            | IntEnum _ -> 32
            | LongEnum _ -> 64
            | FloatEnum _ -> 128
            | StringEnum _ -> 256
