module CodeGenValidation_Types

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
