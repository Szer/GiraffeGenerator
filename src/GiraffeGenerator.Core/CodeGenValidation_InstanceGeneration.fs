module CodeGenValidation_InstanceGeneration

open CodeGenValidation_Types
open CodeGenValidation_Types.Enumeration
open AST
open FSharp.Compiler.SyntaxTree

// get the name and argument expression list for the attribute passed
let getTypeMin (typeName: string) =
    let suffix =
        if typeName.EndsWith("Double") then
            ".NegativeInfinity"
        else ".MinValue"
    typeName + suffix |> longIdentExpr
let getTypeMax (typeName: string) =
    let suffix =
        if typeName.EndsWith("Double") then
            ".PositiveInfinity"
        else ".MaxValue"
    typeName + suffix |> longIdentExpr
let private rangeToExpressions typeName valueToConst r =
    let valueToExpr = valueToConst >> constExpr
    match r with
    | Min min -> [valueToExpr min; getTypeMax typeName]
    | Max max -> [getTypeMin typeName; valueToExpr max]
    | Both (min, max) -> [valueToExpr min; valueToExpr max]
let private getAttributeUsageDefinitionForBuiltInAttribute =
    function
    | Required -> "Required", []
    | FloatRange r -> "Range", rangeToExpressions "System.Double" SynConst.Double r
    | IntRange r -> "Range", rangeToExpressions "System.Int32" SynConst.Int32 r
    | MaxLength len -> "MaxLength", [intExpr len]
    | MinLength len -> "MinLength", [intExpr len]
let private getEnumAttributeUsageDefinition =
    let convertArgs toConst values =
        values
        |> Seq.map toConst
        |> Seq.map constExpr
        |> Seq.toList
    function
    | IntEnum values -> "IntEnumValues", convertArgs SynConst.Int32 values
    | LongEnum values -> "LongEnumValues", convertArgs SynConst.Int64 values
    | FloatEnum values -> "FloatEnumValues", convertArgs SynConst.Double values
    | StringEnum values -> "StringEnumValues", convertArgs (fun v -> SynConst.String(v, r)) values
let private getMultipleOfAttributeUsageDefinition =
    let convertArg toConst value =
        [toConst value |> constExpr]
    function
    | IntMultiple divisor -> "IntMultipleOf", convertArg SynConst.Int32 divisor
    | LongMultiple divisor -> "LongMultipleOf", convertArg SynConst.Int64 divisor
let private getAttributeUsageDefinitionForSpecialCasedAttribute expr =
    function
    | BuiltInAttribute _
    | CustomAttribute _ -> None
    | SpecialCasedCustomValidationAttribute specialCased ->
        match specialCased with
        | UniqueItems ->
            let exprLength =
                SynExpr.DotGet(expr, r, longIdentWithDots "Length", r)
            let set =
                app (identExpr "Set") (expr) |> paren
            let setCount =
                SynExpr.DotGet(set, r, longIdentWithDots "Count", r)
            Some ("UniqueItems", [exprLength ^= setCount])
let private getAttributeUsageDefinitionForCustomAttribute =
    function
    | LongRange r -> "LongRange", rangeToExpressions "System.Int64" SynConst.Int64 r
    | RegexPattern pattern -> "RegexPattern", [strExpr pattern]
    | EnumAttribute enum -> getEnumAttributeUsageDefinition enum
    | MultipleOf target -> getMultipleOfAttributeUsageDefinition target
let rec private getAttributeUsageDefinitionForAttribute attr =
    match attr with
    | BuiltInAttribute builtIn -> getAttributeUsageDefinitionForBuiltInAttribute builtIn |> Some
    | CustomAttribute custom -> getAttributeUsageDefinitionForCustomAttribute custom |> Some
    | SpecialCasedCustomValidationAttribute _ -> None

/// generates an array of isSpecialCased * array expr of attributeTypeName(attributeConstructorParameters) for a kind
let getValidationAttributeConstructionForKind expr kind =
    [
        for attr in enumerateKindValidation false kind do
            let commonDefinition =
                getAttributeUsageDefinitionForAttribute attr
                |> Option.map (fun x -> false, x)
            let specialCasedDefinition =
                getAttributeUsageDefinitionForSpecialCasedAttribute expr attr
                |> Option.map (fun x -> true, x)
            let definition = Option.orElse specialCasedDefinition commonDefinition
            
            if definition.IsSome then
                let isSpecialCased, (attrName, args) = definition.Value
                let arg = tupleComplexExpr args
                isSpecialCased, app (identExpr (attrName + "Attribute")) arg
    ]
    |> List.groupBy fst
    |> List.choose ^ fun (isSpecialCased, expressions) ->
        let attrConstruction = List.map snd expressions
        if attrConstruction.IsEmpty then None
        else (isSpecialCased, SynExpr.ArrayOrList(true, attrConstruction, r)) |> Some

let getValidationAttributesForProperty kind =
    [
        for attr in enumeratePropertyValidation false kind do
            let def = getAttributeUsageDefinitionForAttribute attr
            if def.IsSome then
                let attrName, args = def.Value 
                attrComplex attrName (if args.IsEmpty then unitExpr else tupleComplexExpr args)
    ]
