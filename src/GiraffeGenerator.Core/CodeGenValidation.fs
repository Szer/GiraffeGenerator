module CodeGenValidation

open OpenApi
open CodeGenValidation_Types
open CodeGenValidation_Types.Enumeration
open CodeGenValidation_TypeGeneration
open CodeGenValidation_TypeGeneration.ExtensionPoints
open CodeGenValidation_InstanceGeneration
open CodeGenValidation_LogicGeneration

/// gets all validation attributes to be used in API
let private decideWhichValidationIsBeingUsed api =
    seq {
        for path in api.Paths do
            for method in path.Methods do
                for parameter in method.AllParameters do
                    yield! enumerateKindValidation true parameter.Kind 
    }
    
let generateModuleLevelDeclarations api =
    let usedValidation = decideWhichValidationIsBeingUsed api |> Seq.toArray
    let usedValidationTypes =
        usedValidation
        |> Seq.distinctBy identifyCustomValidationAttributeType
        |> Seq.sortBy string
        |> Seq.toArray
    let hasGeneratedValidationRules = usedValidationTypes.Length > 0
    [
        for validationType in usedValidationTypes do
            let attrOption = generateAttributeDefinitionFor validationType
            if attrOption.IsSome then
                attrOption.Value
        generateValidationReplacerInterface()
        if hasGeneratedValidationRules then
            generateValidationAugmenterInterface()
    ], [
        let replacerInterface = validationReplacerInterface
        let augmenterInterface =
            Some validationAugmenterInterface
            |> Option.filter (fun _ -> hasGeneratedValidationRules)
        yield! generateValidationBinder api replacerInterface augmenterInterface
    ]


/// Applies validation to the resultExpr:
/// {resultExpr} |> Result.bind (bindValidation isObjectValid ctx {location})
/// for objects and DUs or
/// {resultExpr} |> Result.bind (bindValidation (isValueValid [|{attribute instances for this value}|]) ctx {location})
/// for any other kind of value
let bindValidationIntoResult = bindValidationIntoResult

let getValidationAttributesForProperty = getValidationAttributesForProperty