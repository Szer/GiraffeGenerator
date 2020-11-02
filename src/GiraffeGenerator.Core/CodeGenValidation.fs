module CodeGenValidation

open OpenApi
open CodeGenValidation_Types
open CodeGenValidation_Types.Enumeration
open CodeGenValidation_TypeGeneration

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
    [
        for validationType in usedValidationTypes do
            let attrOption = generateAttributeDefinitionFor validationType
            if attrOption.IsSome then
                attrOption.Value
    ]
