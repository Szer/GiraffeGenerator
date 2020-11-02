/// # Design notes
/// We decided to use built-in DataAnnotations validation. This applies several restrictions:
/// 1. Attributes may only be applied to properties on type level.
///    This means no straightforward way to validate array items and option values (i.e. values inside a monad).
///    Considered solutions:
/// 
///    I. Generation of attributes like ArrayItemRegexPatternAttribute
///    Rejected because of really messy codegen for nested monads:
///    each combination of nesting and rule would require a separate attribute like
///    OptionValueArrayItemArrayItemMinLengthAttribute and OptionValueArrayItemArrayItemRegexPatternAttribute.
///
///    II. Generation of an attribute to treat some of monadic property validation attributes as a monad value attributes
///    Rejected because:
///    1. DataAnnotations.Validator provides no way for an attribute to observe another attributes.
///       May be worked around with reflection, but generation of reflection at design-time seems extremely ugly
///    2. DataAnnotations.Validator provides no way for an attribute to interfere the validation process,
///       e.g. we can't prevent it from trying to apply the attributes for monad's value to the monad itself.
///       This may be worked around with generation of the special attributes but this would fail for the same reasons as III.
/// 
///    III. Generation of non deeply nested validation only
///    E.g. generate OptionValueRegexPattern but throw instead of generating OptionValueArrayItemRegexPatternAttribute.
///    Failed because
///    1. We need to know the type parameter of a monad to perform validation.
///    2. The most realistic case for monad value validation is a validation of some user type.
///       This means that attribute type definition depends on the types to which attribute is applied.
///       F# won't compile such recursion for some reason. Tested both `module rec` and `type..and` approaches.
///       Sample: https://github.com/bessgeor/GiraffeGenerator/commit/29702b0497e643f99dd24bfc3c6ab8ccb769ec7c
/// 
/// 2. DataAnnotations.Validator treats records, record properties and other values as a separate concepts.
///    This means we should differentiate them too (different codegen)
///
/// 4. DataAnnotations.Validator can't perform recursive validation.
///    E.g. given the model
///    type NestedObject =
///        { [<Range(2,3)>]
///          int2Or3: int }
///    type WhateverPostBodyJson =
///        { [<Required>]
///          NestedObject: NestedObject }
///    instance { NestedObject = { int2Or3 = 100500 } } would be treated as valid.
///    This means we should generate recursion ourselves.
///
/// Because we should generate traversal of object graph anyways,
/// we could solve 1. by including monad values validation into the traversal process.
///
/// ## Other constraints:
/// - We should provide an extension point for user to customize validation
/// - We should allow user to handle validation errors like any other contract violation.
///   Which means we should bind validation into Result<'model, ArgumentLocationedError
///
/// ## The design
/// The core of the validation is the static object graph traversal function (validate).
/// It accepts a boxed value and type-check matches it against
/// 1. Parameter types from every source including body - they would come to the validation in the first place
/// 2. Every record type nested in the parameters. This allows to reduce copy-paste codegen in case of type re-use and to reduce the maximum indentation
/// It returns an System.ComponentModel.DataAnnotations.ValidationResult array
/// It recurse on any TypeKind.Object property, array item or option value
/// It applies Validator.TryValidateObject to any record it finds.
///     This may be reconsidered to generation of validation for each property.
///     Considerations should include the validation caching as the property validation is probably the most realistic scenario for users, thus it allocates a lot.
///     Also, Validator.TryValidateObject ignores the property path given - custom validation may include the full path assembling.
///     Also, this place is the only real dependency on DataAnnotations for the internal code.
/// It applies special-cased validation (the one which accepts isValid flag ignoring the real value to avoid cross-dependency of attributes and user types) to any value requiring it.
/// It applies other validation defined for the value if it is not the direct value of a property (in which case Validator.TryValidateObject would carry out this validation).
/// It traverses Option.Value if Option.isSome.
/// It traverses arrays via for..to loop to provide index information for the property path.
/// It traverses record properties of non-primitive types.
///
/// There are some helper functions:
/// - validateInner
///   accepts the value and a validation function for it. Checks if there are implementations of extension point interfaces registered in the DI.
///   If IGiraffeValidator<'value> is registered, applies it's validation - this is an option for user to opt-out generated validation.
///   Otherwise the function from parameters is applied.
///   Anyways, if there is IGiraffeAdditionalValidator<'value> registered in the DI, it's output is appended to the results. This provides an opt-in validation extension point.
/// - isObjectValid
///   Curried wrapper for Validator.TryValidateObject to be passed as validation function to validateInner function.
/// - isObjectValid
///   Curried wrapper for Validator.TryValidateValue to be passed as validation function to validateInner function.
///   Accepts attribute array as the first parameter to be partially applied before passing.
/// - withValue/withMemberAndValue
///   Helpers for forking a ValidationContext objects.
/// - bindValidation
///   Interface function for validation. Binder for Result<'model, ArgumentLocationedError> calling the validate function.
module CodeGenValidation_LogicGeneration

open FSharp.Compiler.SyntaxTree
open OpenApi
open OpenApiToAstTypesMatchingAndConversion
open CodeGenErrorsDU
open CodeGenValidation_InstanceGeneration
open AST
open ASTExt 


/// Helper for ReferenceEquals(x, null).
/// FSharp thinks that interface declared in it may not be null
/// (including the instance that may be returned from IServiceProvider.GetService which is obviously false)
/// I don't like to mark the types with [<AllowNullLiteral>] in this case
/// because nullability is an interface of IServiceProvider.GetService which has nothing to do with the type itself.
/// So use System.Object.ReferenceEquals(value, null) instead of FSharp's isNull   
let private isNullRef x = tupleComplexExpr [identExpr x; SynExpr.Null r] |> app (longIdentExpr "System.Object.ReferenceEquals")

let isObjectValid = "isObjectValid"
/// let isObjectValid boxed errors validationContext = Validator.TryValidateObject(boxed, validationContext, errors, true)
let private isObjectValidDeclaration =
    letDecl false "isObjectValid" ["boxed"; "errors"; "validationContext"] None
        (app
            (longIdentExpr "Validator.TryValidateObject")
            (tupleComplexExpr [identExpr "boxed"; identExpr "validationContext"; identExpr "errors"; constExpr (SynConst.Bool true)]))

let isValueValid = "isValueValid"
/// let isValueValid validationAttributes boxed errors validationContext = Validator.TryValidateValue(boxed, validationContext, errors, validationAttributes)
let private isValueValidDeclaration =
    letDecl false "isValueValid" ["validationAttributes"; "boxed"; "errors"; "validationContext"] None
        (app
            (longIdentExpr "Validator.TryValidateValue")
            (tupleExpr ["boxed"; "validationContext"; "errors"; "validationAttributes"]))


/// let validateInner isValid (ctx: HttpContext) validationContext value =
///     let customValidator =
///         ctx.RequestServices.GetService<IGiraffeValidator<'model>>()
/// 
///     let errs =
///         if System.Object.ReferenceEquals(customValidator, null) then
///             let errs = System.Collections.Generic.List()
///             if isValid errs validationContext then Array.empty else errs |> Seq.toArray
///         else
///             customValidator.Validate(value, validationContext)
/// 
///     let customAugmentingValidator =
///         ctx.RequestServices.GetService<IGiraffeAdditionalValidator<'model>>()
/// 
///     if System.Object.ReferenceEquals(customAugmentingValidator, null) then
///         errs
///     else
///         customAugmentingValidator.Validate(value, validationContext)
///         |> Array.append errs 
let private validateInnerDeclaration replacerInterface augmenterInterface =
    // declaration of the binder function
    let generic = SynType.Var(Typar(ident "model", NoStaticReq, false), r)
    let args =
        [ synPat "isValid"
          tuplePatComplex [synPatTyped "ctx" (synType "HttpContext")]
          synPat "validationContext"
          tuplePatComplex [synPatTyped "value" generic] ]
    let declare = letDeclComplex false "validateInner" args None

    // let binding for custom validator interface instance
    let letCustomValidator = letGetAnyServiceDecl "customValidator" false (genericType false replacerInterface [generic])
    
    /// binding of errors either from built-in validation or from validation replacer service provided by the user
    let letErrs1 =
        letExpr "errs" []
            (ifElseExpr (isNullRef "customValidator")
                (letExpr "errs" [] (app (longIdentExpr "System.Collections.Generic.List") (tupleExpr []))
                    (
                        ifElseExpr (curriedCall "isValid" ["value"; "errs"; "validationContext"]) (longIdentExpr "Array.empty")
                            (identExpr "errs" ^|> longIdentExpr "Seq.toArray")
                    ))
                (app (longIdentExpr "customValidator.Validate") (tupleExpr ["value"; "validationContext"])))
    
    /// append errors from validation augmentation to the errors returned from the previous step
    /// if the validation augmentation is possible and defined for the validated type by the user
    let letErrs2 =
        augmenterInterface |> Option.map ^ fun augmenter ->
            letExpr "errs" [] (
                    letGetAnyServiceDecl "customAugmentingValidator" false (genericType false augmenter [generic])
                        (
                            ifElseExpr (isNullRef "customAugmentingValidator")
                                (identExpr "errs")
                                (app (longIdentExpr "customAugmentingValidator.Validate") (tupleExpr ["value"; "validationContext"])
                                 ^|> app (longIdentExpr "Array.append") (identExpr "errs"))
                        )
               )
        |> Option.defaultValue id
    
    declare
    ^ letCustomValidator
    ^ letErrs1
    ^ letErrs2
    ^ identExpr "errs"

/// let withValue (validationContext: ValidationContext) value =
///     let ctx = ValidationContext(box value, validationContext.Items)
///     ctx.InitializeServiceProvider(fun t -> validationContext.GetService t)
///     ctx.MemberName <- null
///     ctx
let withValueDeclaration =
    let args = [tuplePatComplex [synPatTyped "validationContext" (synType "ValidationContext")]; synPat "value"]
    let declare = letDeclComplex false "withValue" args None
    let letCtx =
        letExpr "ctx" []
            (tupleComplexExpr
                [(identExpr "value"); (longIdentExpr "validationContext.Items")]
                |> app (identExpr "ValidationContext"))
    let actions =
        seqExprs [
            app (longIdentExpr "ctx.InitializeServiceProvider") (lambda (simplePats [ simplePat "t" ]) (app (longIdentExpr "validationContext.GetService") (identExpr "t")))
            SynExpr.LongIdentSet(longIdentWithDots "ctx.MemberName", SynExpr.Null r, r)
            identExpr "ctx"
        ]
    declare
    ^ letCtx
    ^ actions

/// let withMemberAndValue validationContext name value =
///     let ctx = withValue validationContext value
///     // this is the original behaviour of Validator: it would ignore MemberName in context when object is validated
///     // so keep it for consistency
///     ctx.MemberName <- name
///     ctx
let withMemberAndValueDeclaration =
    let args = [
        tuplePatComplex [synPatTyped "validationContext" (synType "ValidationContext")]
        synPat "name"
        synPat "value"
    ]
    let declare = letDeclComplex false "withMemberAndValue" args None
    let letCtx =
        letExpr "ctx" [] (curriedCall "withValue" [ "validationContext"; "value" ])
    let actions =
        seqExprs [
            // this is the original behaviour of Validator: it would ignore MemberName in context when object is validated
            // so keep it for consistency
            SynExpr.LongIdentSet(longIdentWithDots "ctx.MemberName", identExpr "name", r)
            identExpr "ctx"
        ]
    declare
    ^ letCtx
    ^ actions
    
let validationBinder = "bindValidation"
/// let bindValidation isValid (ctx: HttpContext) location (value: 'model) =
///     let validationContext =
///         ValidationContext(value, ctx.RequestServices, ctx.Items)
/// 
///     let errs = validate isValid ctx validationContext
///     if Array.length errs > 0 then
///         errs |> ArgumentValidationError |> location |> Error
///     else Ok value
let private binderDeclaration =
    let generic = SynType.Var(Typar(ident "model", NoStaticReq, false), r)
    let args =
        [ tuplePatComplex [synPatTyped "ctx" (synType "HttpContext")]
          synPat "location"
          tuplePatComplex [synPatTyped "value" generic] ]
    let letDecl = letDeclComplex false validationBinder args None
    
    // validation context which contains the value, service provider and a property bag (Yay! We are like AspNetCore MVC here)
    let letValidationContext =
        letExpr "validationContext" []
            (app
                 (identExpr "ValidationContext")
                 (tupleComplexExpr [identExpr "value"; longIdentExpr "ctx.RequestServices"; longIdentExpr "ctx.Items"]))
    let letErrs = letExpr "errs" [] (curriedCall "validate" ["ctx"; "validationContext"])
    
    let returnExpr =
        ifElseExpr ((identExpr "errs" ^|> Array.lengthExpr |> paren) ^= intExpr 0)
            (app (identExpr "Ok") (identExpr "value"))
            (identExpr "errs"
             ^|> identExpr errInnerValidationError
             ^|> identExpr "location"
             ^|> identExpr "Error")
    
    letDecl
    ^ letValidationContext
    ^ letErrs
    ^ returnExpr


/// let rec validate isPassedValueValid ctx (validationContext: ValidationContext) =
///     [|
///         match validationContext.ObjectInstance with
///         | :? (TestValidationPostBodyJson array) as value ->
///             yield! validateInner isPassedValueValid ctx validationContext value 
///             for value in value do
///                 yield! validate isObjectValid ctx (withValue validationContext value)
///         | :? (TestValidationPostBodyJson) as value ->
///             yield! validateInner isPassedValueValid ctx validationContext value
///             yield! validate isObjectValid ctx (withMemberAndValue validationContext "nestedValidationObjectOfIntGTE2" value.nestedValidationObjectOfIntGTE2)
///             if value.nestedValidationOptionOfIntGTE2.IsSome then
///                 let newValidationCtx = withMemberAndValue validationContext "nestedValidationOptionOfIntGTE2.Value" value.nestedValidationOptionOfIntGTE2.Value
///                 yield! validateInner (isValueValid [| RangeAttribute(2, System.Int32.MaxValue) |]) ctx newValidationCtx value.nestedValidationOptionOfIntGTE2.Value
///             if value.deeplyNestedValidation.IsSome then
///                 for value in value.deeplyNestedValidation.Value do
///                     for value in value do
///                         yield! validate isObjectValid ctx (withMemberAndValue validationContext "deeplyNestedValidation.Value.[i].[j]" value)
///         | :? (nestedValidationObjectOfIntGTE2) as value ->
///             yield! validateInner isPassedValueValid ctx validationContext value
///         | :? (deeplyNestedValidation) as value ->
///             yield! validateInner isPassedValueValid ctx (withValue validationContext value) value
///         | :? (PostTestValidationPath) as value ->
///             yield! validateInner isPassedValueValid ctx validationContext value
///         | :? (PostTestValidationQuery) as value ->
///             yield! validateInner isPassedValueValid ctx validationContext value
///         | v -> failwithf "Unknown type came to validation: %A" (v.GetType())
///     |]
module private ValidateDeclaration =
    let private validateInner isValid vCtx value =
        let fName =
            if Option.isSome value then
                "validateInner"
            else "validate"
        let f = identExpr fName
        [
            yield f
            if Option.isSome value then
                yield isValid
            yield identExpr "ctx"
            yield vCtx
            if Option.isSome value then
                yield Option.get value
        ]
        |> List.reduce (app)
        |> yieldBang

    let private validationContext = identExpr "validationContext"

    let private letNewCtx innerCtx body =
        body |> Option.map ^ fun body ->
            letExpr "validationContext" [] innerCtx body

    let private validatePassed value = validateInner (identExpr isObjectValid) validationContext (Some value)

    let private validateValue value attrs =
        validateInner (app (identExpr isValueValid) attrs |> paren) validationContext (Some value)

    let private validateObj vCtx = validateInner (identExpr isObjectValid) vCtx None

    let private withMemberAndValueComplex memberNameExpr value =
        app (app (app (identExpr "withMemberAndValue") validationContext) memberNameExpr) value

    let private withMemberAndValue memberName value =
        withMemberAndValueComplex (strExpr memberName) value

    let private arrayIteratorVars =
        [|0..10|]
        |> Seq.map char
        |> Seq.map ((+)'i')
        |> Seq.map string
        |> Seq.map (fun i -> ident i, identExpr i)
        |> Seq.toArray

    let private getArrayIteratorOrFail i =
        let i = i - 1 // i is always incremented before access
        if i >= arrayIteratorVars.Length then
            failwithf "Nested arrays of %d deepness aren't supported" i
        else arrayIteratorVars.[i]

    let private generateInnerValidation recurse memberName kind arrayNesting isInPropertyContext newCtx innerExpr =
        match kind with
        | TypeKind.Object _ -> validateObj validationContext |> Some |> letNewCtx newCtx
        // Option itself may not be validated so don't generate a context for it
        // It should be special-cased because `new ValidationContext(None)` would throw ArgumentNullException
        | TypeKind.Option _ -> recurse memberName arrayNesting isInPropertyContext innerExpr kind
        | _ -> recurse memberName arrayNesting isInPropertyContext innerExpr kind |> letNewCtx newCtx

    let private generateSpecialCasedValidation isInPropertyContext expr kind = [
        for isSpecialCased, ownValidationAttributes in getValidationAttributeConstructionForKind expr kind do
            if (not isSpecialCased) && (not isInPropertyContext) then // properties would be validated against non special case attributes by validateObject 
                validateValue expr ownValidationAttributes |> Some
            if isSpecialCased then 
                validateValue expr ownValidationAttributes |> Some
    ]

    let rec private generateValidateCaseBody memberName arrayNesting isInPropertyContext expr kind: SynExpr option =
        let expressions =
            [
                match kind with
                | TypeKind.Object o ->
                    validatePassed expr |> Some
                    for (name, kind, _) in o.Properties do
                        let innerExpr = SynExpr.DotGet(expr, r, longIdentWithDots name, r)
                        let newCtx = withMemberAndValue name innerExpr
                        generateInnerValidation generateValidateCaseBody (Some name) kind arrayNesting true newCtx innerExpr

                | TypeKind.Array (inner, _, _) ->
                    yield! generateSpecialCasedValidation isInPropertyContext expr kind
                    
                    let arrayNesting = arrayNesting + 1
                    let iDef, iExpr = getArrayIteratorOrFail arrayNesting
                    
                    let innerExpr = SynExpr.DotIndexedGet(expr, [SynIndexerArg.One(iExpr, false, r)], r, r)
                    let newCtx = withMemberAndValueComplex (sprintfExpr "%s[%d]" [longIdentExpr "validationContext.MemberName"; iExpr] |> paren) innerExpr
                    
                    generateInnerValidation generateValidateCaseBody None inner arrayNesting false newCtx innerExpr
                    |> Option.map ^ fun innerBody ->
                        let outerBound = appBinaryOpExpr "op_Subtraction" (SynExpr.DotGet(expr, r, longIdentWithDots "Length", r)) (intExpr 1)
                        SynExpr.For(DebugPointAtFor.Yes r, iDef, intExpr 0, true, outerBound, innerBody, r)

                | TypeKind.Option inner ->
                    let innerExpr = SynExpr.DotGet(expr, r, longIdentWithDots "Value", r)
                    let memberNameExpr =
                        if memberName.IsSome then
                            strExpr (memberName.Value + ".Value")
                        else sprintfExpr "%s.Value" [longIdentExpr "validationContext.MemberName"] |> paren
                    let newCtx = withMemberAndValueComplex memberNameExpr innerExpr
                    generateInnerValidation generateValidateCaseBody None inner arrayNesting false newCtx innerExpr
                    |> Option.map ^ fun innerBody -> 
                        ifExpr (expr ^|> longIdentExpr "Option.isSome") innerBody

                | _ ->
                    yield! generateSpecialCasedValidation isInPropertyContext expr kind
            ] |> List.choose id
        tryGetSingleExpression expressions

    let private generateValidateCases expr outerName kind =
        let rec gatherObjects isOutermost outerName kind =
            let inline gatherObjects o k = gatherObjects false o k
            match kind with
            | TypeKind.Object o ->
                [
                    // all topmost types should be matched whenever they are objects or not. 
                    if not isOutermost then
                        extractResponseSynType outerName kind, kind
                    for name, kind, _ in o.Properties do
                        yield! gatherObjects (Some name) kind
                ]
            | TypeKind.Array (kind, _, _)
            | TypeKind.Option kind -> gatherObjects outerName kind
            | TypeKind.DU _ -> failwith "DU validation is not supported"
            | TypeKind.Prim _
            | TypeKind.BuiltIn _
            | TypeKind.NoType -> []
        let generateCase synType kind =
            let isInPropertyContext = false
            synType, generateValidateCaseBody None 0 isInPropertyContext expr kind
        [
            generateCase (extractResponseSynType outerName kind) kind
            for synType, obj in gatherObjects true outerName kind do
                generateCase synType obj
        ]

    let validateDeclaration api =
        matchExpr "instance" [
            for path in api.Paths do
                for method in path.Methods do
                    for schema in method.AllParameters do
                        for kind, body in generateValidateCases (identExpr "value") (Some schema.Name) schema.Kind do
                            clause
                                (SynPat.Named(SynPat.IsInst(kind, r), ident "value", false, None, r))
                                (Option.defaultValue unitExpr body)
            clause (synPat "v") (app (app (identExpr "failwithf") (strExpr "Unknown type came to validation: %A")) (app (longIdentExpr "v.GetType") unitExpr |> paren))
        ]
        |> List.singleton
        |> fun x -> SynExpr.ArrayOrList(true, x, r)
        |> letExpr "instance" [] (longIdentExpr "validationContext.ObjectInstance")
        |> letDeclComplex true "validate"
               [ synPat "ctx"
                 tuplePatComplex [synPatTyped "validationContext" (synType "ValidationContext")]
               ] None
    
let generateValidationBinder api replacerInterface augmenterInterface =
    seq {
        isObjectValidDeclaration
        isValueValidDeclaration
        validateInnerDeclaration replacerInterface augmenterInterface
        withValueDeclaration
        withMemberAndValueDeclaration
        ValidateDeclaration.validateDeclaration api
        binderDeclaration
    }

/// Applies validation to the resultExpr:
/// {resultExpr} |> Result.bind (bindValidation isObjectValid ctx {location})
/// for objects and DUs or
/// {resultExpr} |> Result.bind (bindValidation (isValueValid [|{attribute instances for this value}|]) ctx {location})
/// for any other kind of value
let bindValidationIntoResult location resultExpr =
    let validationBinder = identExpr validationBinder
    resultExpr ^|> Result.bindExpr (app (app validationBinder (identExpr "ctx")) (identExpr location) |> paren)
