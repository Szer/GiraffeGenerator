module CodeGenValidation_TypeGeneration

open FSharp.Compiler.SyntaxTree
open CodeGenValidation_Types
open AST
open ASTExt

/// codegen custom attributes for
/// - multiplyOf
/// - enum
/// - uniqueItems
/// - pattern (built-in one doesn't follow the OpenAPI spec: not ECMAScript and always tries to match the whole string)
/// - minValue and maxValue for Int64 (as a Range attribute)
/// Example:
(*
type IntEnumAttribute([<System.ParamArray>]values: int array) =
    inherit ValidationAttribute(sprintf "must be a one of %s" (String.concat ", " (Seq.map string values)))
        override this.FormatErrorMessage name = sprintf "The field %s %s" name base.ErrorMessage
        override _.IsValid(value: obj) =
            let value = value :?> int
            values |> Array.contains value

type StringEnumAttribute([<System.ParamArray>]values: string array) =
    inherit ValidationAttribute(sprintf "must be a one of %s" (String.concat ", " values))
        override this.FormatErrorMessage name = sprintf "The field %s %s" name base.ErrorMessage
        override _.IsValid(value: obj) =
            let value = value :?> string
            values |> Array.contains value

type IntMultiplyOfAttribute(divisor: int) =
    inherit ValidationAttribute(sprintf "must be a multiply of %d" divisor)
        override this.FormatErrorMessage name = sprintf "The field %s %s" name base.ErrorMessage
        override _.IsValid(value: obj) =
            let value = value :?> int
            value % divisor = 0

// special case, as described in the according DU
type UniqueItemsAttribute(isValid) =
    inherit ValidationAttribute("must contain only unique values")
        override this.FormatErrorMessage name = sprintf "The field %s %s" name base.ErrorMessage
        override _.IsValid(value: obj) = isValid
*)
/// generates custom ValidationAttribute. Body should assume single parameter "value" of type obj
let private generateAttributeDefinitionASTComplex additionalMembers (attributeName, pats, messageExpr, bodyExpr) =
    let componentInfo =
        SynComponentInfo.ComponentInfo
            ([], [], [], longIdent attributeName, xmlEmpty, false, None, r)

    let implicitCtor = SynMemberDefn.ImplicitCtor(None, [], simplePats pats, None, r)

    let inheritance =
        SynMemberDefn.ImplicitInherit(synType "ValidationAttribute", messageExpr, None, r)
    
    let methodOverride =
        implDefn MemberKind.Member true "IsValid" ["value"] bodyExpr
    
    let messageOverride =
        implDefn MemberKind.Member true "FormatErrorMessage" ["name"]
            (sprintfExpr "The field %s %s." [identExpr "name"; longIdentExpr "base.ErrorMessageString"])
    
    let members =
        [
            implicitCtor
            inheritance
            yield! additionalMembers
            messageOverride
            methodOverride
        ]
    
    let objModel =
        SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconUnspecified, members, r)

    TypeDefn(componentInfo, objModel, [], r)

let private rangeBody name synType =
    let pats = [simpleTypedPat "min" synType; simpleTypedPat "max" synType]
    let min = identExpr "min"
    let max = identExpr "max"
    let value = identExpr "value"
    let lte = appBinaryOpExpr "op_LessThanOrEqual"
    let checkMin = lte min value
    let checkMax = lte value max
    let body = appBinaryOpExpr "op_BooleanAnd" checkMin checkMax
    let message = "must be between %d and %d", [min; max]
    name + "RangeAttribute", pats, message, body        

let private generateSpecialCasedAttributeBody attrName attrMessage =
    let pats = [simpleTypedPat "isValid" boolType]
    let message = strExpr attrMessage
    attrName, pats, message, identExpr "isValid"

let private generateCustomAttributeValidationBodyForTypedValue value =
    match value with
    | LongRange _ -> [], rangeBody "Long" int64Type
    | RegexPattern _ ->
        let pattern = simpleTypedPat "pattern" (stringType)
        let pats = [pattern]
        let pattern = identExpr "pattern"
        let regexMember =
            [
                pattern
                appBinaryOpExpr
                    "op_BitwiseOr"
                    (longIdentExpr "System.Text.RegularExpressions.RegexOptions.ECMAScript")
                    (longIdentExpr "System.Text.RegularExpressions.RegexOptions.Compiled")
            ]
            |> tupleComplexExpr
            |> app (longIdentExpr "System.Text.RegularExpressions.Regex")
        let regexMember =
            SynMemberDefn.LetBindings
                ([
                    SynBinding.Binding
                        (None,
                         SynBindingKind.NormalBinding,
                         false,
                         false,
                         [],
                         xmlEmpty,
                         SynValData (None, SynValInfo([], SynArgInfo([], false, None)), None),
                         synPat "regex",
                         None,
                         regexMember,
                         r,
                         DebugPointForBinding.DebugPointAtBinding r)
                 ], false, false, r)
        let body = app (longIdentExpr "regex.IsMatch") (identExpr "value")
        let message = "must match the regular expression '%s'", [pattern]
        [regexMember], ("RegexPattern", pats, message, body)
    | MultipleOf target ->
        let name, argType, zero =
            match target with
            | IntMultiple _ -> "Int", intType, intExpr 0
            | LongMultiple _ -> "Long", int64Type, constExpr(SynConst.Int64 0L)
        let pats = [simpleTypedPat "divisor" argType]
        let divisor = identExpr "divisor"
        let value = identExpr "value"
        let body = appBinaryOpExpr "op_Modulus" value divisor ^= zero
        let name = sprintf "%sMultipleOfAttribute" name
        let message = "must be a multiple of %d", [divisor]
        [], (name, pats, message, body)
    | EnumAttribute target ->
        let name, argType, mapper =
            match target with
            | IntEnum _ -> "Int", intType, identExpr "string"
            | LongEnum _ -> "Long", int64Type, identExpr "string"
            | FloatEnum _ -> "Float", doubleType, identExpr "string"
            | StringEnum _ -> "String", stringType, _id
        let pats = [SynSimplePat.Attrib(simpleTypedPat "values" (genericType true "array" [argType]), [attr "System.ParamArray"], r)]
        let values = identExpr "values"
        let value = identExpr "value"
        let body = values ^|> app (longIdentExpr "Array.contains") value
        let name = sprintf "%sEnumValuesAttribute" name
        let message = "must be a one of ['%s']", [values ^|> Seq.mapExpr mapper ^|> String.concatExpr "', '" |> paren]
        [], (name, pats, message, body)
    
let private messageToExpr (pattern, args) =
    if args |> List.tryHead |> Option.isSome then
        sprintfExpr pattern args |> paren
    else strExpr pattern

let private generateAttributeForSpecialCasedValidationType value =
    match value with
    | UniqueItems ->
        generateSpecialCasedAttributeBody "UniqueItemsAttribute" "must contain only unique items"
    |> generateAttributeDefinitionASTComplex []

let private generateAttributeForCustomValidationType value =
    let additionalMembers, (name, pats, message, rawBody) = generateCustomAttributeValidationBodyForTypedValue value
    let message = messageToExpr message
    match value with
    | LongRange _ ->
        let body = letExpr "value" [] (SynExpr.Downcast(identExpr "value", int64Type, r)) rawBody
        name, pats, message, body
    | RegexPattern _ ->
        let body = letExpr "value" [] (SynExpr.Downcast(identExpr "value", stringType, r)) rawBody
        name, pats, message, body
    | MultipleOf target ->
        let argType =
            match target with
            | IntMultiple _ -> intType
            | LongMultiple _ -> int64Type
        let body = letExpr "value" [] (SynExpr.Downcast(identExpr "value", argType, r)) rawBody
        name, pats, message, body
    | EnumAttribute target ->
        let argType =
            match target with
            | IntEnum _ -> intType
            | LongEnum _ -> int64Type
            | FloatEnum _ -> doubleType
            | StringEnum _ -> stringType
        let body = letExpr "value" [] (SynExpr.Downcast(identExpr "value", argType, r)) rawBody
        name, pats, message, body
    |> generateAttributeDefinitionASTComplex additionalMembers

let generateAttributeDefinitionFor value =
    match value with
    | CustomAttribute custom ->
        generateAttributeForCustomValidationType custom
        |> Some
    | SpecialCasedCustomValidationAttribute specialCased ->
        generateAttributeForSpecialCasedValidationType specialCased
        |> Some
    | BuiltInAttribute _ -> None


/// Validation extensibility is accomplished by the interfaces below.
/// To customize validation, user has to implement any of those interfaces for bound model types
/// E.g. for types resulted by default application before type combining
module ExtensionPoints =
    let private generateValidationInterface name summary generic methodName methodType =
        let docs = xmlDocs [[sprintf "<summary>%s</summary>" summary]]
        
        let componentInfo =
            SynComponentInfo.ComponentInfo
                ([attr "Interface"], [TyparDecl([], generic)], [], longIdent name, docs, false, None, r)
        
        let methodDefn = abstractMemberDfn xmlEmpty methodName methodType
            
        let objModel =
            SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconUnspecified, [methodDefn], r)

        TypeDefn(componentInfo, objModel, [], r)

    let validationReplacerInterface = "IGiraffeValidator"
    /// [<Interface>]
    /// /// <summary> replaces any generated validation rules for type </summary>
    /// type IGiraffeValidator<'model> =
    ///     abstract member Validate: 'model * ValidationContext -> ValidationResult array
    let generateValidationReplacerInterface () =
        let generic = Typar(ident "model", NoStaticReq, false)
        let sign = tuple [SynType.Var(generic, r); synType "ValidationContext"] ^-> genericType true "array" [synType "ValidationResult"]
        generateValidationInterface validationReplacerInterface "replaces any generated validation rules for type" generic "Validate" sign
        
    let validationAugmenterInterface = "IGiraffeAdditionalValidator"
    /// [<Interface>]
    /// /// <summary> augments generated validation rules </summary>
    /// type IGiraffeAdditionalValidator<'model> =
    ///    abstract member Validate: 'model * ValidationContext -> ValidationResult array
    let generateValidationAugmenterInterface () =
        let generic = Typar(ident "model", NoStaticReq, false)
        let sign = tuple [SynType.Var(generic, r); synType "ValidationContext"] ^-> genericType true "array" [synType "ValidationResult"]
        generateValidationInterface validationAugmenterInterface "replaces any generated validation rules for type" generic "Validate" sign
      
    // maybe later:  
    (*
    [<Interface>]
    /// <summary> augments any validation rules with model verification </summary>
    type IGiraffeVerifier<'model> =
        abstract member Verify: 'model * ValidationContext -> Task<ValidationResult array>
    *)