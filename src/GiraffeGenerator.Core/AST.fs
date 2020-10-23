module AST

open System
open FSharp.Compiler.XmlDoc
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range

/// Zero range. Used pretty much everywhere
let r = range0

/// Range with one line space for indentation (used in list comprehensions)
let r1 = rangeN "" 1

/// Empty XmlDocs
let xmlEmpty = PreXmlDoc.Empty

/// expression for ()
let unitExpr = SynExpr.Const(SynConst.Unit, r)

/// int expression
let intExpr i = SynExpr.Const(SynConst.Int32 i, r)

/// Ident with 0-range from string
let ident name = Ident(name, r)

/// LongIdent with splitting incoming string by '.'
let longIdent (name: string): LongIdent =
    name.Split([| '.' |])
    |> Seq.map ident
    |> Seq.toList

/// LongIdentWithDots with splitting incoming string by '.'
let longIdentWithDots name = LongIdentWithDots(longIdent name, [])

/// Open declaration with splitting incoming string by '.'
let openDecl name =
    SynModuleDecl.Open(longIdentWithDots name, r)

/// Expression for type
let synType name =
    SynType.LongIdent(longIdentWithDots name)

/// Expression for function type
/// {argType} -> {synType}
let funType argType returnType = SynType.Fun(argType, returnType, r)

/// Infix right-associative operator for creating function type
let inline (^->) a b = funType a b

/// Argument expression
let arg name =
    SynArgInfo.SynArgInfo([], false, Some(ident name))

/// Expression which represents empty argument
let emptyArg = SynArgInfo.SynArgInfo([], false, None)

/// Expression which represents empty argument block for properties
let propertyVal = SynValInfo.SynValInfo([], emptyArg)

/// Expression which represents empty argument block for methods
let emptyMethodVal =
    SynValInfo.SynValInfo([ [ emptyArg ] ], emptyArg)

/// Expression for named pattern {name}
let synPat name =
    SynPat.Named(SynPat.Wild(r), ident name, false, None, r)

/// Expression for long idented pattern
/// {pat} {name}
let synLongPat pat name =
    SynPat.LongIdent(longIdentWithDots pat, None, None, SynArgPats.Pats [ synPat name ], None, r)

/// Expression for multiple args in ctor with patterns
let ctorArgs args = Pats <| List.map synPat args

/// Expression which represents curried argument block
/// e.g. {arg1} {arg2} ...
let curriedArgs argList =
    let args = List.map (arg >> List.singleton) argList
    SynValInfo.SynValInfo(args, emptyArg)




/// Abstract member declaration
let abstractDfn xmlDocs memberKind valDfn name synType: SynMemberDefn =
    SynMemberDefn.AbstractSlot
        (SynValSig.ValSpfn
            ([],
             ident name,
             SynValTyparDecls.SynValTyparDecls([], true, []),
             synType,
             valDfn,
             false,
             false,
             xmlDocs,
             None,
             None,
             r),
         { IsInstance = true
           IsDispatchSlot = true
           IsOverrideOrExplicitImpl = false
           IsFinal = false
           MemberKind = memberKind },
         r)

/// Abstract method member declaration
/// abstract {name}: {synType}
let abstractMemberDfn docs name returnType =
    abstractDfn docs MemberKind.Member emptyMethodVal name returnType

/// Abstract member declaration returning HttpHandler
/// abstract {name}: HttpHandler
let abstractGetterDfn docs name returnType: SynMemberDefn =
    abstractDfn docs MemberKind.PropertyGet propertyVal name returnType

/// Expression for creating single attribute
let attr name: SynAttributeList =
    { Attributes =
          [ { TypeName = longIdentWithDots name
              ArgExpr = unitExpr
              Target = None
              AppliesToGetterAndSetter = false
              Range = r } ]
      Range = r }

/// Type declaration with provided type name and members
let abstractClassDecl name members =
    let componentInfo =
        SynComponentInfo.ComponentInfo
            ([ attr "AbstractClass" ], [], [], longIdent name, PreXmlDoc.Empty, false, None, r)

    let implicitCtor =
        SynMemberDefn.ImplicitCtor(None, [], SynSimplePats.SimplePats([], r), None, r)

    let objModel =
        SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconUnspecified, implicitCtor :: members, r)

    SynModuleDecl.Types([ TypeDefn(componentInfo, objModel, [], r) ], r)

/// Module declaration with splitting incoming string by '.'
/// and with [<RequireQualifiedAccess>] attribute
let moduleDecl xml name decls =
    let moduleName = longIdent name

    let attrib = [ attr "RequireQualifiedAccess" ]
    SynModuleOrNamespace.SynModuleOrNamespace
        (moduleName, false, SynModuleOrNamespaceKind.NamedModule, decls, xml, attrib, None, r)

/// Lambda declaration for
/// fun next ctx -> {expr}
let lambdaFunNextCtxExpr expr =
    SynExpr.Lambda
        (false,
         false,
         SynSimplePats.SimplePats([ SynSimplePat.Id(ident "next", None, false, false, false, r) ], r),
         SynExpr.Lambda
             (false,
              true,
              SynSimplePats.SimplePats([ SynSimplePat.Id(ident "ctx", None, false, false, false, r) ], r),
              expr,
              r),
         r)

/// Let declaration
/// E.g. let {recursive} {name} {parameters}: retType = {expr}
let letDecl recursive name parameters retType expr =
    let retType = retType |> Option.map (longIdentWithDots >> SynType.LongIdent)
    SynModuleDecl.Let
        (
            recursive,
            [
                SynBinding.Binding
                    (
                        None,
                        SynBindingKind.NormalBinding,
                        false,
                        false,
                        [],
                        PreXmlDocEmpty,
                        SynValData.SynValData(None, curriedArgs parameters, None),
                        SynPat.LongIdent(longIdentWithDots name, None, None, parameters |> List.map (fun p -> SynPat.Named(SynPat.Wild(r), ident p, false, None, r)) |> Pats, None, r),
                        retType |> Option.map (fun retType -> (SynBindingReturnInfo(retType, r, []))),
                        retType
                            |> Option.map (fun retType -> SynExpr.Typed(expr, retType, r))
                            |> Option.defaultValue expr,
                        r,
                        NoDebugPointAtLetBinding
                    )
            ],
            r
        )

/// Let declaration for Giraffe HttpHandler with specified name and body expression
/// E.g.:
/// let {NAME}: HttpHandler = fun next ctx -> {EXPR}
let letHttpHandlerDecl name expr =
    letDecl false name [] (Some "HttpHandler") (lambdaFunNextCtxExpr expr)

/// Expression for calling:
/// ctx.GetService<Service>()
let getServiceCall =
    SynExpr.App
        (ExprAtomicFlag.Atomic,
         false,
         SynExpr.TypeApp
             (SynExpr.LongIdent(false, longIdentWithDots "ctx.GetService", None, r),
              r,
              [ SynType.LongIdent(longIdentWithDots "Service") ],
              [],
              None,
              r,
              r),
         unitExpr,
         r)

/// Let expression with continuation for calling:
/// let {binding} = {body} in {next}
let letExprComplex binding body next =
    SynExpr.LetOrUse
        (false,
         false,
         [ SynBinding.Binding
             (None,
              SynBindingKind.NormalBinding,
              false,
              false,
              [],
              PreXmlDoc.Empty,
              SynValData(None, emptyMethodVal, None),
              binding,
              None,
              body,
              r,
              DebugPointAtBinding(r)) ],
         next,
         r)
let letExprComplexParameters name parameters body next =
    letExprComplex
        (SynPat.LongIdent(longIdentWithDots name, None, None, parameters, None, r))
        body
        next
/// Let expression with continuation for calling:
/// let {name} {parameters} = {body} in {next}
let letExpr name parameters body next =
    letExprComplexParameters
        name
        (parameters |> List.map (fun p -> SynPat.Named(SynPat.Wild(r), ident p, false, None, r)) |> Pats)
        body
        next

/// Let expression with continuation for calling:
/// let service = ctx.GetService<Service>() in {NEXT}
let letGetServiceDecl next =
    letExpr "service" [] getServiceCall next

/// If-Then-Else expression
/// if {cond} then {ifTrue} else {ifFalse}
let ifElseExpr cond ifTrue ifFalse =
    SynExpr.IfThenElse(cond, ifTrue, Some ifFalse, DebugPointForBinding.NoDebugPointAtInvisibleBinding, false, r, r)

/// LetBang! expression:
/// let! {ident} = {expr} in {next}
let letBangExpr ident expr next =
    SynExpr.LetOrUseBang(DebugPointAtBinding r, false, false, synPat ident, expr, [], next, r)



/// Expression for task builder with body:
/// task { body }
let taskBuilder body =
    SynExpr.App
        (ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(ident "task"), SynExpr.CompExpr(false, ref false, body, r), r)

/// Expression for ReturnFrom (return!) with continuation:
/// return! {expr}
let returnBang expr =
    SynExpr.YieldOrReturnFrom((false, true), expr, r)
    
/// Expression for Return (return) with continuation:
/// return {expr}
let returnExpr expr =
    SynExpr.YieldOrReturn((false, true), expr, r)

/// Expression for list comprehension:
/// [ {expr} ]
let arrayExpr expr =
    SynExpr.ArrayOrListOfSeqExpr(false, expr, r)

/// Expression for function application:
/// {funExpr} {argExpr}
let app funExpr argExpr =
    SynExpr.App(ExprAtomicFlag.NonAtomic, false, funExpr, argExpr, r)

/// Expression for INFIX function application:
/// {argExpr} {funExpr}
let appI funExpr argExpr =
    SynExpr.App(ExprAtomicFlag.NonAtomic, true, funExpr, argExpr, r)

/// Expression for type application
/// {a}<{b[0],b[1]...}>
let typeApp a b =
    SynExpr.TypeApp(a,r,b,[],None,r,r)

/// Parenthesizes expression
let paren expr =
    SynExpr.Paren(expr, expr.Range.StartRange, Some expr.Range.EndRange, expr.Range)

/// Expression for Ident
let identExpr name =
    if name = "()" then SynExpr.Tuple(false, [], [ r ], r) |> paren
    else SynExpr.Ident(ident name)

/// Expression for empty Ident with non-zero range for proper indentation in list comprehensions
let emptyIdent = SynExpr.Ident(Ident("", r1))

/// Expression for LongIdent
let longIdentExpr name =
    SynExpr.LongIdent(false, longIdentWithDots name, None, r)

/// Sequential expression for two expressions (used in CE)
let seqExpr expr1 expr2 =
    SynExpr.Sequential(DebugPointAtSequential.Both, true, expr1, expr2, r)

/// Sequential expression for expression list (used in CE)
/// [ e1; e2; e3; e4] -> Seq(e1, Seq(e2, Seq(e3, e4)))
/// Will throw exception on expression list's size less than 2
/// Empty ident added to the end to enforce indentation
let rec seqExprs exprs =
    match exprs with
    | []
    | [ _ ] -> failwith "imbosibru"
    | [ e1; e2 ] -> seqExpr e1 (seqExpr e2 emptyIdent)
    | e :: exprs -> seqExpr e (seqExprs exprs)

/// CE expression for expression list using Sequential
let ceExprList exprs =
    SynExpr.CompExpr(true, ref true, seqExprs exprs, r)

/// Expression for application of next and ctx to expr
/// {expr} next ctx
let appNextCtx expr =
    app (app expr (identExpr "next")) (identExpr "ctx")

/// Expression for Giraffe choose function with list of expressions inside
/// Will throw exception on expression list's size less than 2
/// choose [ {exprList} ]
let chooseExpr exprList =
    app (identExpr "choose") (arrayExpr (ceExprList exprList))

/// Infix application expression for Giraffe Kleisli composition operator >=>
/// {e1} >=> {e2}
let (>=>) e1 e2 =
    app (appI (identExpr "op_GreaterEqualsGreater") e1) e2


let constExpr value =
    SynExpr.Const(value, r)
    
/// Constant string expression
let strExpr str =
    constExpr (SynConst.String(str, r))


let sprintfExpr pattern args =
    let initial = app (identExpr "sprintf") (strExpr pattern)
    args |> Seq.fold app initial

/// Application expression for Giraffe route function
/// route {route}
let route route = app (identExpr "route") (strExpr route)

/// Application expression for Giraffe routeBind function
/// routeBind {route} {next}
let routeBind route next =
    app (app (identExpr "routeBind") (strExpr route)) next

/// Expression for calling methods from service:
/// service.{name}
let service name = longIdentExpr ("service." + name)

/// Expression for record with fields
let record xml name fieldList =
    TypeDefn
        (SynComponentInfo.ComponentInfo([ attr "CLIMutable" ], [], [], longIdent name, xml, false, None, r),
         SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(None, fieldList, r), r),
         [],
         r)

/// Expression for discriminated unions
let discriminatedUnion xml name cases =
    let cases =
        cases
        |> List.map
           (
               fun (name, kind, xml) ->
                   SynUnionCase.UnionCase
                       (
                           SynAttributes.Empty,
                           ident name,
                           SynUnionCaseType.UnionCaseFields [ SynField.Field(SynAttributes.Empty, false, None, kind, false, xmlEmpty, None, r) ],
                           xml,
                           None,
                           r
                       )
           )
    TypeDefn
        (SynComponentInfo.ComponentInfo([],[],[],longIdent name, xml, false, None, r),
         SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(None, cases, r),r),SynMemberDefns.Empty,r)

/// Expression for anonymous record type with fields
let anonRecord fieldList = SynType.AnonRecd(false, fieldList, r)

/// Expression for record instance
let recordExpr fieldsAndValues =
    SynExpr.Record(None, None, fieldsAndValues |> List.map (fun (f, v) -> RecordFieldName (longIdentWithDots f, false), v |> Some, None), r)

/// Expression for generic type
let genericType isPostfix name synTypes =
    SynType.App(SynType.LongIdent(longIdentWithDots name), None, synTypes, [], None, isPostfix, r)

let simplePat name = SynSimplePat.Id(ident name,None,false,false,false,r)

let simplePats pats = SynSimplePats.SimplePats(pats, r)

let singleTypedPat name synType = [SynSimplePat.Typed(simplePat name, synType, r)] |> simplePats 

let singleSimplePat name = simplePats [simplePat name]

let lambda pats body =
    SynExpr.Lambda(false, false, pats, body, r) |> paren


/// Expression for generic array type in postfix notation:
/// {synType} array
let arrayOf synType = genericType true "array" [ synType ]

/// Expression for generic option type in postfix notation:
/// {synType} option
let optionOf synType = genericType true "option" [ synType ]

/// Expression for generic Task type:
/// Task<{synTypes}>
let taskOf synTypes = genericType false "Task" synTypes

/// Expression for generic Choice type:
/// Choice<{synTypes}>
let choiceOf synTypes = genericType false "Choice" synTypes

/// Expression for generic Result type:
/// Result<{synTypes}>
let resultOf synTypes = genericType false "Result" synTypes

/// Expression for creating tuple type:
/// {synType0} * {synType1} * {synType2} * ...
let tuple synTypes =
    let types =
        synTypes
        |> List.map (fun synType -> false, synType)

    SynType.Tuple(false, types, r)

// Primitive type expressions
let unitType = synType "unit"
let objType = synType "obj"
let boolType = synType "bool"
let intType = synType "int"
let int64Type = synType "int64"
let doubleType = synType "double"
let stringType = synType "string"
let byteType = synType "byte"
let dateTimeType = synType "System.DateTime"
let dateTimeOffsetType = synType "System.DateTimeOffset"
let guidType = synType "System.Guid"
let uriType = synType "System.Uri"
let nodaTypes =
    {|
        Instant = synType "NodaTime.Instant"
        LocalDate = synType "NodaTime.LocalDate"
        LocalTime = synType "NodaTime.LocalTime"
        LocalDateTime = synType "NodaTime.LocalDateTime"
        OffsetDateTime = synType "NodaTime.OffsetDateTime"
        ZonedDateTime = synType "NodaTime.ZonedDateTime"
        Offset = synType "NodaTime.Offset"
        Duration = synType "NodaTime.Duration"
        Period = synType "NodaTime.Period"
        DateTimeZone = synType "NodaTime.DateTimeZone"
    |}

/// Expression for field in records
/// {name}: {fieldType}
let field name fieldType =
    SynField.Field([], false, Some(ident name), fieldType, false, PreXmlDoc.Empty, None, r)
   
/// Tuple pattern (for arg list or matching) 
let tuplePat args =
    SynPat.Paren(SynPat.Tuple(false, args |> List.map (fun v -> SynPat.Named(SynPat.Wild(r), ident v, false, None, r)), r), r)

/// Expression for implementing any member kind in class
let implDefn memberKind isOverride name args expr =
    let methodArgs =
        if args = ["()"] then
            SynValInfo([[emptyArg]], emptyArg)
        else curriedArgs args
    let ctorArgs =
        if args = ["()"] then
            Pats [tuplePat []]
        else ctorArgs args
    SynMemberDefn.Member
        (SynBinding.Binding
            (None,
             SynBindingKind.NormalBinding,
             false,
             false,
             [],
             PreXmlDoc.Empty,
             SynValData
                 (Some
                     { IsInstance = true
                       IsDispatchSlot = false
                       IsOverrideOrExplicitImpl = isOverride
                       IsFinal = false
                       MemberKind = memberKind },
                  methodArgs,
                  None),
             SynPat.LongIdent(longIdentWithDots ("this." + name), None, None, ctorArgs, None, r),
             None,
             expr,
             r,
             NoDebugPointAtInvisibleBinding),
         r)

/// Expression for override method
/// override __.{name} {args} = {expr}
let methodImplDefn name args expr =
    implDefn MemberKind.Member true name args expr

/// Expression for override getter
/// override __.{name} = {expr}
let propertyImplDefn name expr =
    implDefn MemberKind.PropertyGet true name [] expr

/// Module type declarations
let types typeDefinitions = SynModuleDecl.Types(typeDefinitions, r)

/// Creating PreXmlDocs from line list
let xmlDocs lines =
    let lines = List.collect id lines
    if List.isEmpty lines then
        PreXmlDoc.Empty
    else
        let collector = XmlDocCollector()

        let i =
            List.fold (fun i line ->
                collector.AddXmlDocLine(line, mkPos i 0)
                i + 1) 0 lines

        PreXmlDoc.PreXmlDoc(mkPos i 0, collector)

/// curried function call with multiple args
/// {name} {args[0]} {args[1]} ...
let curriedCall name args =
    if List.isEmpty args
    then failwith "Can't invoke thisCallInputNextCtx with empty arg list"

    let rec inner argsRest resultSoFar =
        match argsRest with
        | [] -> resultSoFar
        | arg :: rest -> identExpr arg |> app resultSoFar |> inner rest

    longIdentExpr name |> inner args

/// Tuple expression
/// ({args[0]}, {args[1]}, ...)
let tupleComplexExpr args =
    SynExpr.Tuple(false, args, [ r ], r) |> paren
/// Tuple expression
/// ({args[0]}, {args[1]}, ...)
let tupleExpr args =
    tupleComplexExpr (List.map identExpr args)

/// Simple match clause expr
/// | {pat} -> {expr}
let clause pat expr =
    SynMatchClause.Clause(pat, None, expr, r, DebugPointForTarget.Yes)

/// Match expression
/// match {what} with {clauses}
let matchExpr what clauses =
    SynExpr.Match(DebugPointAtBinding r, identExpr what, clauses, r)

/// Infix right-associative operator for creating match clauses
let inline (^=>) a b = clause a b

let simpleValueMatching value cases =
    let clauses =
        cases
        |> List.map
            (
                fun (v, into, res) ->
                    SynPat.LongIdent(longIdentWithDots v, None, None, [SynPat.Named(SynPat.Wild(r), ident into, false, None, r)] |> Pats, None, r) ^=> res
            )
    matchExpr value clauses
let inline (^|>) a b = app (appI (identExpr "op_PipeRight") a) b
let inline (^>>) a b = paren (app (appI (identExpr "op_ComposeRight") a) b)
let inline (^=) a b = app (appI (identExpr "op_Equals") a) b

let setStatusCodeExpr code =
    app (identExpr "setStatusCode") (intExpr code)

let inline (^) f x = f x