module AST

open FSharp.Compiler.Ast

/// Zero range. Used pretty much everywhere
let r = FSharp.Compiler.Range.range0

/// Range with one line space for indentation (used in list comprehensions)
let r1 = FSharp.Compiler.Range.rangeN "" 1

/// expression for ()
let unitExpr = SynExpr.Const(SynConst.Unit, r)

/// Ident with 0-range from string
let ident name = Ident(name, r)

/// LongIdent with splitting incoming string by '.'
let longIdent (name: string): LongIdent =
    name.Split([| '.' |])
    |> Seq.map ident
    |> Seq.toList

/// LongIdentWithDots with splitting incoming string by '.'
let longIdentWithDots name =
    LongIdentWithDots(longIdent name, [])

/// Open declaration with splitting incoming string by '.'
let openDecl name =
    SynModuleDecl.Open(longIdentWithDots name, r)

/// Expression for type
let synType name =
    SynType.LongIdent(longIdentWithDots name)

/// Argument expression
let arg name =
    SynArgInfo.SynArgInfo([], false, Some(ident name))

/// Expression which represents empty argument
let emptyArg =
    SynArgInfo.SynArgInfo([], false, None)

/// Expression which represents empty argument block for properties
let propertyVal =
    SynValInfo.SynValInfo([], emptyArg)
    
/// Expression which represents empty argument block for methods
let emptyMethodVal =
    SynValInfo.SynValInfo([[emptyArg]], emptyArg)    

/// Expression for named pattern {name}
let synPat name =
    SynPat.Named(SynPat.Wild(r), ident name, false, None, r)

/// Expression for multiple args in ctor with patterns
let ctorArgs args =
    Pats <| List.map synPat args

/// Expression which represents curried argument block
/// e.g. {arg1} {arg2} ...
let curriedArgs argList =
    let args =
        List.map (arg >> List.singleton) argList
    SynValInfo.SynValInfo(args, emptyArg)

/// Module declaration with splitting incoming string by '.'
/// and with [<RequireQualifiedAccess>] attribute
let moduleDecl name decls =
    let moduleName = longIdent name

    let attrib =
        [ { SynAttributeList.Attributes =
                [ { SynAttribute.Range = r
                    TypeName = longIdentWithDots "RequireQualifiedAccess"
                    ArgExpr = unitExpr
                    Target = None
                    AppliesToGetterAndSetter = false } ]
            Range = r } ]
    SynModuleOrNamespace.SynModuleOrNamespace
        (moduleName, false, SynModuleOrNamespaceKind.NamedModule, decls, PreXmlDoc.Empty, attrib, None, r)


/// Abstract member declaration
let abstractDfn memberKind valDfn name synType: SynMemberDefn =
    SynMemberDefn.AbstractSlot
        (SynValSig.ValSpfn
            ([], ident name, SynValTyparDecls.SynValTyparDecls([], true, []), synType, valDfn, false, false,
             PreXmlDoc.Empty, None, None, r),
         { IsInstance = true
           IsDispatchSlot = true
           IsOverrideOrExplicitImpl = false
           IsFinal = false
           MemberKind = memberKind }, r)

/// Abstract method member declaration
/// abstract {name}: {synType}
let abstractMemberDfn name synType = abstractDfn MemberKind.Member emptyMethodVal name synType

/// Abstract member declaration returning HttpHandler
/// abstract {name}: HttpHandler
let abstractHttpHandler name: SynMemberDefn =
    abstractDfn MemberKind.PropertyGet propertyVal name (synType "HttpHandler")

/// Expression for implementing any member kind in class
let implDefn memberKind isOverride name args expr =
    SynMemberDefn.Member
        (SynBinding.Binding
            (None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty,
             SynValData
                 (Some
                     { IsInstance = true
                       IsDispatchSlot = false
                       IsOverrideOrExplicitImpl = isOverride
                       IsFinal = false
                       MemberKind = memberKind }, curriedArgs args, None),
             SynPat.LongIdent(longIdentWithDots ("this." + name), None, None, ctorArgs args, None, r), None, expr, r,
             NoSequencePointAtInvisibleBinding), r)

/// Expression for override method
/// override __.{name} {args} = {expr}
let methodImplDefn name args expr =
    implDefn MemberKind.Member true name args expr

/// Expression for override getter
/// override __.{name} = {expr}
let propertyImplDefn name expr =
    implDefn MemberKind.PropertyGet true name [] expr

/// Expression for creating single attribute
let attr name: SynAttribute =
    { TypeName = longIdentWithDots name
      ArgExpr = unitExpr
      Target = None
      AppliesToGetterAndSetter = false
      Range = r }

/// Type declaration with provided type name and members
let abstractClassDecl name members =
    let componentInfo =
        SynComponentInfo.ComponentInfo
            ([ { Attributes = [ attr "AbstractClass" ]
                 Range = r } ], [], [], longIdent name, PreXmlDoc.Empty, false, None, r)

    let implicitCtor =
        SynMemberDefn.ImplicitCtor(None, [], SynSimplePats.SimplePats([],r),None, r)
    
    let objModel = SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconUnspecified, implicitCtor :: members, r)
    SynModuleDecl.Types([ TypeDefn(componentInfo, objModel, [], r) ], r)

/// Let declaration for Giraffe HttpHandler with specified name and body expression
/// E.g.:
/// let {NAME}: HttpHandler = fun next ctx -> {EXPR}
let letHttpHandlerDecl name expr =
    SynModuleDecl.Let
        (false,
         [ SynBinding.Binding
             (None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty,
              SynValData.SynValData(None, curriedArgs [ "next"; "ctx" ], None),
              SynPat.Named(SynPat.Wild(r), ident name, false, None, r),
              Some
                  (SynBindingReturnInfo.SynBindingReturnInfo(SynType.LongIdent(longIdentWithDots "HttpHandler"), r, [])),
              SynExpr.Typed
                  (SynExpr.Lambda
                      (false, false,
                       SynSimplePats.SimplePats([ SynSimplePat.Id(ident "next", None, false, false, false, r) ], r),
                       SynExpr.Lambda
                           (false, true,
                            SynSimplePats.SimplePats([ SynSimplePat.Id(ident "ctx", None, false, false, false, r) ], r),
                            expr, r), r), SynType.LongIdent(longIdentWithDots "HttpHandler"), r), r,
              NoSequencePointAtLetBinding) ], r)

/// Expression for calling:
/// ctx.GetService<Service>()
let getServiceCall =
    SynExpr.App
        (ExprAtomicFlag.Atomic, false,
         SynExpr.TypeApp
             (SynExpr.LongIdent(false, longIdentWithDots "ctx.GetService", None, r), r,
              [ SynType.LongIdent(longIdentWithDots "Service") ], [], None, r, r), unitExpr, r)

/// Let expression with continuation for calling:
/// let service = ctx.GetService<Service>() in {NEXT}
let letGetServiceDecl next =
    SynExpr.LetOrUse
        (false, false,
         [ SynBinding.Binding
             (None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, SynValData(None, emptyMethodVal, None),
              SynPat.Named(SynPat.Wild(r), ident "service", false, None, r), None, getServiceCall, r,
              SequencePointAtBinding(r)) ], next, r)

/// Expression for task builder with body:
/// task { body }
let taskBuilder body =
    SynExpr.App
        (ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(ident "task"), SynExpr.CompExpr(false, ref false, body, r), r)

/// Expression for ReturnFrom (return!) with continuation:
/// return! {expr}
let returnBang expr =
    SynExpr.YieldOrReturnFrom((false, true), expr, r)

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

/// Expression for Ident
let identExpr name = SynExpr.Ident(ident name)

/// Expression for empty Ident with non-zero range for proper indentation in list comprehensions
let emptyIdent = SynExpr.Ident(Ident("", r1))

/// Expression for LongIdent
let longIdentExpr name = SynExpr.LongIdent(false, longIdentWithDots name, None, r)

/// Sequential expression for two expressions (used in CE)
let seqExpr expr1 expr2 =
    SynExpr.Sequential(SequencePointsAtSeq, true, expr1, expr2, r)

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
let (>=>) e1 e2 = app (appI (identExpr "op_GreaterEqualsGreater") e1) e2

/// Constant string expression
let strExpr str = SynExpr.Const(SynConst.String(str, r), r)

/// Application expression for Giraffe route function
/// route {route}
let route route =
    app (identExpr "route") (strExpr route)

/// Expression for calling methods from service:
/// service.{name}
let service name = longIdentExpr ("service." + name)

/// Expression for record with fields
let record name fieldList =
    TypeDefn
        (SynComponentInfo.ComponentInfo([], [], [], longIdent name, PreXmlDoc.Empty, false, None, r),
         SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(None, fieldList, r), r), [], r)


/// Expression for generic type
let genericType isPostfix name synTypes = 
    SynType.App(SynType.LongIdent(longIdentWithDots name), None, synTypes, [], None, isPostfix, r)
 
/// Expression for generic array type in postfix notation:
/// {synType} array
let arrayOf synType =
    genericType true "array" [synType]

/// Expression for generic option type in postfix notation:
/// {synType} optionа
let optionOf synType =
    genericType true "option" [synType]

/// Expression for generic Task type:
/// Task<{synTypes}>
let taskOf synTypes =
    genericType false "Task" synTypes

/// Expression for generic Choice type:
/// Task<{synTypes}>
let choiceOf synTypes =
    genericType false "Choice" synTypes

/// Expression for function type
/// {argType} -> {synType}
let funType argType returnType =
    SynType.Fun(argType, returnType, r)

let inline (^->) a b = funType a b

// Primitive type expressions
let unitType = synType "unit"
let boolType = synType "bool"
let intType = synType "int"
let int64Type = synType "int64"
let doubleType = synType "double"
let stringType = synType "string"
let byteType = synType "byte"
let dateType = synType "System.DateTimeOffset"
let guidType = synType "System.Guid"
let uriType = synType "System.Uri"

/// Expression for field in records
/// {name}: {fieldType}
let field name fieldType =
    SynField.Field([], false, Some(ident name), fieldType, false, PreXmlDoc.Empty, None, r)

/// Module type declarations
let types typeDefinitions = SynModuleDecl.Types(typeDefinitions, r)