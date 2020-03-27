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

/// Default MemberFlags for abstract members in Service interface declaration
let memberFlags: MemberFlags =
    { IsInstance = true
      IsDispatchSlot = true
      IsOverrideOrExplicitImpl = false
      IsFinal = false
      MemberKind = MemberKind.PropertyGet }

/// Member declaration as abstract method returning HttpHandler
let abstractHttpHandler name: SynMemberDefn =
    SynMemberDefn.AbstractSlot
        (SynValSig.ValSpfn
            ([], ident name, SynValTyparDecls.SynValTyparDecls([], true, []),
             SynType.LongIdent(longIdentWithDots "HttpHandler"),
             SynValInfo.SynValInfo([], SynArgInfo.SynArgInfo([], false, None)), false, false, PreXmlDoc.Empty, None,
             None, r), memberFlags, r)


/// Type declaration with provided type name and members
let typeDecl name members =
    let componentInfo = SynComponentInfo.ComponentInfo([], [], [], longIdent name, PreXmlDoc.Empty, false, None, r)
    let objModel = SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.TyconUnspecified, members, r)
    SynModuleDecl.Types([ TypeDefn(componentInfo, objModel, [], r) ], r)

/// Let declaration for Giraffe HttpHandler with specified name and body expression
/// E.g.:
/// let {NAME}: HttpHandler = fun next ctx -> {EXPR} 
let letHttpHandlerDecl name expr =
    SynModuleDecl.Let
        (false,
         [ SynBinding.Binding
             (None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty,
              SynValData.SynValData
                  (None,
                   SynValInfo.SynValInfo
                       ([ [ SynArgInfo.SynArgInfo([], false, Some(ident "next")) ]
                          [ SynArgInfo.SynArgInfo([], false, Some(ident "ctx")) ] ], SynArgInfo([], false, None)), None),
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
             (None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty,
              SynValData(None, SynValInfo.SynValInfo([], SynArgInfo([], false, None)), None),
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
/// Will throw exception on expression list's size less than 3
let rec seqExprs exprs =
    match exprs with
    | []
    | [ _ ] -> failwith "imbosibru"
    | [ e1; e2 ] -> seqExpr e1 e2
    | e :: exprs -> seqExpr e (seqExprs exprs)

/// CE expression for expression list using Sequential
let ceExprList exprs =
    SynExpr.CompExpr(true, ref true, seqExprs exprs, r)
    
/// Expression for Giraffe choose function with list of expressions inside (no less than 3)
/// choose [ {exprList} ]
let chooseExpr exprList =
    app (app (app (identExpr "choose") (arrayExpr (ceExprList exprList))) (identExpr "next")) (identExpr "ctx")

/// Infix application expression for Giraffe Kleisli composition operator >=>
/// {e1} >=> {e2}
let (>=>) e1 e2 = app (appI (identExpr "op_GreaterEqualsGreater") e1) e2

/// Constant string expression
let strExpr str = SynExpr.Const(SynConst.String(str, r), r)

/// Application expression for Giraffe route function
/// route {route}
let route route =
    app (identExpr "route") (strExpr route)

/// Expression for Giraffe GET handler
let GET = identExpr "GET"

/// Expression for calling methods from service:
/// service.{name}
let service name = longIdentExpr ("service." + name)