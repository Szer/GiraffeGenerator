module CodeGenNullChecking

open AST
open ASTExt
open FSharp.Compiler.SyntaxTree
open OpenApi
open OpenApiToAstTypesMatchingAndConversion

type private PropertyPathSegment =
  | Property of string
  | NullableValue
  | CollectionValue of PropertyPathSegment
  | OptionValue of PropertyPathSegment

let private isNullReference = "isNullReference"
let private nullReferenceToOption = "nullReferenceToOption"
let private defaultWithEmptySeq = Option.defaultValueExpr (longIdentExpr "Seq.empty")
let private checkForUnexpectedNullsName = "checkForUnexpectedNulls"
let generateNullCheckingHelpers () =
    [
        // isNullReference
        letDecl false isNullReference ["v"] None // (=) doesn't work because F# thinks that C# can't fuck it up on nulls
            ^ app (longIdentExpr "System.Object.ReferenceEquals")
                (tupleComplexExpr [ identExpr "v"; SynExpr.Null(r) ])
        // nullReferenceToOption
        letDecl false nullReferenceToOption ["v"] None
            ^ ifElseExpr (app (identExpr isNullReference) (identExpr "v"))
                (identExpr "None")
                (app (identExpr "Some") (identExpr "v"))
        // checkForUnexpectedNulls
        let checkForUnexpectedNulls =
            let checkers = "checkers"
            let errType = "errType"
            let value = "value"
            let mapCheckers = "mapCheckers"
            letDecl false checkForUnexpectedNullsName [checkers; errType; value] None
            ^ letOrUseComplexParametersDecl mapCheckers (Pats [SynPat.Paren(tuplePat ["typeName"; "path"; "accessor"], r)])
                (
                     letOrUseComplexParametersDecl "v" (Pats [SynPat.Wild(r)])
                        (
                            app (identExpr CodeGenErrorsDU.errInnerModelBindingUnexpectedNull)
                                (
                                    recordExpr
                                        [
                                            "TypeName", identExpr "typeName"
                                            "PropertyPath", identExpr "path"
                                        ]
                                )
                        )
                        (
                            app (identExpr "accessor") (identExpr value)
                            ^|> Seq.filterExpr _id
                            ^|> Seq.mapExpr (identExpr "v")
                        )
                )
                (
                    identExpr checkers
                    ^|> Seq.collectExpr (identExpr mapCheckers)
                    ^|> longIdentExpr "Seq.toArray"
                    ^|> lambda (simplePats[simplePat "v"])
                        (
                            ifElseExpr (app (appI (identExpr "op_Equals") (longIdentExpr "v.Length")) (constExpr (SynConst.Int32 0)))
                                (app (identExpr "Ok") (identExpr value))
                                (
                                    ifElseExpr
                                        (app (appI (identExpr "op_GreaterThan") (longIdentExpr "v.Length")) (constExpr (SynConst.Int32 1)))
                                        (identExpr "v" ^|> identExpr CodeGenErrorsDU.errInnerCombined)
                                        (identExpr "v" ^|> longIdentExpr "Array.head")
                                    ^|> identExpr errType
                                    ^|> identExpr "Error"
                                )
                        )
                )
        checkForUnexpectedNulls
    ]

type private Modifier =
    | Arr
    | Opt
    | Nul
module private Modifiers =
    let private notNull = Seq.mapExpr (longIdentExpr nullReferenceToOption) ^|> Seq.chooseExpr _id
    let rec mapExprToSeqOfB currentMods expr =
        match currentMods with
        | h::[] ->
            match h with
            | Arr -> expr ^|> Seq.collectExpr _id
            | Opt -> expr ^|> Seq.chooseExpr _id
            | Nul -> expr ^|> notNull
        | [] -> expr
        | h1::h2::t ->
            let f =
                match h1,h2 with
                | Arr,Arr -> Seq.collectExpr _id ^|> Seq.collectExpr _id
                | Opt,Opt -> failwith "'a option option should never be generated"
                | Arr,Opt -> Seq.collectExpr _id ^|> Seq.chooseExpr _id
                | Opt,Arr -> Seq.chooseExpr _id ^|> Seq.collectExpr _id
                | Nul,Arr -> notNull ^|> Seq.collectExpr _id
                | Arr,Nul -> Seq.collectExpr _id ^|> notNull
                | Opt,Nul -> Seq.chooseExpr _id ^|> notNull
                | Nul,Opt -> failwith "Option nullcheck should not be generated as options can't be nulls by occasion (or should not be generated in cases when the value can)"
                | Nul,Nul -> failwith "Double nullcheck should not be generated as it has no meaning"
            let expr = expr ^|> f
            mapExprToSeqOfB t expr

/// Generate checks for unexpected nulls
let private generateNullCheckersArray sourceVar (schema: TypeSchema) =
    let rec enumeratePaths prevPath kind =
        seq {
            match kind with
            | TypeKind.Object o ->
                yield!
                    o.Properties
                    |> Seq.collect ^ fun (name, kind, _) ->
                        let newPath = [yield! prevPath; NullableValue; Property name]
                        enumeratePaths newPath kind
              | TypeKind.Array (kind, _) ->
                  for l in enumeratePaths prevPath kind do
                      List.mapi (fun i v -> if i + 1 = prevPath.Length then CollectionValue v else v) l
              | TypeKind.Option kind ->
                  for l in enumeratePaths prevPath kind do
                      List.mapi (fun i v -> if i + 1 = prevPath.Length then OptionValue v else v) l
              | TypeKind.Prim p ->
                  let isNullable =
                      match p with
                      | PrimTypeKind.Any -> true
                      | PrimTypeKind.String s ->
                          match s with
                          | StringFormat.DateString
                          | StringFormat.DateTimeString
                          | StringFormat.Custom _ -> false
                          | _ -> true
                      | _ -> false
                  if isNullable then
                      [yield! prevPath; NullableValue]
              | _ -> prevPath
        }
    let rec analyze expr path mods (l: PropertyPathSegment list) =
        seq {
            // normalize value to seq<'a> guaranteed to be without nulls
            // h -> Property -> None does initial normalization
            // every other match assumes that expr is already non-option seq
            match l with
            | (h::t) ->
                match h with
                | OptionValue o ->
                    yield! analyze expr path [Opt; yield! mods] [o;yield!t]
                | CollectionValue c ->
                    let expr = expr |> Option.defaultWith (fun _ -> failwith "There should be at some access to a value before the value itself")
                    yield! analyze (Some expr) path [Nul; Arr; yield! mods] [c;yield!t]
                | Property p ->
                    let path =
                        [
                            yield! path
                            p
                            yield!
                                mods
                                |> Seq.map
                                       (
                                           function
                                           | Arr -> Some "[i]"
                                           | Opt -> Some "?Value"
                                           | Nul -> None
                                       )
                                |> Seq.choose id
                        ]
                    match expr with
                    | None ->
                        let ident = app (app (longIdentExpr "Seq.replicate") (constExpr <| SynConst.Int32 1)) (identExpr p)
                        yield! analyze (Some ident) path [] t
                    | Some expr ->
                        let propAccess = longIdentExpr <| sprintf "v.%s" p
                        let mapProp = lambda (simplePats [simplePat "v"]) propAccess
                        let expr = expr ^|> Seq.mapExpr mapProp
                        let expr = Modifiers.mapExprToSeqOfB mods expr
                        yield! analyze (Some expr) path [] t
                | NullableValue ->
                    let expr = expr |> Option.defaultWith (fun _ -> failwith "There should be at some access to a value before the value itself")
                    // the value is nullable, so check it
                    yield (path, Modifiers.mapExprToSeqOfB mods expr ^|> Seq.mapExpr (identExpr isNullReference))
                    // and continue the analysis
                    yield! analyze (Some expr) path [Nul; yield! mods] t
            | _ -> ()
        }
    enumeratePaths [Property sourceVar] schema.Kind
    |> Seq.collect (analyze None [] [])
    |> Seq.distinctBy fst
    |> Seq.map ^ fun (path, expr) ->
            let typeName = getOwnName schema.Kind (fun _ -> schema.Name) |> strExpr
            let pathList = SynExpr.ArrayOrList(true, path |> List.skip 1 |> List.map strExpr, r)
            let checker = lambda (simplePats [SynSimplePat.Typed(simplePat sourceVar, extractResponseSynType schema.Kind, r)]) expr
            tupleComplexExpr [typeName; pathList; checker]
     |> Seq.toList
     |> fun exprs -> if exprs.Length > 0 then Some <| SynExpr.ArrayOrList(true, exprs, r) else None       


let bindNullCheckIntoResult varName schema error expr =
    let nullCheckers = generateNullCheckersArray varName schema
    nullCheckers
    |> Option.map ^ fun checkers ->
        expr
        ^|> Result.bindExpr
        ^ paren
            (
                app (app (identExpr checkForUnexpectedNullsName) (checkers))
                    (identExpr error)
            ) 
    |> Option.defaultValue expr
