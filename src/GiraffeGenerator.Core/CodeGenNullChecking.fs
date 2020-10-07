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

let private mapSeqMapOpt = "mapSeqMapOpt"
let private bindSeqMapOpt = "bindSeqMapOpt"
let private mapSeqBindOpt = "mapSeqBindOpt"
let private bindSeqBindOpt = "bindSeqBindOpt"
let private checkNulls = "checkNulls"
let private ofOption = "ofOption"
let private ofValue = "ofValue"
let private ofObj = "ofObj"

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
              | TypeKind.Array kind ->
                  for l in enumeratePaths prevPath kind do
                      let l = l |> List.mapi (fun i v -> i,v)
                      [
                          for i,v in l do
                            match prevPath.Length - i with
                            | 2 ->
                                yield v
                                yield NullableValue
                            | 1 -> yield CollectionValue v
                            | _ -> yield v
                      ]
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
        
    let mapSeqMapOpt = identExpr mapSeqMapOpt |> app
    let bindSeqMapOpt = identExpr bindSeqMapOpt |> app
    let mapSeqBindOpt = identExpr mapSeqBindOpt |> app
    let bindSeqBindOpt = identExpr bindSeqBindOpt |> app
    let checkNulls = identExpr checkNulls
    let ofOption = identExpr ofOption
    let ofValue = identExpr ofValue
    let ofObj = longIdentExpr ofObj
    let rec analyze expr path isOption isCollection l =
        seq {
            // normalize value to seq<'a> guaranteed to be without nulls
            // h -> Property -> None does initial normalization
            // every other match assumes that expr is already non-option seq
            //     and treats isOption / isCollection as 'a modifiers instead of 'a seq modifiers
            match l with
            | (h::t) ->
                match h with
                | Property p ->
                    let path = [ yield! path; p ]
                    let path =
                        if isOption && isCollection then
                            [ yield! path; "[i]"; "?Value" ]
                        elif isOption then
                            [ yield! path; "?Value" ]
                        elif isCollection then
                            [ yield! path; "[i]" ]
                        else path
                    match expr with
                    | None ->
                        // initial normalization
                        let ident = identExpr p
                        let expr =
                            match (isOption, isCollection) with
                            // inputVariable: Option<'a> seq
                            | (true, true)   -> ident ^|> mapSeqMapOpt _id
                            // inputVariable: Option<'a>
                            | (true, false)  -> ident ^|> ofOption
                            // inputVariable: 'a seq. It is not known if it is nullable yet, so no checks or filters
                            | (false, true)  -> ident
                            // inputVariable: 'a
                            | (false, false) -> ident ^|> ofValue
                        yield! analyze (Some expr) path false false t
                    | Some expr ->
                        let mapProp = lambda (simplePats [simplePat "v"]) (longIdentExpr <| sprintf "v.%s" p)
                        let expr =
                            match (isOption, isCollection) with
                            // parent.Property: Option<'a> seq
                            | (true, true)   -> expr ^|> Seq.collectExpr mapProp ^|> mapSeqMapOpt _id
                            // parent.Property: 'a option
                            | (true, false)  -> expr ^|> Seq.mapExpr mapProp ^|> mapSeqMapOpt _id
                            // parent.Property: 'a seq
                            | (false, true)  -> expr ^|> Seq.collectExpr mapProp
                            // parent.Property: 'a
                            | (false, false) -> expr ^|> Seq.mapExpr mapProp
                        yield! analyze (Some expr) path false false t
                | OptionValue o ->
                    yield! analyze expr path true isCollection [o;yield!t]
                | CollectionValue c ->
                    yield! analyze expr path isOption true [c;yield!t]
                | NullableValue ->
                    let expr = expr |> Option.defaultWith (fun _ -> failwith "There should be at some access to a value before the value itself")
                    let expr =
                        // normalize to seq<'nullable>
                        match (isOption, isCollection) with
                        // expr: seq<Option<'nullable> seq>
                        | (true, true)   -> expr ^|> bindSeqBindOpt _id
                        // expr: seq<'nullable option>
                        | (true, false)  -> expr ^|> mapSeqBindOpt _id
                        // expr: seq<'nullable seq>
                        | (false, true)  -> expr ^|> Seq.collectExpr _id
                        // expr: seq<'nullable>
                        | (false, false) -> expr
                    // the value is nullable, so check it
                    yield (path, expr ^|> checkNulls)
                    // and continue the analysis skipping the nulls
                    let goDeeperExpr = expr ^|> Seq.mapExpr ofObj ^|> mapSeqMapOpt _id
                    yield! analyze (Some goDeeperExpr) path false false t
            | _ -> ()
        }
    enumeratePaths [Property sourceVar] schema.Kind
    |> Seq.collect (analyze None [] false false)
    |> Seq.distinctBy fst
    |> Seq.map ^ fun (path, expr) ->
            let typeName = getOwnName schema.Kind (fun _ -> schema.Name) |> strExpr
            let pathList = SynExpr.ArrayOrList(true, path |> List.skip 1 |> List.map strExpr, r)
            let checker = lambda (simplePats [SynSimplePat.Typed(simplePat sourceVar, extractResponseSynType schema.Kind, r)]) expr
            tupleComplexExpr [typeName; pathList; checker]
     |> Seq.toList
     |> fun exprs -> if exprs.Length > 0 then Some <| SynExpr.ArrayOrList(true, exprs, r) else None       

let private defaultWithEmptySeq = Option.defaultValueExpr (longIdentExpr "Seq.empty")
let private checkForUnexpectedNullsName = "checkForUnexpectedNulls"
let generateNullCheckingHelpers () =
    [
        // mapSeqMapOpt
        letDecl false mapSeqMapOpt ["map"; "so"] None
            ^ identExpr "so"
            ^|> Seq.mapExpr (paren (Option.mapExpr (identExpr "map")))
            ^|> seqChooseId
        // bindSeqMapOpt
        letDecl false bindSeqMapOpt ["map"; "so"] None
            ^ identExpr "so"
            ^|> Seq.collectExpr (Option.mapExpr (identExpr "map") ^>> defaultWithEmptySeq)
        // mapSeqBindOpt
        letDecl false mapSeqBindOpt ["map"; "so"] None
            ^ identExpr "so"
            ^|> Seq.mapExpr (paren (Option.bindExpr (identExpr "map")))
            ^|> seqChooseId
        // bindSeqBindOpt
        letDecl false bindSeqBindOpt ["map"; "so"] None
            ^ identExpr "so"
            ^|> app (identExpr bindSeqMapOpt) (identExpr "map")
            ^|> seqChooseId
        let isNullReference = "isNullReference"
        letDecl false isNullReference ["v"] None // (=) doesn't work because F# thinks that C# can't fuck it up on nulls
            ^ app (longIdentExpr "System.Object.ReferenceEquals")
                (tupleComplexExpr [ identExpr "v"; SynExpr.Null(r) ])
        // checkNulls
        letDecl false checkNulls ["s"] None
            ^ identExpr "s"
            ^|> Seq.mapExpr (identExpr isNullReference)
        // ofObj
        letDecl false ofObj ["v"] None
            ^ ifElseExpr (app (identExpr isNullReference) (identExpr "v"))
                (identExpr "None")
                (app (identExpr "Some") (identExpr "v"))
        // ofValue
        letDecl false ofValue ["v"] None
            ^ app (app (longIdentExpr "Seq.replicate") (constExpr (SynConst.Int32 1))) (identExpr "v")
        // ofOption
        letDecl false ofOption ["v"] None
            ^ identExpr "v"
            ^|> Option.mapExpr (identExpr ofValue)
            ^|> defaultWithEmptySeq
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
