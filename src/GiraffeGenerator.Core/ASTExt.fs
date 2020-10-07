module ASTExt

open AST

let _id = identExpr "id"
let seqChooseId = app (longIdentExpr "Seq.choose") _id
module Seq =
    let collectExpr = longIdentExpr "Seq.collect" |> app
    let mapExpr = longIdentExpr "Seq.map" |> app
    let filterExpr = longIdentExpr "Seq.filter" |> app
module Option =
    let bindExpr = longIdentExpr "Option.bind" |> app
    let mapExpr = longIdentExpr "Option.map" |> app
    let defaultValueExpr = longIdentExpr "Option.defaultValue" |> app
module Result =
    let mapExpr = longIdentExpr "Result.map" |> app
    let bindExpr = longIdentExpr "Result.bind" |> app
    let mapErrorExpr = longIdentExpr "Result.mapError" |> app
