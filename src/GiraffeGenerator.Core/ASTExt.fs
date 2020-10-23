module ASTExt

open AST

let _id = identExpr "id"
let seqChooseId = app (longIdentExpr "Seq.choose") _id
module Seq =
    let collectExpr = longIdentExpr "Seq.collect" |> app
    let chooseExpr = longIdentExpr "Seq.choose" |> app
    let mapExpr = longIdentExpr "Seq.map" |> app
    let filterExpr = longIdentExpr "Seq.filter" |> app
    let reduceExpr reducer initial = app (app (longIdentExpr "Seq.reduce") reducer) initial

module Option =
    let bindExpr = longIdentExpr "Option.bind" |> app
    let mapExpr = longIdentExpr "Option.map" |> app
    let defaultValueExpr = longIdentExpr "Option.defaultValue" |> app
    let isSomeExpr = longIdentExpr "Option.isSome"
    
module Result =
    let mapExpr = longIdentExpr "Result.map" |> app
    let bindExpr = longIdentExpr "Result.bind" |> app
    let mapErrorExpr = longIdentExpr "Result.mapError" |> app

module String =
    let concatExprComplex delimiterExpr = app (longIdentExpr "String.concat") delimiterExpr
    let concatExpr delimiter = concatExprComplex (strExpr delimiter)