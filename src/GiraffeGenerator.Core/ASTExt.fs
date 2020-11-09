module ASTExt

open AST

let _id = identExpr "id"
let _not = identExpr "not"
let seqChooseId = app (longIdentExpr "Seq.choose") _id

module Array =
    let mapExpr = longIdentExpr "Array.map" |> app
    let lengthExpr = longIdentExpr "Array.length"
    let headExpr = longIdentExpr "Array.head"
    
module Seq =
    let collectExpr = longIdentExpr "Seq.collect" |> app
    let chooseExpr = longIdentExpr "Seq.choose" |> app
    let mapExpr = longIdentExpr "Seq.map" |> app
    let filterExpr = longIdentExpr "Seq.filter" |> app
    let reduceExpr reducer initial = app (app (longIdentExpr "Seq.reduce") reducer) initial
    let containsExpr = longIdentExpr "Seq.contains" |> app

module Option =
    let bindExpr = longIdentExpr "Option.bind" |> app
    let mapExpr = longIdentExpr "Option.map" |> app
    let map2Expr f a b = app (app (app (longIdentExpr "Option.map2") f) a) b
    let orElseExpr = longIdentExpr "Option.orElse" |> app
    let defaultValueExpr = longIdentExpr "Option.defaultValue" |> app
    let ofObjExpr = longIdentExpr "Option.ofObj" |> app
    let isSomeExpr = longIdentExpr "Option.isSome"
    
module Result =
    let mapExpr = longIdentExpr "Result.map" |> app
    let bindExpr = longIdentExpr "Result.bind" |> app
    let mapErrorExpr = longIdentExpr "Result.mapError" |> app

module String =
    let concatExprComplex delimiterExpr = app (longIdentExpr "String.concat") delimiterExpr
    let concatExpr delimiter = concatExprComplex (strExpr delimiter)