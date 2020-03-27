module CodeGen

open AST
open OpenApi
open Fantomas
open FSharp.Compiler.Ast

let inline (^) f x = f x

/// helper function to create Giraffe flows:
/// route {path.Route} >=> {path.Method} >=> service.{path.Name}
let createRoute path =
    route path.Route >=> (path.Method.ToUpper() |> identExpr) >=> service path.Name

/// Creating whole module AST for Giraffe webapp
let giraffeAst name paths =
    moduleDecl name
        [ openDecl "FSharp.Control.Tasks.V2.ContextInsensitive"
          openDecl "Giraffe"

          let abstractMembers =
              paths |> List.map (fun x -> abstractHttpHandler x.Name)
          typeDecl "Service" abstractMembers

          let routes =
              paths |> List.map createRoute

          letHttpHandlerDecl "webApp" ^ taskBuilder ^ letGetServiceDecl ^ returnBang ^ chooseExpr
                                                                                           [ yield! routes
                                                                                             emptyIdent ] ]

/// Creating source code string from provided AST using default Fantomas settings
let sourceCode ast =
    
    let dummyInput =
        ParsedImplFileInput.ParsedImplFileInput
            ("", false, QualifiedNameOfFile.QualifiedNameOfFile(Ident("", r)), [], [], [ ast ], (true, true))
    
    let dummyImplFile = ParsedInput.ImplFile(dummyInput)
    
    CodeFormatter.FormatASTAsync(dummyImplFile, "abc.fs", [], None, Fantomas.FormatConfig.FormatConfig.Default)
    |> Async.RunSynchronously
