module CodeGen

open AST
open OpenApi
open Fantomas
open FSharp.Compiler.Ast

let inline (^) f x = f x

/// helper function to create partial Giraffe flow:
/// {method.Method} >=> service.{method.Name}
let createMethod (method: PathMethodCall) =
    method.Method.ToUpperInvariant() |> identExpr >=> service method.Name

/// helper function to create Giraffe flows:
/// route {path.Route} >=> {method.Method} >=> service.{method.Name}
/// or
/// route {path.Route} >=> choose [
///     {method1.Method} >=> service.{method1.Name}
///     {method2.Method} >=> service.{method2.Name}
/// ]
let createRoute (path: ParsedPath) =
    let methods = 
        if path.Methods.Length > 1 then
            chooseExpr [ for method in path.Methods -> createMethod method ]
        else
            createMethod path.Methods.[0]
    route path.Route >=> methods


/// Creating whole module AST for Giraffe webapp
let giraffeAst (api: Api) =
    moduleDecl api.Name
        [ openDecl "FSharp.Control.Tasks.V2.ContextInsensitive"
          openDecl "Giraffe"

          let abstractMembers =
              [ for path in api.Paths do
                    for method in path.Methods do
                        yield abstractHttpHandler method.Name ]
          typeDecl "Service" abstractMembers

          let routes = api.Paths |> List.map createRoute
              
          let routesExpr =
              if routes.Length > 1 then
                  chooseExpr routes
              else
                  routes.[0]

          letHttpHandlerDecl "webApp" ^ taskBuilder ^ letGetServiceDecl ^ returnBang ^ appNextCtx routesExpr ]

/// Creating source code string from provided AST using default Fantomas settings
let sourceCode ast =
    
    let dummyInput =
        ParsedImplFileInput.ParsedImplFileInput
            ("", false, QualifiedNameOfFile.QualifiedNameOfFile(Ident("", r)), [], [], [ ast ], (true, true))
    
    let dummyImplFile = ParsedInput.ImplFile(dummyInput)
    
    CodeFormatter.FormatASTAsync(dummyImplFile, "abc.fs", [], None, Fantomas.FormatConfig.FormatConfig.Default)
    |> Async.RunSynchronously
