module CodeGenErrorsDU
open AST
open ASTExt
open FSharp.Compiler.SyntaxTree
open OpenApi
    
// generate binding errors DU and stringifiers
let errInnerTypeName = "ArgumentError"
let errInnerGiraffeBinding = "GiraffeBindingError"
let errInnerFormatterBindingExn = "FormatterBindingException"
let errInnerCombined = "CombinedArgumentErrors"
let private summary v = Some { Summary = Some [v]; Example = None; Remarks = None; }
let private errInnerType =
  DU {
      Name = errInnerTypeName
      Docs = summary "Input binding error"
      Cases =
          [
              {
                  CaseName = Some errInnerGiraffeBinding
                  Docs = summary "Giraffe returned a Result.Error from tryBindXXX"
                  Kind = Prim <| PrimTypeKind.String (StringFormat.String None)
              }
              {
                  CaseName = Some errInnerFormatterBindingExn
                  Docs = summary "Exception occurred during IFormatter bind"
                  Kind = BuiltIn "exn"
              }
              {
                  CaseName = Some errInnerCombined
                  Docs = summary "Multiple errors occurred in one location"
                  Kind = TypeKind.Array (DU { Name = errInnerTypeName; Docs = None; Cases = [] }, None, None)
              }
          ]
  }
let errOuterTypeName = "ArgumentLocationedError"
let errOuterBody = "BodyBindingError"
let errOuterQuery = "QueryBindingError"
let errOuterPath = "PathBindingError"
let errOuterCombined = "CombinedArgumentLocationError"
let private errOuterType =
  DU {
      Name = errOuterTypeName
      Docs = summary "Location argument error"
      Cases =
          [
              {
                  CaseName = Some errOuterBody
                  Docs = summary "Body error"
                  Kind = errInnerType
              }
              {
                  CaseName = Some errOuterQuery
                  Docs = summary "Query error"
                  Kind = errInnerType
              }
              {
                  CaseName = Some errOuterPath
                  Docs = summary "Path error"
                  Kind = errInnerType
              }
              {
                  CaseName = Some errOuterCombined
                  Docs = summary "Multiple locations errors"
                  Kind = TypeKind.Array (DU { Name = errOuterTypeName; Docs = None; Cases = [] }, None, None)
              }
          ]
  }

let innerErrToStringName = "argErrorToString"
let private levelParam = "level"
let private valueParam = "value"
let private sepVar = "sep"
let private err = "err"
let private nextLevel = app (appI (identExpr "op_Addition") (identExpr levelParam)) (SynConst.Int32 1 |> constExpr)
let private letSep =
  letExpr
      sepVar
      []
      (
          app
              (longIdentExpr "System.String")
              (
                  tupleComplexExpr
                    [
                        SynConst.Char ' ' |> constExpr
                        app (appI (identExpr "op_Multiply") (identExpr levelParam)) (SynConst.Int32 2 |> constExpr)
                    ]
              )
      )

let private innerErrToStringDecl =
  letDecl true innerErrToStringName [levelParam; valueParam] None
  ^ letSep
  ^ simpleValueMatching valueParam
    [
        errInnerGiraffeBinding, err, sprintfExpr "%sGiraffe binding error: %s" [identExpr sepVar; identExpr err]
        errInnerFormatterBindingExn, err, longIdentExpr (sprintf "%s.Message" err)
        errInnerCombined, err,
            sprintfExpr "%sMultiple errors:\\n%s"
            ^ [identExpr sepVar
               app
                (String.concatExpr "\\n")
                (
                    app // Seq.map (recCall (level + 1))
                        (app (longIdentExpr "Seq.map") (paren(app (identExpr innerErrToStringName) (paren(nextLevel)))))
                        (identExpr err)
                    |> paren
                )
               |> paren
            ]
    ]

let private callInnerWithFormat format var =
  sprintfExpr format [ identExpr sepVar; paren ( app (app (identExpr innerErrToStringName) (paren nextLevel)) (identExpr var)) ]

let outerErrToStringName = "argLocationErrorToString"
let private outerErrToStringDecl =
  letDecl true outerErrToStringName [levelParam; valueParam] None
  ^ letSep
  ^ simpleValueMatching valueParam
    [
        let common =
            [
                errOuterBody, "body"
                errOuterPath, "path"
                errOuterQuery, "query"
            ]
        for (case, var) in common do
            let uppercase = System.String([| System.Char.ToUpperInvariant var.[0]; yield! var |> Seq.skip 1 |])
            let format = sprintf "%%s%s binding error:\\n%%s" uppercase
            case, var, (callInnerWithFormat format var)
            
        errOuterCombined, err,
             sprintfExpr "%sMultiple binding errors:\\n%s"
             ^ [identExpr sepVar
                app
                 (String.concatExpr "\\n\\n")
                 (
                     app // Seq.map (recCall (level + 1))
                         (app (longIdentExpr "Seq.map") (paren(app (identExpr outerErrToStringName) (paren(nextLevel)))))
                         (identExpr err)
                     |> paren
                 )
                |> paren
             ]
    ]

let tryExtractErrorName = "tryExtractError"
let private tryExtractErrorDecl =
    let value = "value"
    letDecl false tryExtractErrorName [value] None
    ^ matchExpr value
        [
            clause (SynPat.LongIdent(longIdentWithDots "Ok", None, None, [SynPat.Wild r] |> Pats, None, r)) (identExpr "None")
            clause (SynPat.LongIdent(longIdentWithDots "Error", None, None, [SynPat.Named(SynPat.Wild(r), ident "err", false, None, r)] |> Pats, None, r))
                ^ app (identExpr "Some") (identExpr "err") 
        ]

let typeSchemas =
    [ errInnerTypeName, errInnerType
      errOuterTypeName, errOuterType ]
    |> List.map (fun (name, kind) -> { DefaultValue = None; Name = name; Kind = kind; Docs = None })

let generateHelperFunctions () =
    [
        innerErrToStringDecl
        outerErrToStringDecl
        tryExtractErrorDecl
    ]