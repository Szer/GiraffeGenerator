module CodeGenErrorsDU
open AST
open FSharp.Compiler.SyntaxTree
open OpenApi
    
// generate binding errors DU and stringifiers
let errInnerTypeName = "ArgumentError"
let errInnerGiraffeBinding = "GiraffeBindingError"
let errInnerFormatterBindingExn = "FormatterBindingException"
let errInnerModelBindingUnexpectedNull = "ModelBindingUnexpectedNull"
let errInnerCombined = "CombinedArgumentErrors"
let private errInnerType =
  DU {
      Name = errInnerTypeName
      Docs = Some { Summary = Some ["Represents some concrete error somewhere in arguments"]; Example = None; Remarks = None }
      Cases =
          [
              {
                  CaseName = Some errInnerGiraffeBinding
                  Docs = Some { Summary = Some ["Giraffe error returned in Result.Error of tryBindXXX method"]; Example = None; Remarks = None }
                  Kind = Prim <| PrimTypeKind.String StringFormat.String
              }
              {
                  CaseName = Some errInnerFormatterBindingExn
                  Docs = Some { Summary = Some [ "Represents exception occured during IFormatter bind" ]; Example = None; Remarks = None }
                  Kind = BuiltIn "exn"
              }
              {
                  CaseName = Some errInnerModelBindingUnexpectedNull
                  Docs = Some { Summary = Some ["For IFormatter bind (JSON, for example), represents a null that happens to exist because no value was provided for a property which is required"]; Example = None; Remarks = None }
                  Kind = TypeKind.Object
                      {
                          Name = None
                          Properties =
                              [
                                  "TypeName", Prim (PrimTypeKind.String StringFormat.String), None
                                  "PropertyPath", TypeKind.Array (Prim <| PrimTypeKind.String StringFormat.String, None), None
                              ]
                          Docs = None
                      }
              }
              {
                  CaseName = Some errInnerCombined
                  Docs = Some { Summary = Some ["Represents multiple errors occured in one location"]; Example = None; Remarks = None }
                  Kind = TypeKind.Array (DU { Name = errInnerTypeName; Docs = None; Cases = [] }, None)
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
      Docs = Some { Summary = Some ["Represents error in arguments of some location"]; Example = None; Remarks = None; }
      Cases =
          [
              {
                  CaseName = Some errOuterBody
                  Docs = Some { Summary = Some ["Represents error in body"]; Example = None; Remarks = None; }
                  Kind = errInnerType
              }
              {
                  CaseName = Some errOuterQuery
                  Docs = Some { Summary = Some ["Represents error in query"]; Example = None; Remarks = None; }
                  Kind = errInnerType
              }
              {
                  CaseName = Some errOuterPath
                  Docs = Some { Summary = Some ["Represents error in path"]; Example = None; Remarks = None; }
                  Kind = errInnerType
              }
              {
                  CaseName = Some errOuterCombined
                  Docs = Some { Summary = Some ["Represents errors in multiple locations"]; Example = None; Remarks = None; }
                  Kind = TypeKind.Array (DU { Name = errOuterTypeName; Docs = None; Cases = [] }, None)
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
        errInnerModelBindingUnexpectedNull, err,
            sprintfExpr "%sUnexpected null was found somewhere on path %s.%s"
            ^ [identExpr sepVar; longIdentExpr (sprintf "%s.TypeName" err); paren (app (app (longIdentExpr "String.concat") (strExpr ".")) (longIdentExpr (sprintf "%s.PropertyPath" err))) ]
        errInnerFormatterBindingExn, err, longIdentExpr (sprintf "%s.Message" err)
        errInnerCombined, err,
            sprintfExpr "%sMultiple errors:\\n%s"
            ^ [identExpr sepVar
               app
                (app (longIdentExpr "String.concat") (strExpr "\\n"))
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
                 (app (longIdentExpr "String.concat") (strExpr "\\n\\n"))
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