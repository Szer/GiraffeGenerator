module Program

open OpenApi
open CodeGen
open System.IO

let getArg args name =
    try
        let i = 
            args
            |> Array.findIndex ((=) name)
        args.[i+1]
    with _ ->
        failwithf "Missing required argument %s" name

[<EntryPoint>]
let main argv =
    let inputFile = getArg argv "--inputfile"
    let outputFile = getArg argv "--outputfile"
    
    let doc, errors = read inputFile
    if errors <> null && errors.Errors <> null && errors.Errors.Count > 0 then 
        errors.Errors
        |> Seq.map (fun err -> sprintf "%s (at %s)" err.Message err.Pointer)
        |> String.concat "\n"
        |> failwith
    let api = parse doc
    
    let resultSource =
        giraffeAst api
        |> sourceCode
    File.WriteAllText(outputFile, resultSource)
    
    0
