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
    
    let doc,_ = read inputFile
    let name, routes = parse doc
    
    let resultSource =
        giraffeAst name routes
        |> sourceCode
    File.WriteAllText(outputFile, resultSource)
    
    0
