﻿module Program

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

let getOptionalFlag args name =
    args
    |> Array.tryFindIndex ((=) name)
    |> Option.map (fun _ -> true)

let getOptionalArg args name =
    args
    |> Array.tryFindIndex ((=) name)
    |> Option.map ((+) 1)
    |> Option.filter (fun i -> i < args.Length)
    |> Option.map (fun i -> args.[i])

[<EntryPoint>]
let main argv =
    let inputFile = getArg argv "--inputfile"
    let outputFile = getArg argv "--outputfile"
    
    let useNodaTimeSwitchName = "--use-noda-time"
    let mapDateTimeIntoSwitchName = "--map-date-time-into"
    
    let useNodaTime = getOptionalFlag argv useNodaTimeSwitchName |> Option.defaultValue false
    let dateTimeType =
        getOptionalArg argv mapDateTimeIntoSwitchName
        |> Option.map
            (
                function
                | "zoned-date-time" -> Configuration.DateTimeGeneratedType.ZonedDateTime
                | "offset-date-time" -> Configuration.DateTimeGeneratedType.OffsetDateTime
                | "local-date-time" -> Configuration.DateTimeGeneratedType.LocalDateTime
                | "instant" -> Configuration.DateTimeGeneratedType.Instant
                | v -> failwithf "unknown date time mapping: %s" v 
            )
    
    if not useNodaTime then
        if dateTimeType.IsSome then
            failwithf "%s may only be specified in conjunction with %s flag" mapDateTimeIntoSwitchName useNodaTimeSwitchName
    
    Configuration.value <- {
            UseNodaTime = useNodaTime
            // the non-noda behavior is DateTimeOffset, so take the most similar as default
            MapDateTimeInto = dateTimeType |> Option.defaultValue Configuration.DateTimeGeneratedType.OffsetDateTime
            ModuleName = getOptionalArg argv "--module-name"
        }
    
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
