module Program

open OpenApi
open CodeGen
open System.IO
open Argu

type DateTimeGeneratedType =
    | Zoned_Date_Time
    | Offset_Date_Time
    | Local_Date_Time
    | Instant
    member this.ToConfigType() =
        match this with
        | Zoned_Date_Time -> Configuration.DateTimeGeneratedType.ZonedDateTime
        | Offset_Date_Time -> Configuration.DateTimeGeneratedType.OffsetDateTime
        | Local_Date_Time -> Configuration.DateTimeGeneratedType.LocalDateTime
        | Instant -> Configuration.DateTimeGeneratedType.Instant

type Config =
    | [<Mandatory>]Inputfile of string
    | [<Mandatory>]Outputfile of string
    | Module_Name of string
    | Allow_Unqualified_Access
    | Use_Noda_Time
    | Map_Date_Time_Into of DateTimeGeneratedType
    
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Inputfile _ -> "Path to OpenAPI spec file to generate server implementation by"
            | Outputfile _ -> "Path to .fs file to write the source code generated"
            | Module_Name _ -> "Override module name for the server generated. Default is taken from the spec description"
            | Allow_Unqualified_Access -> "Opts-out [<RequireQualifiedAccess>] generation for the module being generated"
            | Use_Noda_Time -> "Opts-in usage of NodaTime types"
            | Map_Date_Time_Into _ -> "Specifies NodaTime type used for date-time OpenAPI format"

let parser = ArgumentParser.Create<Config>("GiraffeGenerator")

[<EntryPoint>]
let main argv =
    let parsed = parser.Parse argv
    let parsed = parsed.GetAllResults()
    let mutable inputFile = ""
    let mutable outputFile = ""
    
    for option in parsed do
        match option with
        | Inputfile file -> inputFile <- file
        | Outputfile file -> outputFile <- file
        | Module_Name name -> Configuration.value <- { Configuration.value with ModuleName = Some name }
        | Allow_Unqualified_Access -> Configuration.value <- { Configuration.value with AllowUnqualifiedAccess = true }
        | Use_Noda_Time -> Configuration.value <- { Configuration.value with UseNodaTime = true }
        | Map_Date_Time_Into kind -> Configuration.value <- { Configuration.value with MapDateTimeInto = kind.ToConfigType() }
    
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
