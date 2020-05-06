module Program

open FSharp.Control.Tasks.V2.ContextInsensitive
open System
open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

let exampleService =
    { new SimpleAPIoverview.Service() with
        member this.ListVersionsv2 = fun next ctx -> task {
            let! input = this.ListVersionsv2Input ctx
            return! this.ListVersionsv2Output input next ctx
        }
        member _.ListVersionsv2Input ctx = task {
            return
                if DateTime.Now.Ticks / 10L % 2L = 0L then
                    Choice1Of2 { SimpleAPIoverview.dataSetList.apis = [||]; total = 123 }
                else
                    Choice2Of2 true
            }
        member _.ListVersionsv2Output input =
            match input with
            | Choice1Of2 dataList -> json dataList
            | Choice2Of2 bool -> setStatusCode 300 >=> json bool
        
        
        member this.GetVersionDetailsv2 = fun next ctx -> task {
            let! input = this.GetVersionDetailsv2Input ctx
            return! this.GetVersionDetailsv2Output input next ctx
        }
        member _.GetVersionDetailsv2Input ctx = task {
            return
                if DateTime.Now.Ticks / 10L % 2L = 0L then
                    Choice1Of2 {| subscriptionId = "hello" |}
                else
                    Choice2Of2 false
            }
        member _.GetVersionDetailsv2Output input =
            match input with
            | Choice1Of2 sub -> json sub
            | Choice2Of2 bool -> setStatusCode 203 >=> json bool 
    
        member this.ListSearchableFields args = fun next ctx -> task {
            let! input = this.ListSearchableFieldsInput(args, ctx)
            return! this.ListSearchableFieldsOutput input next ctx
        }
        member _.ListSearchableFieldsInput ((args,ctx)) = task {
            return
                if args.version = "v1" then
                    Choice1Of2 "ok"
                else
                    Choice2Of2 "not_ok"
            }
        member _.ListSearchableFieldsOutput input =
            match input with
            | Choice1Of2 ok -> json ok
            | Choice2Of2 notok -> setStatusCode 404 >=> json notok
        
        member this.PerformSearch args = fun next ctx -> task {
            let! input = this.PerformSearchInput(args, ctx)
            return! this.PerformSearchOutput input next ctx
        }
        member _.PerformSearchInput ((args,ctx)) = task {
            return
                if args.version = "v1" then
                    Choice1Of2 [| box "abc" |]
                else
                    Choice2Of2 ()
            }
        member _.PerformSearchOutput input =
            match input with
            | Choice1Of2 array -> json array
            | Choice2Of2 () -> setStatusCode 404 >=> setStatusCode 404 }
        

let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe SimpleAPIoverview.webApp

let configureServices (services : IServiceCollection) =
    services
        .AddGiraffe()
        .AddSingleton<SimpleAPIoverview.Service>(exampleService)
    |> ignore

let configureLogging (loggerBuilder : ILoggingBuilder) =
    loggerBuilder.AddConsole()
                 .AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .Configure(configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    .UseUrls("http://*:5005")
                    |> ignore)
        .Build()
        .Run()
    0
