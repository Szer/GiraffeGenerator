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
        member _.ListVersionsv2Input ctx = task {
            return
                if DateTime.Now.Ticks / 10L % 2L = 0L then
                    Choice1Of2 { SimpleAPIoverview.dataSetList.apis = [||]; total = 123 }
                else
                    Choice2Of2 true
            }
        
        member _.GetVersionDetailsv2Input ctx = task {
            return
                if DateTime.Now.Ticks / 10L % 2L = 0L then
                    Choice1Of2 {| subscriptionId = "hello" |}
                else
                    Choice2Of2 false
            }
        }

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
