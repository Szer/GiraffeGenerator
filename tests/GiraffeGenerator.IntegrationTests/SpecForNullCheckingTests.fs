namespace GiraffeGenerator.IntegrationTests

open System.Net
open System.Net.Http
open System.Text
open FSharp.Control.Tasks.V2.ContextInsensitive
open System
open Giraffe
open Giraffe.Serialization.Json
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Newtonsoft.Json
open Xunit

type SpecForNullCheckingTests() =
    
    let specForNullCheckingService=
        {
            new Specfornullchecking.Service() with
                member _.PostIdInput ((body, ctx)) =
                    task {
                        return
                            {
                                nullCheckingTest = body.nullCheckingTest
                            }
                    }
                
        }
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe Specfornullchecking.webApp

    let jsonSettings =
        JsonSerializerSettings(Converters=[|optionConverter|])
    let configureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer(jsonSettings))
            .AddSingleton(specForNullCheckingService)
        |> ignore

    let configureLogging (loggerBuilder : ILoggingBuilder) =
        loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
                     .AddConsole()
                     .AddDebug() |> ignore

    let webHostBuilder =
        WebHost.CreateDefaultBuilder()
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .ConfigureLogging(configureLogging)
            
    let server = new TestServer(webHostBuilder)
    let client = server.CreateClient()

    [<Fact>]
    let ``Nullchecking fails at catching a nullref but error is handled by DU anyways. This is not by design but seems to be good enough for this PR - nullchecking needs improvement anyways ``() = task {
        let jsonInputString = "null"
        use jsonContent = new StringContent(jsonInputString, Encoding.UTF8, "text/json")
        try
            let! response = client.PostAsync("/id", jsonContent)
            let! _ = response.Content.ReadAsStringAsync()
            Assert.Equal(false,true)
        with | e -> Assert.Equal(e.Message, "Body binding error:\nObject reference not set to an instance of an object.")
    }
    
    interface IDisposable with
        member _.Dispose() = server.Dispose()

