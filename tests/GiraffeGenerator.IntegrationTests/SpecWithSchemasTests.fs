namespace GiraffeGenerator.IntegrationTests

open System.Net
open FSharp.Control.Tasks.V2.ContextInsensitive
open System
open Giraffe
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Xunit

type SpecWithSchemasTests() =
    
    let specWithSchemasService=
        { new SpecwithschemasAPI.Service() with
            
            member _.ListVersionsv2 = text "123"
            
            member _.GetVersionDetailsv2Input ctx = task {
                return { SpecwithschemasAPI.dataSetList.apis = [||]; total = 123 }
            }
            member _.GetVersionDetailsv2Output dataSetList = json dataSetList
            
            member _.PostVersionDetailsv2 = text "345"}
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe SpecwithschemasAPI.webApp

    let configureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton(specWithSchemasService)
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
    let ``GET / -> OK "123"``() = task {
        let! response = client.GetAsync("/")
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("123",text)
        Assert.Equal(HttpStatusCode.OK ,response.StatusCode)
    }
    
    [<Fact>]
    let ``GET /v2 -> OK "{total:123,apis:[]}"``() = task {
        let! response = client.GetAsync("/v2")
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("{\"total\":123,\"apis\":[]}",text)
        Assert.Equal(HttpStatusCode.OK ,response.StatusCode)
    }
    
    [<Fact>]
    let ``POST /v2 -> OK "345"``() = task {
        let! response = client.PostAsync("/v2", null)
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("345",text)
        Assert.Equal(HttpStatusCode.OK ,response.StatusCode)
    }

    interface IDisposable with
        member _.Dispose() = server.Dispose()

