namespace GiraffeGenerator.IntegrationTests

open System.Net
open System.Threading.Tasks
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

type SpecWithTextXmlTests() =
    
    let service =
        { new SpecwithxmlschemasAPI.Service() with
            member _.ApplicationJsonInput _ = Task.FromResult "1"
            member _.ApplicationXmlInput _ = Task.FromResult "2"
            member _.TextJsonInput _ = Task.FromResult "3"
            member _.TextXmlInput _ = Task.FromResult "4" }
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe SpecwithxmlschemasAPI.webApp

    let configureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton(service)
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
    let ``GET /applicationJson -> OK "1"``() = task {
        let! response = client.GetAsync("/applicationJson")
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("\"1\"",text)
        Assert.Equal(HttpStatusCode.OK ,response.StatusCode)
    }
    
    [<Fact>]
    let ``GET /applicationXml -> OK "2"``() = task {
        let! response = client.GetAsync("/applicationXml")
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("""<?xml version="1.0" encoding="utf-8"?>
<string>2</string>""",text)
        Assert.Equal(HttpStatusCode.OK ,response.StatusCode)
    }
    
    [<Fact>]
    let ``GET /textJson -> OK "3"``() = task {
        let! response = client.GetAsync("/textJson")
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("\"3\"",text)
        Assert.Equal(HttpStatusCode.OK ,response.StatusCode)
    }
    
    [<Fact>]
    let ``GET /textXml -> OK "4"``() = task {
        let! response = client.GetAsync("/textXml")
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("""<?xml version="1.0" encoding="utf-8"?>
<string>4</string>""",text)
        Assert.Equal(HttpStatusCode.OK ,response.StatusCode)
    }
    
    interface IDisposable with
        member _.Dispose() = server.Dispose()
