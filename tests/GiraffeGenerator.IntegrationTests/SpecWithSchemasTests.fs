namespace GiraffeGenerator.IntegrationTests

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
            
            member _.listVersionsv2 = text "123"
            member this.getVersionDetailsv2 = fun next ctx -> task {
                let! dataSetList = this.getVersionDetailsv2Input ctx
                return! this.getVersionDetailsv2Output dataSetList next ctx
            }
            
            member _.getVersionDetailsv2Input ctx = task {
                return { SpecwithschemasAPI.dataSetList.apis = [||]; total = 123 }
            }
            member _.getVersionDetailsv2Output dataSetList = json dataSetList
            
            member _.postVersionDetailsv2 = text "345"}
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe SpecwithschemasAPI.webApp

    let configureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton<SpecwithschemasAPI.Service>(specWithSchemasService)
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
    let ``/ >=> GET``() = task {
        let! response = client.GetAsync("/")
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("123",text)
    }
    
    [<Fact>]
    let ``/v2 >=> GET``() = task {
        let! response = client.GetAsync("/v2")
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("{\"total\":123,\"apis\":[]}",text)
    }
    
    [<Fact>]
    let ``/v2 >=> POST``() = task {
        let! response = client.PostAsync("/v2", null)
        let! text = response.Content.ReadAsStringAsync()
        Assert.Equal("345",text)
    }

    interface IDisposable with
        member _.Dispose() = server.Dispose()

