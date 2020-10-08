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

type SpecWithParametersAndRequestBodyTests() =
    
    let specWithParametersAndRequestBodyService=
        {
            new SpecwithparametersandrequestbodyAPI.Service() with
                member _.PostIdInput ((input, body, ctx)) =
                    task {
                        return
                            {
                                pathParam = input.paramFromPath
                                queryParam = input.paramFromQuery
                                total = body.total
                                defaultsTest = body.defaultsTest
                            }
                    }
                
        }
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe SpecwithparametersandrequestbodyAPI.webApp

    let jsonSettings =
        JsonSerializerSettings(Converters=[|optionConverter|])
    let configureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer(jsonSettings))
            .AddSingleton(specWithParametersAndRequestBodyService)
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
    let ``Fully specified input parameters returned as output as is``() = task {
        let expected: SpecwithparametersandrequestbodyAPI.dataSetListOutput =
            {
                pathParam = "valueOfThePathParam"
                queryParam = Some 1123
                total = Some 165443
                defaultsTest =
                    {
                        optionalArrayWithDefaultItems = Some [| 1;2;3;4 |]
                        requiredArrayWithDefaultItems = [| 5;6;7;8 |]
                        apiKey = "passw0rd"
                        apiVersionNumber = "48"
                        apiUrl = Uri("https://microsoft.com")
                        apiCount = 1L
                        apiAvg = 9
                        isInternal = false
                        start = DateTime(2019, 1, 21)
                        someDateTime = DateTimeOffset(DateTime(2019,1,21,23,59,16,343), TimeSpan(3,0,0))
                        pi = 3.141592654
                        someUid = Guid.Parse("6662cbfd-f323-4b7d-bcc0-28f127c2b365")
                    }
            }
        // usage of codegen requires custom converter for options
        // as we don't know which json framework user is going to use.
        // But we want tests to be nice and to respect the spec, so here is OptionConverter.fs (by @Szer)
        let jsonInputString = """{
    "total": 165443,
    "defaultsTest": {
        "optionalArrayWithDefaultItems": [1,2,3,4],
        "requiredArrayWithDefaultItems": [5,6,7,8],
        "apiKey": "passw0rd",
        "apiVersionNumber": "48",
        "apiUrl": "https://microsoft.com",
        "apiCount": 1,
        "apiAvg": 9,
        "isInternal": false,
        "start": "2019-1-21",
        "someDateTime": "2019-1-21T23:59:16.343+03:00",
        "pi": 3.141592654,
        "someUid": "6662cbfd-f323-4b7d-bcc0-28f127c2b365"
    }
}
"""
        use jsonContent = new StringContent(jsonInputString, Encoding.UTF8, "text/json")
        let! response = client.PostAsync("/id/valueOfThePathParam?param=1123", jsonContent)
        let! text = response.Content.ReadAsStringAsync()
        let deserialized = JsonConvert.DeserializeObject<_>(text, jsonSettings)
        Assert.Equal(expected, deserialized)
        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }

    interface IDisposable with
        member _.Dispose() = server.Dispose()

