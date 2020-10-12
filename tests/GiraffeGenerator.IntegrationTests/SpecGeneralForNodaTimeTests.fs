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
open NodaTime
open NodaTime.Serialization.JsonNet
open Xunit

type SpecGeneralForNodaTimeTests() =
    
    let specGeneralForNodaTimeService =
        {
            new Specforgeneralnodatimetesting.Service() with
                member _.PostIdInput ((input, body, ctx)) =
                    task {
                        return
                            {
                                dateParamFromQuery = input.dateParam
                                timeParamFromQuery = input.timeParam
                                offsetParamFromQuery = input.offsetParam
                                durationParamFromQuery = input.durationParam
                                instantParamFromQuery = input.instantParam
                                localDateTimeParamFromQuery = input.localDateTimeParam
                                offsetDateTimeParamFromQuery = input.offsetDateTimeParam
                                forDefaultsTesting = body.forDefaultsTesting
                                zonedDateTime = body.zonedDateTime
                            }
                    }
                
        }
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe Specforgeneralnodatimetesting.webApp

    let jsonSettings =
        let s = JsonSerializerSettings(Converters=System.Collections.Generic.List())
        s.Converters.Add optionConverter
        s.ConfigureForNodaTime(DateTimeZoneProviders.Tzdb)
    let configureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer(jsonSettings))
            .AddSingleton(specGeneralForNodaTimeService)
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
        let date = LocalDate(1995, 10, 9)
        let time = LocalTime(01, 30, 00)
        let offset = Offset.FromHours 1
        let duration = Duration.FromMinutes 15.
        let zone = DateTimeZoneProviders.Tzdb.[ "Europe/London" ]
        let dateTime = date + time
        let offsetDateTime = OffsetDateTime(dateTime, offset)
        let zonedDateTime = dateTime.InZoneLeniently zone
        let instant = zonedDateTime.ToInstant()
        let expected: Specforgeneralnodatimetesting.dataSetListOutput =
            {
                dateParamFromQuery = date |> Some
                timeParamFromQuery = time |> Some
                offsetParamFromQuery = offset |> Some
                durationParamFromQuery = duration |> Some
                instantParamFromQuery = instant |> Some
                localDateTimeParamFromQuery = dateTime |> Some
                offsetDateTimeParamFromQuery = offsetDateTime |> Some
                zonedDateTime = zonedDateTime
                forDefaultsTesting =
                    {
                        dateParamFromBody = date
                        timeParamFromBody = time
                        offsetParamFromBody = offset
                        timeZoneParamFromBody = zone
                        periodParamFromBody =
                            let v = PeriodBuilder()
                            v.Days <- 8
                            v.Hours <- 115L
                            v.Minutes <- 78L
                            v.Build()
                        durationParamFromBody = duration
                        instantParamFromBody = instant
                        localDateTimeParamFromBody = dateTime
                        offsetDateTimeParamFromBody = offsetDateTime 
                    }
            }
        // usage of codegen requires custom converter for options and noda time types
        // as we don't know which json framework user is going to use.
        // But we want tests to be nice and to respect the spec, so here is OptionConverter.fs (by @Szer) and NodaTime.Serialization.JsonNet
        // Also, json by hand for the success story this test is checking to ensure that spec is respected
        let jsonInputString = """{
    "forDefaultsTesting": {
        "dateParamFromBody": "1995-10-09",
        "timeParamFromBody": "01:30:00",
        "offsetParamFromBody": "+01:00",
        "timeZoneParamFromBody": "Europe/London",
        "periodParamFromBody": "P8DT115H78M",
        "durationParamFromBody": "00:15:00",
        "instantParamFromBody": "1995-10-09T00:30:00Z",
        "localDateTimeParamFromBody": "1995-10-09T01:30:00",
        "offsetDateTimeParamFromBody": "1995-10-09T01:30:00+01:00"
    },
    "zonedDateTime": "1995-10-09T01:30:00.000+01 Europe/London"
}
"""
        use jsonContent = new StringContent(jsonInputString, Encoding.UTF8, "text/json")
        let query = // note the difference in duration formats
            [
                "dateParam", "1995-10-09"
                "timeParam", "01:30:00"
                "offsetParam", "+01:00"
                "durationParam", "00:00:15:00"
                "instantParam", "1995-10-09T00:30:00Z"
                "localDateTimeParam", "1995-10-09T01:30:00"
                "offsetDateTimeParam", "1995-10-09T01:30:00+01:00"
            ]
            |> Seq.map (fun (n,v) -> n, Uri.EscapeDataString v)
            |> Seq.map (fun (n,v) -> n + "=" + v)
            |> String.concat "&"
        let url = sprintf "/id?%s" query
        let! response = client.PostAsync(url, jsonContent)
        let! text = response.Content.ReadAsStringAsync()
        let deserialized = JsonConvert.DeserializeObject<_>(text, jsonSettings)
        Assert.Equal(expected, deserialized)
        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }
    
    interface IDisposable with
        member _.Dispose() = server.Dispose()

