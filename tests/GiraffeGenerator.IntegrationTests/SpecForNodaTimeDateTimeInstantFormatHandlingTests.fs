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

type SpecForNodaTimeDateTimeFormatHandlingTests() =
    
    let serviceInstant =
        {
            new SpecForNodaTimeDateTimeInstantFormatHandling.Service() with
                member _.PostIdInput ((body, ctx)) =
                    task {
                        return body
                    }
                
        }
    let serviceLocalDateTime =
        {
            new SpecForNodaTimeDateTimeLocalDateTimeFormatHandling.Service() with
                member _.PostIdInput ((body, ctx)) =
                    task {
                        return body
                    }
                
        }
    let serviceOffsetDateTime =
        {
            new SpecForNodaTimeDateTimeOffsetDateTimeFormatHandling.Service() with
                member _.PostIdInput ((body, ctx)) =
                    task {
                        return body
                    }
                
        }
    let serviceZonedDateTime =
        {
            new SpecForNodaTimeDateTimeZonedDateTimeFormatHandling.Service() with
                member _.PostIdInput ((body, ctx)) =
                    task {
                        return body
                    }
                
        }
    let serviceDefaultDateTime =
        {
            new SpecForNodaTimeDateTimeDefaultFormatHandling.Service() with
                member _.PostIdInput ((body, ctx)) =
                    task {
                        return body
                    }
                
        }
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe
        <|
            choose [
                subRoute "/instant" SpecForNodaTimeDateTimeInstantFormatHandling.webApp
                subRoute "/local-date-time" SpecForNodaTimeDateTimeLocalDateTimeFormatHandling.webApp
                subRoute "/offset-date-time" SpecForNodaTimeDateTimeOffsetDateTimeFormatHandling.webApp
                subRoute "/zoned-date-time" SpecForNodaTimeDateTimeZonedDateTimeFormatHandling.webApp
                subRoute "/default" SpecForNodaTimeDateTimeDefaultFormatHandling.webApp
            ]

    let jsonSettings =
        let s = JsonSerializerSettings(Converters=System.Collections.Generic.List())
        s.Converters.Add optionConverter
        s.ConfigureForNodaTime(DateTimeZoneProviders.Tzdb)
    let configureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer(jsonSettings))
            .AddSingleton(serviceInstant)
            .AddSingleton(serviceLocalDateTime)
            .AddSingleton(serviceOffsetDateTime)
            .AddSingleton(serviceZonedDateTime)
            .AddSingleton(serviceDefaultDateTime)
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
    
    let testBody prefix value = task {
        let expected = value
        let jsonInputString = JsonConvert.SerializeObject(value, jsonSettings)
        use jsonContent = new StringContent(jsonInputString, Encoding.UTF8, "text/json")
        let url = sprintf "/%s/id" prefix
        let! response = client.PostAsync(url, jsonContent)
        let! text = response.Content.ReadAsStringAsync()
        let deserialized = JsonConvert.DeserializeObject(text, value.GetType(), jsonSettings)
        Assert.Equal(expected, deserialized)
        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }
    
    let instant = Instant.FromUtc(2020, 10, 22, 14, 20)
    let zone = DateTimeZoneProviders.Tzdb.Item "Europe/Moscow"
    let zoned = instant.InZone zone

    [<Fact>]
    let ``API method accepts and returns date time in the instant format``() =
        let value: SpecForNodaTimeDateTimeInstantFormatHandling.bodyModel =
            { dateTime = instant }
        testBody "instant" value

    [<Fact>]
    let ``API method accepts and returns date time in the local date time format``() =
        let value: SpecForNodaTimeDateTimeLocalDateTimeFormatHandling.bodyModel =
            { dateTime = zoned.LocalDateTime }
        testBody "local-date-time" value

    [<Fact>]
    let ``API method accepts and returns date time in the offset date time format``() =
        let value: SpecForNodaTimeDateTimeOffsetDateTimeFormatHandling.bodyModel =
            { dateTime = zoned.ToOffsetDateTime() }
        testBody "offset-date-time" value

    [<Fact>]
    let ``API method accepts and returns date time in the zoned date time format``() =
        let value: SpecForNodaTimeDateTimeZonedDateTimeFormatHandling.bodyModel =
            { dateTime = zoned }
        testBody "zoned-date-time" value

    
    [<Fact>]
    let ``API method accepts and returns date time in the default format``() =
        let value: SpecForNodaTimeDateTimeOffsetDateTimeFormatHandling.bodyModel =
            { dateTime = zoned.ToOffsetDateTime() }
        testBody "default" value

    interface IDisposable with
        member _.Dispose() = server.Dispose()

