namespace GiraffeGenerator.IntegrationTests

open System.ComponentModel.DataAnnotations
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

type AlwaysFailValidationReplacer() =
    interface SpecWithValidationExtensibility.IGiraffeValidator<SpecWithValidationExtensibility.GetTestValidationQuery> with
        member _.Validate((_,_)) = [|
            ValidationResult("Never valid")
        |]

type AlwaysFailValidationAugmenter() =
    interface SpecWithValidationExtensibility.IGiraffeAdditionalValidator<SpecWithValidationExtensibility.GetTestValidationQuery> with
        member _.Validate((_,_)) = [|
            ValidationResult("Never valid")
        |]

type NeverFailValidationReplacer() =
    interface SpecWithValidationExtensibility.IGiraffeValidator<SpecWithValidationExtensibility.GetTestValidationQuery> with
        member _.Validate((_,_)) = [||]

type NeverFailValidationAugmenter() =
    interface SpecWithValidationExtensibility.IGiraffeAdditionalValidator<SpecWithValidationExtensibility.GetTestValidationQuery> with
        member _.Validate((_,_)) = [||]

type SpecWithValidationExtensibilityTests() =
    let specWithValidationService=
        {
            new SpecWithValidationExtensibility.Service() with
                member _.TestValidationInput ((_, _)) =
                    task {
                        return Choice1Of2 "ok"
                    }
                member _.TestValidationInputError ((err, _)) =
                    task {
                        return Choice2Of2 (SpecWithValidationExtensibility.argLocationErrorToString 0 err)
                    }
                
        }
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe SpecWithValidationExtensibility.webApp
        
    let jsonSettings =
        JsonSerializerSettings(Converters=[|optionConverter|])

    let configureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer(jsonSettings))
            .AddSingleton(specWithValidationService)
        |> ignore

    let configureLogging (loggerBuilder : ILoggingBuilder) =
        loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
                     .AddConsole()
                     .AddDebug() |> ignore

    let webHostBuilder addServices =
        WebHost.CreateDefaultBuilder()
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(addServices >> configureServices)
            .ConfigureLogging(configureLogging)
            
    let server addServices = new TestServer(webHostBuilder addServices)
    let client addServices = (server addServices).CreateClient()
    
    [<Fact>]
    let ``[no extensions (baseline)] /test-validation?maxLengthRestrictedTo8String=12345678 -> 200 "ok"`` ()  = task {
        let client = client id
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=12345678")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal("\"ok\"", responseText)
        do Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }
    
    [<Fact>]
    let ``[no extensions (baseline)] /test-validation?maxLengthRestrictedTo8String=123456789 -> 400 "must be a string or array type with a maximum length of '8'"`` () = task {
        let client = client id
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=123456789")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Contains("must be a string or array type with a maximum length of '8'", responseText)
    }
    
    [<Fact>]
    let ``[replace validation: always fail] /test-validation?maxLengthRestrictedTo8String=12345678 -> 400 "Never valid"`` ()  = task {
        let client = client (fun s -> s.AddSingleton<SpecWithValidationExtensibility.IGiraffeValidator<_>, AlwaysFailValidationReplacer>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=12345678")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Contains("Never valid", responseText)
    }
    
    [<Fact>]
    let ``[replace validation: always fail] /test-validation?maxLengthRestrictedTo8String=123456789 -> 400 "Never valid"`` () = task {
        let client = client (fun s -> s.AddSingleton<SpecWithValidationExtensibility.IGiraffeValidator<_>, AlwaysFailValidationReplacer>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=123456789")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Contains("Never valid", responseText)
    }
    
    [<Fact>]
    let ``[augment validation: always fail] /test-validation?maxLengthRestrictedTo8String=12345678 -> 400 "Never valid"`` ()  = task {
        let client = client (fun s -> s.AddSingleton<SpecWithValidationExtensibility.IGiraffeAdditionalValidator<_>, AlwaysFailValidationAugmenter>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=12345678")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Contains("Never valid", responseText)
    }
    
    [<Fact>]
    let ``[augment validation: always fail] /test-validation?maxLengthRestrictedTo8String=123456789 -> 400 "Never valid, must be a string or array type with a maximum length of '8'"`` () = task {
        let client = client (fun s -> s.AddSingleton<SpecWithValidationExtensibility.IGiraffeAdditionalValidator<_>, AlwaysFailValidationAugmenter>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=123456789")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Contains("Never valid", responseText)
        do Assert.Contains("must be a string or array type with a maximum length of '8'", responseText)
    }
    
    [<Fact>]
    let ``[replace&augment validation: always fail] /test-validation?maxLengthRestrictedTo8String=12345678 -> 400 "Never valid, Never valid"`` ()  = task {
        let client = client (fun s ->
            s.AddSingleton<SpecWithValidationExtensibility.IGiraffeValidator<_>, AlwaysFailValidationReplacer>()
             .AddSingleton<SpecWithValidationExtensibility.IGiraffeAdditionalValidator<_>, AlwaysFailValidationAugmenter>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=12345678")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Contains("Never valid", responseText)
        let idx1 = responseText.IndexOf "Never valid"
        let idx2 = responseText.LastIndexOf "Never valid"
        Assert.NotEqual(idx1, idx2)
    }
    
    [<Fact>]
    let ``[replace&augment validation: always fail] /test-validation?maxLengthRestrictedTo8String=123456789 -> 400 "Never valid, Never valid'"`` () = task {
        let client = client (fun s ->
            s.AddSingleton<SpecWithValidationExtensibility.IGiraffeValidator<_>, AlwaysFailValidationReplacer>()
             .AddSingleton<SpecWithValidationExtensibility.IGiraffeAdditionalValidator<_>, AlwaysFailValidationAugmenter>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=123456789")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Contains("Never valid", responseText)
        let idx1 = responseText.IndexOf "Never valid"
        let idx2 = responseText.LastIndexOf "Never valid"
        Assert.NotEqual(idx1, idx2)
    }
    
    [<Fact>]
    let ``[replace validation: never fail] /test-validation?maxLengthRestrictedTo8String=12345678 -> 200 "ok"`` ()  = task {
        let client = client (fun s -> s.AddSingleton<SpecWithValidationExtensibility.IGiraffeValidator<_>, NeverFailValidationReplacer>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=12345678")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal("\"ok\"", responseText)
        do Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }
    
    [<Fact>]
    let ``[replace validation: never fail] /test-validation?maxLengthRestrictedTo8String=123456789 -> 200 "ok"`` () = task {
        let client = client (fun s -> s.AddSingleton<SpecWithValidationExtensibility.IGiraffeValidator<_>, NeverFailValidationReplacer>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=123456789")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal("\"ok\"", responseText)
        do Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }
    
    [<Fact>]
    let ``[augment validation: never fail] /test-validation?maxLengthRestrictedTo8String=12345678 -> 200 "ok"`` ()  = task {
        let client = client (fun s -> s.AddSingleton<SpecWithValidationExtensibility.IGiraffeAdditionalValidator<_>, NeverFailValidationAugmenter>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=12345678")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal("\"ok\"", responseText)
        do Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }
    
    [<Fact>]
    let ``[augment validation: never fail] /test-validation?maxLengthRestrictedTo8String=123456789 -> 400 "must be a string or array type with a maximum length of '8'"`` () = task {
        let client = client (fun s -> s.AddSingleton<SpecWithValidationExtensibility.IGiraffeAdditionalValidator<_>, NeverFailValidationAugmenter>())
        let! response = client.GetAsync("/test-validation?maxLengthRestrictedTo8String=123456789")
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Contains("must be a string or array type with a maximum length of '8'", responseText)
    }