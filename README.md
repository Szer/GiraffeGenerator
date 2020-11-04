# Giraffe Generator
[![.NET Core CI](https://github.com/Szer/GiraffeGenerator/workflows/.NET%20Core/badge.svg?branch=master)](https://github.com/Szer/GiraffeGenerator/actions?query=workflow%3A%22.NET+Core%22)


This is first naive version of Giraffe server generator from OpenAPI specification

I believe in "contract-first" approach, and your OpenAPI spec is basically contract for your API

Backend code is just an implementation of this contract and client doesn't really want to know about it

Neither should you, so this library will help you with that

It follows [Myriad](https://github.com/MoiraeSoftware/myriad) approach and defines MSBuild target to generate code based on input

It's still in early stage of development, so mostly basic features are being supported.

## Example project is available to check [here](https://github.com/Szer/GiraffeGenerator/tree/master/src/Example)

## Future feature list (basically TODO list):

- [ ] Creating models from OpenAPI schemas
   - [x] records generated from schema definitions with all data types from spec
   - [x] handlers in generated webApp should support these types
   - [x] default values support for primitive types
   - [ ] `oneOf` support
   - [ ] `anyOf` support
   - [ ] `allOf` support
   - [ ] `discriminator` support
   - [x] `not` *won't be supported*
   - [x] validation support (#33)
   - [x] NodaTime support opt-in (#32)
- [x] Multiple responses from one endpoint
- [ ] Creating endpoints with support for bindings
   - [x] path
   - [x] query
   - [x] body
   - [ ] header (#35)
   - [ ] cookie (#35)
   - [ ] content-type negotiated body (#36)
   - [x] binding error handling
- [x] Add XML comments on top of endpoint from OpenAPI descriptions
- [ ] Support authentication (best efforts)
- [ ] Support JSON/XML (de-)serialization
   - [x] JSON
   - [ ] XML

## How to use

- Add nuget `GiraffeGenerator.Sdk`
- Create OpenAPI spec file
- Add generated file to your project file:
```
    <Compile Include="Generated.fs">
      <OpenApiSpecFile>spec.yaml</OpenApiSpecFile>
    </Compile>
```
- Build project to generate the file
- Implement interface defined in this file and register your implementation in AspNetCore DI
- If you want to customize validation for some or all of your models, you have two extension points:
  - `IGiraffeValidator<'model>` - replaces all generated validation for the `'model` type.
    Note: doesn't replace the validation for nested complex types (objects, arrays, options).
    Provide an implementation for them explicitely if you want to replace validation for them too.
  - `IGiraffeAdditionalValidator<'model>` - adds more validation
    to either `IGiraffeValidator<'model>` or generated validation
- Note for people migrating from/to *nix: `System.ComponentModel.DataAnnotations.RangeAttribute` used
  by validation produces a different text representation for Double.*Infinity on *nix:
  "Infinity" instead of infinity unicode symbol (&#221E;)
- May require serializer configuration to support mapping of absent and null values from/to Optionon<_>
- May require serializer configuration to throw on absent required properties

## Codegen configuration

All configuration is done by adding more child tags to Compile object.
There are two types of configuration: parameterless (flags) and parameterfull (parameters).

`flag` is specified like `<OpenApiFlag>true</OpenApiFlag>`: absense of tag or any content except for `true` is treated as `false`

`parameter` is passed as tag content like `<OpenApiValue>your value</OpenApiValue>`

Example of both:
```
    <Compile Include="Generated.fs">
      <OpenApiSpecFile>spec.yaml</OpenApiSpecFile>
      <OpenApiSomeFlag>true</OpenApiSomeFlag>
    </Compile>
```

### Generated module name customization
Defined as parameter `OpenApiModuleName`

### Allowing non-qualified access
Defined as flag `OpenApiAllowUnqualifiedAccess`

### NodaTime support
Enabled by `OpenApiUseNodaTime` flag.

Has optional parameter `OpenApiMapDateTimeInto` which controls generated type for `date-time` OpenAPI string format.
Has four possible values:
- `instant`
- `local-date-time`
- `offset-date-time` (default)
- `zoned-date-time`

Adds support for the following custom string formats:
| Format           | NodaTime type                          | Supports default values (format) |
| ---------------- | -------------------------------------- | -------------------------------- |
| local-date       | LocalDate                              | [x] `uuuu'-'MM'-'dd (c)`
| date             | LocalDate                              | [x] (as above)
| date-time        | Differs (see `OpenApiMapDateTimeInto`) | [~] By configured format
| instant          | Instant                                | [x] `uuuu'-'MM'-'dd'T'HH':'mm':'ss.FFFFFFFFF'Z'` 
| local-time       | LocalTime                              | [x] `HH':'mm':'ss.FFFFFFFFF`
| time             | LocalTime                              | [x] (as above)
| local-date-time  | LocalDateTime                          | [x] `uuuu'-'MM'-'dd'T'HH':'mm':'ss.FFFFFFFFF (c)`
| offset-date-time | OffsetDateTime                         | [x] `uuuu'-'MM'-'dd'T'HH':'mm':'ss;FFFFFFFFFo<Z+HH:mm> (c)`
| zoned-date-time  | ZonedDateTime                          | [ ] No
| offset           | Offset                                 | [x] general pattern, e.g. +05 or -03:30
| time-offset      | Offset                                 | [x] (as above)
| duration         | Duration                               | [x] -H:mm:ss.FFFFFFFFF
| period           | Interval                               | [x] ISO8601 Duration (round-trip)
| time-zone        | DateTimeZone                           | [x] IANA Tzdb identifier
| date-time-zone   | DateTimeZone                           | [x] (as above)

Usage requires installation of NodaTime 3+ nuget at least.
For some content types of bodies containing NodaTime types [additional packages may be needed](https://nodatime.org/3.0.x/userguide/serialization).

Note that zoned-date-time cannot be passed in query string or path parameters by default.
Also note that (de)serialization duration format differs for query string binding and
Json.NET serialization by default. See [this test](https://github.com/bessgeor/GiraffeGenerator/blob/feature/noda-time-support__%2332/tests/GiraffeGenerator.IntegrationTests/SpecGeneralForNodaTimeTests.fs#L115) for more details.

## How it works internally

- It parses OpenAPI spec with package `Microsoft.OpenApi.Readers` to intermediate representation (IR)
- Then it creates F# AST based on that IR
- Finally it produces source code file with help of `Fantomas`

## How to build and test

1. Restore tools: `dotnet tool restore`
1. `dotnet pwsh build.ps1`

At this stage there is no NuGet package publishing and packages are being published locally

To consume them in `Example` project there is local `NuGet.Config` with local repo added

After first full `build&pack` you could delete `Generated.fs` file from `Example` project and build it again to see that it actually generates on build
