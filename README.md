# Giraffe Generator
[![.NET Core CI](https://github.com/Szer/GiraffeGenerator/workflows/.NET%20Core/badge.svg?branch=master)](https://github.com/Szer/GiraffeGenerator/actions?query=workflow%3A%22.NET+Core%22)


This is first naive version of Giraffe server generator from OpenAPI specification

I believe in "contract-first" approach, and your OpenAPI spec is basically contract for your API

Backend code is just an implementation of this contract and client doesn't really want to know about it

Neither should you, so this library will help you with that

It follows [Myriad](https://github.com/MoiraeSoftware/myriad) approach and defines MSBuild target to generate code based on input

It's still in VERY early stage of development, so only VERY basic features are being supported.

## Example project is available to check [here](https://github.com/Szer/GiraffeGenerator/tree/master/src/Example)

## Future feature list (basically TODO list):

- [ ] Creating models from OpenAPI schemas
   - [x] records generated from schema definitions with all data types from spec
   - [x] handlers in generated webApp should support these types
   - [ ] `oneOf` support
   - [ ] `anyOf` support
   - [ ] `allOf` support
   - [ ] `discriminator` support
   - [x] `not` *won't be supported*
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
