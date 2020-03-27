# Giraffe Generator

This is first naive version of Giraffe server generator from OpenAPI specification
I believe in "contract-first" approach, and your OpenAPI spec is basically contract for your API
Backend code is just an implementation of this contract and client doesn't really want to know about it
Neither should you, so this library should help you with that

It follows (Myriad)[https://github.com/MoiraeSoftware/myriad] approach and defines MSBuild target to generate code based on input
It's still in VERY early stage of development, only VERY basic features are supported

## How to use

1. Add nugets:
    - `GiraffeGenerator.Core`
    - `GiraffeGenerator.Sdk`
1. Create OpenAPI spec file
1. Add generated file to your project file:
```
    <Compile Include="Generated.fs">
      <OpenApiSpecFile>spec.yaml</OpenApiSpecFile>
    </Compile>
```
1. Implement interface defined in this file and register your implementation in AspNetCore DI

## How it works

1. Parse OpenAPI spec with package `Microsoft.OpenApi.Readers` in intermediate representation (IR)
1. Create F# AST based on that IR
1. Create source code file from that F# AST with `Fantomas`

## How to build

1. Give executables right to `./build.sh`
1. Run it!

At this stage there is no NuGet package publishing and packages are being published locally
To consume them in `Example` project there is local `NuGet.Config` with local repo added

After first full `build&pack` you could delete `Generated.fs` file from `Example` project and build it again to see that it actually generates it on build