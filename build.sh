dotnet tool restore
dotnet pack src/GiraffeGenerator.Sdk -c Release -o bin/nupkg
dotnet pack src/GiraffeGenerator -c Release -o bin/nupkg
dotnet mergenupkg --source bin/nupkg/GiraffeGenerator.Sdk.1.0.0.nupkg --other bin/nupkg/GiraffeGenerator.1.0.0.nupkg --tools --only-files
dotnet pack src/GiraffeGenerator.Core -c Release -o bin/nupkg
dotnet build src/Example -c Release
dotnet test
