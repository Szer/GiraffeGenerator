export V=$(pcregrep -o1 '(\d+\.\d+\.\d+)' Directory.build.props)
dotnet tool restore
dotnet pack src/GiraffeGenerator.Sdk -c Release -o bin/nupkg /p:Version=$V
dotnet pack src/GiraffeGenerator -c Release -o bin/nupkg /p:Version=$V
dotnet mergenupkg --source bin/nupkg/GiraffeGenerator.Sdk.$V.nupkg --other bin/nupkg/GiraffeGenerator.$V.nupkg --tools --only-files
dotnet pack src/GiraffeGenerator.Core -c Release -o bin/nupkg /p:Version=$V
dotnet build src/Example -c Release
dotnet test
