$matches = Select-String -path Directory.build.props '<Version>(\d+\.\d+\.\d+)</Version>'
$version = $matches.Matches[0].Groups[1].Value

dotnet pack src/GiraffeGenerator.Sdk -c Release -o bin/nupkg /p:Version=$version
dotnet pack src/GiraffeGenerator -c Release -o bin/nupkg /p:Version=$version
dotnet mergenupkg --source bin/nupkg/GiraffeGenerator.Sdk.$V.nupkg --other bin/nupkg/GiraffeGenerator.$V.nupkg --tools --only-files
dotnet pack src/GiraffeGenerator.Core -c Release -o bin/nupkg /p:Version=$version
dotnet build src/Example -c Release
dotnet test
