$matches = Select-String -path Directory.build.props '<Version>(\d+\.\d+\.\d+)</Version>'
$version = $matches.Matches[0].Groups[1].Value

Get-ChildItem .\ -include bin,obj -Recurse | foreach ($_) { remove-item $_.fullname -Force -Recurse }

dotnet tool restore
dotnet restore --locked-mode -s https://api.nuget.org/v3/index.json
dotnet pack src/GiraffeGenerator.Sdk -c Release -o bin/nupkg /p:Version=$version
dotnet pack src/GiraffeGenerator -c Release -o bin/nupkg /p:Version=$version --no-restore
dotnet mergenupkg --source bin/nupkg/GiraffeGenerator.Sdk.$version.nupkg --other bin/nupkg/GiraffeGenerator.$version.nupkg --tools --only-files
dotnet pack src/GiraffeGenerator.Core -c Release -o bin/nupkg /p:Version=$version --no-restore
dotnet build src/Example -c Release --no-restore
dotnet test --no-restore
