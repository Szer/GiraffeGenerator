$ErrorActionPreference = "Stop"

# protection from accident runs
$rndNumber = Get-Random -Maximum 999 -Minimum 100
Write-Host "Please enter number $rndNumber to continue: "
$inputNumber = Read-Host
if ($inputNumber -ne $rndNumber) { exit }

Write-Host "Have you bumped version and added all changes to changelog?"
$inputCh = Read-Host
if ($inputCh -ne "y") { exit }

#read new version from dir.build.props
$matches = Select-String -path Directory.build.props '<Version>(\d+)\.(\d+)\.(\d+)</Version>'
$major = $matches.Matches[0].Groups[1].Value
$minor = $matches.Matches[0].Groups[2].Value
$patch = $matches.Matches[0].Groups[3].Value
$version = "$major.$minor.$patch"

#save dir.build.props and changelog for later
git stash clear
git stash push Directory.build.props
git stash push CHANGELOG.md

#fetch tags and hard reset current master to origin master
git fetch --all --tags
git checkout master
git reset --hard origin/master
git clean -xfd

#unstash dir.build.props and changelog
git stash pop
git stash pop

#run build script early to make sure everything works
./build.ps1
if( $LASTEXITCODE -ne 0 ) {
    throw "errors on build"
}

#check that this tag hasn't been released yet
$tag_exists = git tag -l $version
if ($tag_exists -ne $null) {
    throw "Tag already exists"
}

#check that last version in changelog is lower
$old_matches = Select-String -path CHANGELOG.md '## \[(\d+)\.(\d+)\.(\d+)\]'
$old_major = 0
$old_minor = 0
$old_patch = 0

if ($old_matches -ne $null) {
    $old_major = $old_matches.Matches[0].Groups[1].Value
    $old_minor = $old_matches.Matches[0].Groups[2].Value
    $old_patch = $old_matches.Matches[0].Groups[3].Value
}

if ($major -lt $old_major) {
    throw "major version can't be lower than current one"
} elseif ($major -eq $old_major) {
    if($minor -lt $old_minor) {
        throw "minor version can't be lower than current one"
    } elseif ($minor -eq $old_minor) {
        if($patch -le $old_patch) {
            throw "patch version can't be lower or equal than current one"
        }
    }
}

# we don't want to change changelog on patch updates
if ($major -ne $old_major -or $minor -ne $old_minor) {

    #put version and current date in changelog
    $date = Get-Date -UFormat "%Y-%m-%d"
    (Get-Content CHANGELOG.md) `
        -replace '## \[Unreleased\]$', "## [Unreleased]`n`n## [$version] - $date" |
      Out-File CHANGELOG.md
    
    #put link to changes at bottom
    $repo_link = "https://github.com/Szer/GiraffeGenerator/compare"
    $compareString = Select-String -path CHANGELOG.md "\[Unreleased\]: $repo_link/(.*)\.\.\.(.*)$"
    if($compareString -eq $null) {throw "can't find unreleased link at the bottom of CHANGELOG"}
    
    $from = $compareString.Matches[0].Groups[1].Value
    $to = $compareString.Matches[0].Groups[2].Value
    
    $newLinks = "[Unreleased]: https://github.com/Szer/GiraffeGenerator/compare/v$version...master`n[$version]: https://github.com/Szer/GiraffeGenerator/compare/$from...v$version"
    (Get-Content CHANGELOG.md) `
        -replace "\[Unreleased\]: $repo_link/.*\.\.\..*$", $newLinks |
      Out-File CHANGELOG.md
}

#commit dir.build.props with changelog with message "release {version}"
git commit -m "release v$version" -a
git tag -a "v$version" -m "release v$version"

#push to master with tags
git push --follow-tags

#push to master#publish two packages to nuget
$apikey = Get-ChildItem Env:GIRAFFE_GENERATOR_NUGET
dotnet nuget push "bin/nupkg/GiraffeGenerator.Core.$version.nupkg" --api-key $apikey.Value -s https://api.nuget.org/v3/index.json
dotnet nuget push "bin/nupkg/GiraffeGenerator.Sdk.$version.nupkg" --api-key $apikey.Value -s https://api.nuget.org/v3/index.json
