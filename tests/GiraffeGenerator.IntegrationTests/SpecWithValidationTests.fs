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
open Xunit

module ValidationCases =
    let private enumerateValidVariations valid validForkers =
        seq {
            yield "original valid case", valid
            for desc, forkValid in validForkers do
                yield desc, forkValid valid
        }
        |> Seq.cache

    let private validationResult location path message =
        let v: SpecWithValidation.validationErrors =
            {
                location = location
                propertyPath = path
                message = message
            }
        v
    let private response location path messages =
        let v: SpecWithValidation.validationErrorResponse =
            {
                otherErrors = None
                validationErrors =
                    messages
                    |> Array.map (validationResult location path)
            }
        v

    let validPath: SpecWithValidation.PostTestValidationPath =
        { versionEnum = 1; stringOfRestrictedToBeBetween3And8Length = "len" }
    let private validPathForkers: (string*(SpecWithValidation.PostTestValidationPath -> SpecWithValidation.PostTestValidationPath)) array =
        [|
            "versionEnum = 2 ",fun x -> { x with versionEnum = 2 }
            "versionEnum = 3 ",fun x -> { x with versionEnum = 3 }
            "stringOfRestrictedToBeBetween3And8Length = \"length 8\" ",fun x -> { x with stringOfRestrictedToBeBetween3And8Length = "length 8" }
        |]
    let private pathResponse = response "path"
    let private invalidPathMakers: (string*(SpecWithValidation.PostTestValidationPath -> struct (SpecWithValidation.validationErrorResponse*SpecWithValidation.PostTestValidationPath))) array =
        [|
            """versionEnum = 0 """, fun x ->
                pathResponse [|"versionEnum"|] [|"The field versionEnum must be a one of ['1', '2', '3']."|],
                { x with versionEnum = 0 }
            """versionEnum = 4 """, fun x ->
                pathResponse [|"versionEnum"|] [|"The field versionEnum must be a one of ['1', '2', '3']."|],
                { x with versionEnum = 4 }
            """versionEnum = -1 """, fun x ->
                pathResponse [|"versionEnum"|] [|"The field versionEnum must be a one of ['1', '2', '3']."|],
                { x with versionEnum = -1 }
            """stringOfRestrictedToBeBetween3And8Length = "!?" """, fun x ->
                pathResponse [| "stringOfRestrictedToBeBetween3And8Length" |] [| "The field stringOfRestrictedToBeBetween3And8Length must be a string or array type with a minimum length of '3'." |],
                { x with stringOfRestrictedToBeBetween3And8Length = "!?" }
            (* // path won't match in routing in this cases
            """stringOfRestrictedToBeBetween3And8Length = "" """, fun x ->
                pathResponse [| "stringOfRestrictedToBeBetween3And8Length" |] [| "The field stringOfRestrictedToBeBetween3And8Length must be a string or array type with a minimum length of '3'." |],
                { x with stringOfRestrictedToBeBetween3And8Length = "" }*)
            """stringOfRestrictedToBeBetween3And8Length = "My length is definitely bigger than 8" """, fun x ->
                pathResponse [| "stringOfRestrictedToBeBetween3And8Length" |] [| "The field stringOfRestrictedToBeBetween3And8Length must be a string or array type with a maximum length of '8'." |],
                { x with stringOfRestrictedToBeBetween3And8Length = "My length is definitely bigger than 8" }
        |]
    let private validPathVariations = enumerateValidVariations validPath validPathForkers

    let validQuery: SpecWithValidation.PostTestValidationQuery =
        { maxLengthRestrictedTo8String = "1234"
          minLengthRestrictedTo3String = "123456"
          stringOfDDMMMPattern = "15May"
          stringOfLengthBetween8And64ContainingAGiraffe = "1giraffe2giraffe3giraffe"
          integerDivisibleBy2 = 8
          integerBetween8And64 = 8
          integerBetween8And64Exclusive = 8
          integerBetween8ExclusiveAnd64 = 9
          integerBetween8ExclusiveAnd64Exclusive = 9
          integerGTE8 = 8
          integerGT8 = 9
          integerLTE64 = 64
          integerLT64 = 63
          integerEQ64 = 64
          longEnum = 0L
          longDivisibleBy2 = 8L
          longBetween8And64 = 8L
          longBetween8And64Exclusive = 8L
          longBetween8ExclusiveAnd64 = 9L
          longBetween8ExclusiveAnd64Exclusive = 9L
          longGTE8 = 8L
          longGT8 = 9L
          longLTE64 = 64L
          longLT64 = 63L
          longEQ64 = 64L
          floatEnum = 3.14
          floatBetween8And64 = 8.
          floatBetween8And64Exclusive = 8.
          floatBetween8ExclusiveAnd64 = 8.00000001
          floatBetween8ExclusiveAnd64Exclusive = 8.00000001
          floatGTE8 = 8.
          floatGT8 = 8.00000001
          floatLTE64 = 64.
          floatLT64 = 63.99999999
          floatEQ64 = 64. }
    let private validQueryForkers: (string*(SpecWithValidation.PostTestValidationQuery -> SpecWithValidation.PostTestValidationQuery)) array =
        [|
            //may not be valid in query string
            //"maxLengthRestrictedTo8String = \"\" ",fun x -> { x with maxLengthRestrictedTo8String = "" }
            "maxLengthRestrictedTo8String = \"12345678\" ",fun x -> { x with maxLengthRestrictedTo8String = "12345678" }
            "minLengthRestrictedTo3String = \"123\" ",fun x -> { x with minLengthRestrictedTo3String = "123" }
            "stringOfDDMMMPattern = \"01Jan\" ",fun x -> { x with stringOfDDMMMPattern = "01Jan" }
            "stringOfDDMMMPattern = \"30Feb\" ",fun x -> { x with stringOfDDMMMPattern = "30Feb" }
            "stringOfDDMMMPattern = \"29Mar\" ",fun x -> { x with stringOfDDMMMPattern = "29Mar" }
            "stringOfLengthBetween8And64ContainingAGiraffe = \"1giraffe!\" ",fun x -> { x with stringOfLengthBetween8And64ContainingAGiraffe = "1giraffe!" }
            "stringOfLengthBetween8And64ContainingAGiraffe = \"giraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffe!\" ",fun x -> { x with stringOfLengthBetween8And64ContainingAGiraffe = "giraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffe!" }
            "integerDivisibleBy2 = 0 ",fun x -> { x with integerDivisibleBy2 = 0 }
            "integerDivisibleBy2 = 2 ",fun x -> { x with integerDivisibleBy2 = 2 }
            "integerDivisibleBy2 = 2147483646 ",fun x -> { x with integerDivisibleBy2 = 2147483646 }
            "integerDivisibleBy2 = -2147483648 ",fun x -> { x with integerDivisibleBy2 = -2147483648 }
            "integerBetween8And64 = 64 ",fun x -> { x with integerBetween8And64 = 64 }
            "integerBetween8And64Exclusive = 32 ",fun x -> { x with integerBetween8And64Exclusive = 32 }
            "integerBetween8And64Exclusive = 62 ",fun x -> { x with integerBetween8And64Exclusive = 62 }
            "integerBetween8ExclusiveAnd64 = 64 ",fun x -> { x with integerBetween8ExclusiveAnd64 = 64 }
            "integerBetween8ExclusiveAnd64Exclusive = 63 ",fun x -> { x with integerBetween8ExclusiveAnd64Exclusive = 63 }
            "integerGTE8 = Int32.MaxValue ",fun x -> { x with integerGTE8 = Int32.MaxValue }
            "integerGT8 = Int32.MaxValue ",fun x -> { x with integerGT8 = Int32.MaxValue }
            "integerLTE64 = Int32.MinValue ",fun x -> { x with integerLTE64 = Int32.MinValue }
            "integerLT64 = Int32.MinValue ",fun x -> { x with integerLT64 = Int32.MinValue }
            "longEnum = Int64.MinValue ",fun x -> { x with longEnum = Int64.MinValue }
            "longEnum = Int64.MaxValue ",fun x -> { x with longEnum = Int64.MaxValue }
            "longDivisibleBy2 = 0L ",fun x -> { x with longDivisibleBy2 = 0L }
            "longDivisibleBy2 = 2L ",fun x -> { x with longDivisibleBy2 = 2L }
            "longDivisibleBy2 = -9223372036854775806L ",fun x -> { x with longDivisibleBy2 = -9223372036854775806L }
            "longDivisibleBy2 = -9223372036854775808L ",fun x -> { x with longDivisibleBy2 = -9223372036854775808L }
            "longBetween8And64 = 64L ",fun x -> { x with longBetween8And64 = 64L }
            "longBetween8And64Exclusive = 63L ",fun x -> { x with longBetween8And64Exclusive = 63L }
            "longBetween8ExclusiveAnd64 = 64L ",fun x -> { x with longBetween8ExclusiveAnd64 = 64L }
            "longBetween8ExclusiveAnd64Exclusive = 63L ",fun x -> { x with longBetween8ExclusiveAnd64Exclusive = 63L }
            "longGTE8 = Int64.MaxValue ",fun x -> { x with longGTE8 = Int64.MaxValue }
            "longGT8 = Int64.MaxValue ",fun x -> { x with longGT8 = Int64.MaxValue }
            "longLTE64 = Int64.MinValue ",fun x -> { x with longLTE64 = Int64.MinValue }
            "longLT64 = Int64.MinValue ",fun x -> { x with longLT64 = Int64.MinValue }
            "floatEnum = 3.141 ",fun x -> { x with floatEnum = 3.141 }
            "floatEnum = 3.1415 ",fun x -> { x with floatEnum = 3.1415 }
            "floatEnum = 3.14159 ",fun x -> { x with floatEnum = 3.14159 }
            "floatEnum = 3.141592 ",fun x -> { x with floatEnum = 3.141592 }
            "floatBetween8And64 = 8.00000001 ",fun x -> { x with floatBetween8And64 = 8.00000001 }
            "floatBetween8And64 = 63.999999999 ",fun x -> { x with floatBetween8And64 = 63.999999999 }
            "floatBetween8And64 = 64. ",fun x -> { x with floatBetween8And64 = 64. }
            "floatBetween8And64Exclusive = 8.00000001 ",fun x -> { x with floatBetween8And64Exclusive = 8.00000001 }
            "floatBetween8And64Exclusive = 63.99999999 ",fun x -> { x with floatBetween8And64Exclusive = 63.99999999 }
            "floatBetween8ExclusiveAnd64 = 9.00000001 ",fun x -> { x with floatBetween8ExclusiveAnd64 = 9.00000001 }
            "floatBetween8ExclusiveAnd64 = 63.99999999 ",fun x -> { x with floatBetween8ExclusiveAnd64 = 63.99999999 }
            "floatBetween8ExclusiveAnd64 = 64. ",fun x -> { x with floatBetween8ExclusiveAnd64 = 64. }
            "floatGTE8 = Double.MaxValue ",fun x -> { x with floatGTE8 = Double.MaxValue }
            "floatGTE8 = Double.PositiveInfinity ",fun x -> { x with floatGTE8 = Double.PositiveInfinity }
            "floatGT8 = Double.MaxValue ",fun x -> { x with floatGT8 = Double.MaxValue }
            "floatGT8 = Double.PositiveInfinity ",fun x -> { x with floatGT8 = Double.PositiveInfinity }
            "floatLTE64 = Double.MinValue ",fun x -> { x with floatLTE64 = Double.MinValue }
            "floatLTE64 = Double.NegativeInfinity ",fun x -> { x with floatLTE64 = Double.NegativeInfinity }
            "floatLT64 = Double.MinValue ",fun x -> { x with floatLT64 = Double.MinValue }
            "floatLT64 = Double.NegativeInfinity ",fun x -> { x with floatLT64 = Double.NegativeInfinity }
        |]
    let private queryResponse = response "query"
    let private invalidQueryMakers: (string*(SpecWithValidation.PostTestValidationQuery -> struct(SpecWithValidation.validationErrorResponse*SpecWithValidation.PostTestValidationQuery))) array =
        [|
            """maxLengthRestrictedTo8String = "I am too long" """, fun x ->
                queryResponse [| "maxLengthRestrictedTo8String" |] [| "The field maxLengthRestrictedTo8String must be a string or array type with a maximum length of '8'." |],
                { x with maxLengthRestrictedTo8String = "I am too long" }
            """minLengthRestrictedTo3String = "!?" """, fun x ->
                queryResponse [| "minLengthRestrictedTo3String" |] [| "The field minLengthRestrictedTo3String must be a string or array type with a minimum length of '3'." |],
                { x with minLengthRestrictedTo3String = "!?" }
            """stringOfDDMMMPattern = "-1Feb" """, fun x ->
                queryResponse [| "stringOfDDMMMPattern" |] [| "The field stringOfDDMMMPattern must match the regular expression '^((3[0-1])|([0-2]?[0-9]))(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov)$'." |],
                { x with stringOfDDMMMPattern = "-1Feb" }
            """stringOfDDMMMPattern = "34Mar" """, fun x ->
                queryResponse [| "stringOfDDMMMPattern" |] [| "The field stringOfDDMMMPattern must match the regular expression '^((3[0-1])|([0-2]?[0-9]))(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov)$'." |],
                { x with stringOfDDMMMPattern = "34Mar" }
            """stringOfDDMMMPattern = "30Spartans" """, fun x ->
                queryResponse [| "stringOfDDMMMPattern" |] [| "The field stringOfDDMMMPattern must match the regular expression '^((3[0-1])|([0-2]?[0-9]))(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov)$'." |],
                { x with stringOfDDMMMPattern = "30Spartans" }
            """stringOfLengthBetween8And64ContainingAGiraffe = "Short!" """, fun x ->
                queryResponse [| "stringOfLengthBetween8And64ContainingAGiraffe" |]
                    [| "The field stringOfLengthBetween8And64ContainingAGiraffe must be a string or array type with a minimum length of '8'."
                       "The field stringOfLengthBetween8And64ContainingAGiraffe must match the regular expression 'giraffe'." |],
                { x with stringOfLengthBetween8And64ContainingAGiraffe = "Short!" }
            """stringOfLengthBetween8And64ContainingAGiraffe = "giraffe" """, fun x ->
                queryResponse [| "stringOfLengthBetween8And64ContainingAGiraffe" |] [| "The field stringOfLengthBetween8And64ContainingAGiraffe must be a string or array type with a minimum length of '8'." |],
                { x with stringOfLengthBetween8And64ContainingAGiraffe = "giraffe" }
            """stringOfLengthBetween8And64ContainingAGiraffe = "Not a Giraffe because of case sensitivity" """, fun x ->
                queryResponse [| "stringOfLengthBetween8And64ContainingAGiraffe" |] [| "The field stringOfLengthBetween8And64ContainingAGiraffe must match the regular expression 'giraffe'." |],
                { x with stringOfLengthBetween8And64ContainingAGiraffe = "Not a Giraffe because of case sensitivity" }
            """stringOfLengthBetween8And64ContainingAGiraffe = "giraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffe" """, fun x ->
                queryResponse [| "stringOfLengthBetween8And64ContainingAGiraffe" |] [| "The field stringOfLengthBetween8And64ContainingAGiraffe must be a string or array type with a maximum length of '64'." |],
                { x with stringOfLengthBetween8And64ContainingAGiraffe = "giraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffegiraffe" }
            """stringOfLengthBetween8And64ContainingAGiraffe = "This text describes a long-necked animal of ochre color with brown spots" """, fun x ->
                queryResponse [| "stringOfLengthBetween8And64ContainingAGiraffe" |]
                    [| "The field stringOfLengthBetween8And64ContainingAGiraffe must be a string or array type with a maximum length of '64'."
                       "The field stringOfLengthBetween8And64ContainingAGiraffe must match the regular expression 'giraffe'." |],
                { x with stringOfLengthBetween8And64ContainingAGiraffe = "This text describes a long-necked animal of ochre color with brown spots" }
            """integerDivisibleBy2 = 1 """, fun x ->
                queryResponse [| "integerDivisibleBy2" |] [| "The field integerDivisibleBy2 must be a multiple of 2." |],
                { x with integerDivisibleBy2 = 1 }
            """integerDivisibleBy2 = -1 """, fun x ->
                queryResponse [| "integerDivisibleBy2" |] [| "The field integerDivisibleBy2 must be a multiple of 2." |],
                { x with integerDivisibleBy2 = -1 }
            """integerBetween8And64 = 7 """, fun x ->
                queryResponse [| "integerBetween8And64" |] [| "The field integerBetween8And64 must be between 8 and 64." |],
                { x with integerBetween8And64 = 7 }
            """integerBetween8And64 = 65 """, fun x ->
                queryResponse [| "integerBetween8And64" |] [| "The field integerBetween8And64 must be between 8 and 64." |],
                { x with integerBetween8And64 = 65 }
            """integerBetween8And64Exclusive = 7 """, fun x ->
                queryResponse [| "integerBetween8And64Exclusive" |] [| "The field integerBetween8And64Exclusive must be between 8 and 63." |],
                { x with integerBetween8And64Exclusive = 7 }
            """integerBetween8And64Exclusive = 64 """, fun x ->
                queryResponse [| "integerBetween8And64Exclusive" |] [| "The field integerBetween8And64Exclusive must be between 8 and 63." |],
                { x with integerBetween8And64Exclusive = 64 }
            """integerBetween8ExclusiveAnd64 = 8 """, fun x ->
                queryResponse [| "integerBetween8ExclusiveAnd64" |] [| "The field integerBetween8ExclusiveAnd64 must be between 9 and 64." |],
                { x with integerBetween8ExclusiveAnd64 = 8 }
            """integerBetween8ExclusiveAnd64 = 65 """, fun x ->
                queryResponse [| "integerBetween8ExclusiveAnd64" |] [| "The field integerBetween8ExclusiveAnd64 must be between 9 and 64." |],
                { x with integerBetween8ExclusiveAnd64 = 65 }
            """integerBetween8ExclusiveAnd64Exclusive = 8 """, fun x ->
                queryResponse [| "integerBetween8ExclusiveAnd64Exclusive" |] [| "The field integerBetween8ExclusiveAnd64Exclusive must be between 9 and 63." |],
                { x with integerBetween8ExclusiveAnd64Exclusive = 8 }
            """integerBetween8ExclusiveAnd64Exclusive = 64 """, fun x ->
                queryResponse [| "integerBetween8ExclusiveAnd64Exclusive" |] [| "The field integerBetween8ExclusiveAnd64Exclusive must be between 9 and 63." |],
                { x with integerBetween8ExclusiveAnd64Exclusive = 64 }
            """integerGTE8 = 7 """, fun x ->
                queryResponse [| "integerGTE8" |] [| "The field integerGTE8 must be between 8 and 2147483647." |],
                { x with integerGTE8 = 7 }
            """integerGTE8 = Int32.MinValue """, fun x ->
                queryResponse [| "integerGTE8" |] [| "The field integerGTE8 must be between 8 and 2147483647." |],
                { x with integerGTE8 = Int32.MinValue }
            """integerGT8 = 8 """, fun x ->
                queryResponse [| "integerGT8" |] [| "The field integerGT8 must be between 9 and 2147483647." |],
                { x with integerGT8 = 8 }
            """integerGT8 = Int32.MinValue """, fun x ->
                queryResponse [| "integerGT8" |] [| "The field integerGT8 must be between 9 and 2147483647." |],
                { x with integerGT8 = Int32.MinValue }
            """integerLTE64 = 65 """, fun x ->
                queryResponse [| "integerLTE64" |] [| "The field integerLTE64 must be between -2147483648 and 64." |],
                { x with integerLTE64 = 65 }
            """integerLTE64 = Int32.MaxValue """, fun x ->
                queryResponse [| "integerLTE64" |] [| "The field integerLTE64 must be between -2147483648 and 64." |],
                { x with integerLTE64 = Int32.MaxValue }
            """integerLT64 = 64 """, fun x ->
                queryResponse [| "integerLT64" |] [| "The field integerLT64 must be between -2147483648 and 63." |],
                { x with integerLT64 = 64 }
            """integerLT64 = Int32.MaxValue """, fun x ->
                queryResponse [| "integerLT64" |] [| "The field integerLT64 must be between -2147483648 and 63." |],
                { x with integerLT64 = Int32.MaxValue }
            """longEnum = -1L """, fun x ->
                queryResponse [| "longEnum" |] [| "The field longEnum must be a one of ['-9223372036854775808', '0', '9223372036854775807']." |],
                { x with longEnum = -1L }
            """longEnum = 1L """, fun x ->
                queryResponse [| "longEnum" |] [| "The field longEnum must be a one of ['-9223372036854775808', '0', '9223372036854775807']." |],
                { x with longEnum = 1L }
            """longDivisibleBy2 = 1L """, fun x ->
                queryResponse [| "longDivisibleBy2" |] [| "The field longDivisibleBy2 must be a multiple of 2." |],
                { x with longDivisibleBy2 = 1L }
            """longDivisibleBy2 = -1L """, fun x ->
                queryResponse [| "longDivisibleBy2" |] [| "The field longDivisibleBy2 must be a multiple of 2." |],
                { x with longDivisibleBy2 = -1L }
            """longBetween8And64 = 7L """, fun x ->
                queryResponse [| "longBetween8And64" |] [| "The field longBetween8And64 must be between 8 and 64." |],
                { x with longBetween8And64 = 7L }
            """longBetween8And64 = 65L """, fun x ->
                queryResponse [| "longBetween8And64" |] [| "The field longBetween8And64 must be between 8 and 64." |],
                { x with longBetween8And64 = 65L }
            """longBetween8And64Exclusive = 7L """, fun x ->
                queryResponse [| "longBetween8And64Exclusive" |] [| "The field longBetween8And64Exclusive must be between 8 and 63." |],
                { x with longBetween8And64Exclusive = 7L }
            """longBetween8And64Exclusive = 64L """, fun x ->
                queryResponse [| "longBetween8And64Exclusive" |] [| "The field longBetween8And64Exclusive must be between 8 and 63." |],
                { x with longBetween8And64Exclusive = 64L }
            """longBetween8ExclusiveAnd64 = 8L """, fun x ->
                queryResponse [| "longBetween8ExclusiveAnd64" |] [| "The field longBetween8ExclusiveAnd64 must be between 9 and 64." |],
                { x with longBetween8ExclusiveAnd64 = 8L }
            """longBetween8ExclusiveAnd64 = 65L """, fun x ->
                queryResponse [| "longBetween8ExclusiveAnd64" |] [| "The field longBetween8ExclusiveAnd64 must be between 9 and 64." |],
                { x with longBetween8ExclusiveAnd64 = 65L }
            """longBetween8ExclusiveAnd64Exclusive = 8L """, fun x ->
                queryResponse [| "longBetween8ExclusiveAnd64Exclusive" |] [| "The field longBetween8ExclusiveAnd64Exclusive must be between 9 and 63." |],
                { x with longBetween8ExclusiveAnd64Exclusive = 8L }
            """longBetween8ExclusiveAnd64Exclusive = 64L """, fun x ->
                queryResponse [| "longBetween8ExclusiveAnd64Exclusive" |] [| "The field longBetween8ExclusiveAnd64Exclusive must be between 9 and 63." |],
                { x with longBetween8ExclusiveAnd64Exclusive = 64L }
            """longGTE8 = 7L """, fun x ->
                queryResponse [| "longGTE8" |] [| "The field longGTE8 must be between 8 and 9223372036854775807." |],
                { x with longGTE8 = 7L }
            """longGTE8 = Int64.MinValue """, fun x ->
                queryResponse [| "longGTE8" |] [| "The field longGTE8 must be between 8 and 9223372036854775807." |],
                { x with longGTE8 = Int64.MinValue }
            """longGT8 = 8L """, fun x ->
                queryResponse [| "longGT8" |] [| "The field longGT8 must be between 9 and 9223372036854775807." |],
                { x with longGT8 = 8L }
            """longGT8 = Int64.MinValue """, fun x ->
                queryResponse [| "longGT8" |] [| "The field longGT8 must be between 9 and 9223372036854775807." |],
                { x with longGT8 = Int64.MinValue }
            """longLTE64 = 65L """, fun x ->
                queryResponse [| "longLTE64" |] [| "The field longLTE64 must be between -9223372036854775808 and 64." |],
                { x with longLTE64 = 65L }
            """longLTE64 = Int64.MaxValue """, fun x ->
                queryResponse [| "longLTE64" |] [| "The field longLTE64 must be between -9223372036854775808 and 64." |],
                { x with longLTE64 = Int64.MaxValue }
            """longLT64 = 64L """, fun x ->
                queryResponse [| "longLT64" |] [| "The field longLT64 must be between -9223372036854775808 and 63." |],
                { x with longLT64 = 64L }
            """longLT64 = Int64.MaxValue """, fun x ->
                queryResponse [| "longLT64" |] [| "The field longLT64 must be between -9223372036854775808 and 63." |],
                { x with longLT64 = Int64.MaxValue }
            """floatEnum = 3.1415921 """, fun x ->
                queryResponse [| "floatEnum" |] [| "The field floatEnum must be a one of ['3.14', '3.141', '3.1415', '3.14159', '3.141592']." |],
                { x with floatEnum = 3.1415921 }
            """floatEnum = 3. """, fun x ->
                queryResponse [| "floatEnum" |] [| "The field floatEnum must be a one of ['3.14', '3.141', '3.1415', '3.14159', '3.141592']." |],
                { x with floatEnum = 3. }
            """floatEnum = 4. """, fun x ->
                queryResponse [| "floatEnum" |] [| "The field floatEnum must be a one of ['3.14', '3.141', '3.1415', '3.14159', '3.141592']." |],
                { x with floatEnum = 4. }
            """floatBetween8And64 = 7.99999999 """, fun x ->
                queryResponse [| "floatBetween8And64" |] [| "The field floatBetween8And64 must be between 8 and 64." |],
                { x with floatBetween8And64 = 7.99999999 }
            """floatBetween8And64 = 64.00000001 """, fun x ->
                queryResponse [| "floatBetween8And64" |] [| "The field floatBetween8And64 must be between 8 and 64." |],
                { x with floatBetween8And64 = 64.00000001 }
            """floatBetween8And64Exclusive = 7.99999999 """, fun x ->
                queryResponse [| "floatBetween8And64Exclusive" |] [| "The field floatBetween8And64Exclusive must be between 8 and 63.99999999." |],
                { x with floatBetween8And64Exclusive = 7.99999999 }
            """floatBetween8And64Exclusive = 64. """, fun x ->
                queryResponse [| "floatBetween8And64Exclusive" |] [| "The field floatBetween8And64Exclusive must be between 8 and 63.99999999." |],
                { x with floatBetween8And64Exclusive = 64. }
            """floatBetween8ExclusiveAnd64 = 8. """, fun x ->
                queryResponse [| "floatBetween8ExclusiveAnd64" |] [| "The field floatBetween8ExclusiveAnd64 must be between 8.00000001 and 64." |],
                { x with floatBetween8ExclusiveAnd64 = 8. }
            """floatBetween8ExclusiveAnd64 = 64.00000001 """, fun x ->
                queryResponse [| "floatBetween8ExclusiveAnd64" |] [| "The field floatBetween8ExclusiveAnd64 must be between 8.00000001 and 64." |],
                { x with floatBetween8ExclusiveAnd64 = 64.00000001 }
            """floatBetween8ExclusiveAnd64Exclusive = 8. """, fun x ->
                queryResponse [| "floatBetween8ExclusiveAnd64Exclusive" |] [| "The field floatBetween8ExclusiveAnd64Exclusive must be between 8.00000001 and 63.99999999." |],
                { x with floatBetween8ExclusiveAnd64Exclusive = 8. }
            """floatBetween8ExclusiveAnd64Exclusive = 64. """, fun x ->
                queryResponse [| "floatBetween8ExclusiveAnd64Exclusive" |] [| "The field floatBetween8ExclusiveAnd64Exclusive must be between 8.00000001 and 63.99999999." |],
                { x with floatBetween8ExclusiveAnd64Exclusive = 64. }
            """floatGTE8 = 7.99999999 """, fun x ->
                queryResponse [| "floatGTE8" |] [| "The field floatGTE8 must be between 8 and ∞." |],
                { x with floatGTE8 = 7.99999999 }
            """floatGTE8 = Double.MinValue """, fun x ->
                queryResponse [| "floatGTE8" |] [| "The field floatGTE8 must be between 8 and ∞." |],
                { x with floatGTE8 = Double.MinValue }
            """floatGTE8 = Double.NegativeInfinity """, fun x ->
                queryResponse [| "floatGTE8" |] [| "The field floatGTE8 must be between 8 and ∞." |],
                { x with floatGTE8 = Double.NegativeInfinity }
            """floatGT8 = 8. """, fun x ->
                queryResponse [| "floatGT8" |] [| "The field floatGT8 must be between 8.00000001 and ∞." |],
                { x with floatGT8 = 8. }
            """floatGT8 = Double.MinValue """, fun x ->
                queryResponse [| "floatGT8" |] [| "The field floatGT8 must be between 8.00000001 and ∞." |],
                { x with floatGT8 = Double.MinValue }
            """floatGT8 = Double.NegativeInfinity """, fun x ->
                queryResponse [| "floatGT8" |] [| "The field floatGT8 must be between 8.00000001 and ∞." |],
                { x with floatGT8 = Double.NegativeInfinity }
            """floatLTE64 = 64.00000001 """, fun x ->
                queryResponse [| "floatLTE64" |] [| "The field floatLTE64 must be between -∞ and 64." |],
                { x with floatLTE64 = 64.00000001 }
            """floatLTE64 = Double.MaxValue """, fun x ->
                queryResponse [| "floatLTE64" |] [| "The field floatLTE64 must be between -∞ and 64." |],
                { x with floatLTE64 = Double.MaxValue }
            """floatLTE64 = Double.PositiveInfinity """, fun x ->
                queryResponse [| "floatLTE64" |] [| "The field floatLTE64 must be between -∞ and 64." |],
                { x with floatLTE64 = Double.PositiveInfinity }
            """floatLT64 = 64. """, fun x ->
                queryResponse [| "floatLT64" |] [| "The field floatLT64 must be between -∞ and 63.99999999." |],
                { x with floatLT64 = 64. }
            """floatLT64 = Double.MaxValue """, fun x ->
                queryResponse [| "floatLT64" |] [| "The field floatLT64 must be between -∞ and 63.99999999." |],
                { x with floatLT64 = Double.MaxValue }
            """floatLT64 = Double.PositiveInfinity """, fun x ->
                queryResponse [| "floatLT64" |] [| "The field floatLT64 must be between -∞ and 63.99999999." |],
                { x with floatLT64 = Double.PositiveInfinity }
        |]
    let private validQueryVariations = enumerateValidVariations validQuery validQueryForkers

    let private validNestedValidationObjectOfIntGTE2: SpecWithValidation.nestedValidationObjectOfIntGTE2 =
        {
            nestedIntGT2 = 32
        }
    let private validDeeplyNestedValidation: SpecWithValidation.deeplyNestedValidation =
        {
            intGTE2ObjectOption =
                Some {
                    intGTE2Option = Some 32
                }
        }

    let validBody: SpecWithValidation.TestValidationPostBodyJson=
        {
            arrayOf2To4UniqueItems = [|1;2;3|]
            arrayOf2To4Items = [|1;2;3|]
            arrayOptionOfMinimum2Items = Some [|1;2|]
            arrayOfMaximum4Items = [|1;2|]
            nestedValidationArrayOfIntGTE2 = [|5;6;7|]
            nestedValidationObjectOfIntGTE2 = validNestedValidationObjectOfIntGTE2
            nestedValidationOptionOfIntGTE2 = Some 32
            deeplyNestedValidation =  Some [| [| validDeeplyNestedValidation |] |]
        }
    let private validBodyForkers: (string*(SpecWithValidation.TestValidationPostBodyJson -> SpecWithValidation.TestValidationPostBodyJson)) array =
        [|
            "arrayOf2To4Items = [|2;2|] ",fun x -> { x with arrayOf2To4Items = [|2;2|] }
            "arrayOf2To4Items = [|2;2;2;2|] ",fun x -> { x with arrayOf2To4Items = [|2;2;2;2|] }
            "arrayOfMinimum2Items = None ",fun x -> { x with arrayOptionOfMinimum2Items = None }
            "arrayOfMaximum4Items = [||] ",fun x -> { x with arrayOfMaximum4Items = [||] }
            "arrayOfMaximum4Items = [|1;2;3;4|] ",fun x -> { x with arrayOfMaximum4Items = [|1;2;3;4|] }
            "nestedValidationArrayOfIntGTE2 = [|2;2;2|] ",fun x -> { x with nestedValidationArrayOfIntGTE2 = [|2;2;2|] }
            "nestedValidationObjectOfIntGTE2 = { validNestedValidationObjectOfIntGTE2 with nestedIntGT2 = 3 } ",fun x -> { x with nestedValidationObjectOfIntGTE2 = { validNestedValidationObjectOfIntGTE2 with nestedIntGT2 = 3 } }
            "nestedValidationOptionOfIntGTE2 = None ",fun x -> { x with nestedValidationOptionOfIntGTE2 = None }
            "nestedValidationOptionOfIntGTE2 = Some 2 ",fun x -> { x with nestedValidationOptionOfIntGTE2 = Some 2 }
            "deeplyNestedValidation = None ",fun x -> { x with deeplyNestedValidation = None }
            "deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = None } |] |] ",
            fun x -> { x with deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = None } |] |] }
            "deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = None } } |] |] ",
            fun x -> { x with deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = None } } |] |] }
            "deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 2 } } |] |] ",
            fun x -> { x with deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 2 } } |] |] }
            "deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 2 } } |]; [||] |] ",
            fun x -> { x with deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 2 } } |]; [||] |] }
        |]
    let private bodyResponse = response "body"
    let private bodyResponseMultipleProperty messages =
        let v: SpecWithValidation.validationErrorResponse =
            {
                otherErrors = None
                validationErrors =
                    messages
                    |> Array.map (fun (prop, msg) -> validationResult "body" [|prop|] msg)
            }
        v
    let private invalidBodyMakers: (string*(SpecWithValidation.TestValidationPostBodyJson -> struct (SpecWithValidation.validationErrorResponse*SpecWithValidation.TestValidationPostBodyJson))) array =
        [|
            """arrayOf2To4UniqueItems = [|1;1;1|] """, fun x ->
                bodyResponse [| "arrayOf2To4UniqueItems" |] [| "The field arrayOf2To4UniqueItems must contain only unique items." |],
                { x with arrayOf2To4UniqueItems = [|1;1;1|] }
            """arrayOf2To4UniqueItems = [|1;2;1;2;1;2|] """, fun x ->
                bodyResponse [| "arrayOf2To4UniqueItems" |]
                    [|
                        "The field arrayOf2To4UniqueItems must be a string or array type with a maximum length of '4'."
                        "The field arrayOf2To4UniqueItems must contain only unique items."
                    |],
                { x with arrayOf2To4UniqueItems = [|1;2;1;2;1;2|] }
            """arrayOf2To4Items = [||] """, fun x ->
                bodyResponse [| "arrayOf2To4Items" |] [| "The field arrayOf2To4Items must be a string or array type with a minimum length of '2'." |],
                { x with arrayOf2To4Items = [||] }
            """arrayOf2To4Items = [|1|] """, fun x ->
                bodyResponse [| "arrayOf2To4Items" |] [| "The field arrayOf2To4Items must be a string or array type with a minimum length of '2'." |],
                { x with arrayOf2To4Items = [|1|] }
            """arrayOf2To4Items = [|1;2;3;4;5|] """, fun x ->
                bodyResponse [| "arrayOf2To4Items" |] [| "The field arrayOf2To4Items must be a string or array type with a maximum length of '4'." |],
                { x with arrayOf2To4Items = [|1;2;3;4;5|] }
            """arrayOfMaximum4Items = [|1;2;3;4;5;6|] """, fun x ->
                bodyResponse [| "arrayOfMaximum4Items" |] [| "The field arrayOfMaximum4Items must be a string or array type with a maximum length of '4'." |],
                { x with arrayOfMaximum4Items = [|1;2;3;4;5;6|] }
            """arrayOptionOfMinimum2Items = Some [||] """, fun x ->
                bodyResponse [| "arrayOptionOfMinimum2Items.Value" |] [| "The field arrayOptionOfMinimum2Items.Value must be a string or array type with a minimum length of '2'." |],
                { x with arrayOptionOfMinimum2Items = Some [||] }
            """arrayOptionOfMinimum2Items = Some [|1|] """, fun x ->
                bodyResponse [| "arrayOptionOfMinimum2Items.Value" |] [| "The field arrayOptionOfMinimum2Items.Value must be a string or array type with a minimum length of '2'." |],
                { x with arrayOptionOfMinimum2Items = Some [|1|] }
            """nestedValidationArrayOfIntGTE2 = [|1;2;2|] """, fun x ->
                bodyResponse [| "nestedValidationArrayOfIntGTE2[0]" |] [| "The field nestedValidationArrayOfIntGTE2[0] must be between 2 and 2147483647." |],
                { x with nestedValidationArrayOfIntGTE2 = [|1;2;2|] }
            """nestedValidationArrayOfIntGTE2 = [|2;1;2|] """, fun x ->
                bodyResponseMultipleProperty
                    [|
                        "nestedValidationArrayOfIntGTE2[1]", "The field nestedValidationArrayOfIntGTE2[1] must be between 2 and 2147483647."
                        "nestedValidationArrayOfIntGTE2[2]", "The field nestedValidationArrayOfIntGTE2[2] must be between 2 and 2147483647."
                    |],
                { x with nestedValidationArrayOfIntGTE2 = [|2;1;1|] }
            """nestedValidationObjectOfIntGTE2 = { validNestedValidationObjectOfIntGTE2 with nestedIntGT2 = 1 } """, fun x ->
                bodyResponse [| "nestedIntGT2" |] [| "The field nestedIntGT2 must be between 2 and 2147483647." |],
                { x with nestedValidationObjectOfIntGTE2 = { validNestedValidationObjectOfIntGTE2 with nestedIntGT2 = 1 } }
            """nestedValidationOptionOfIntGTE2 = Some 1 """, fun x ->
                bodyResponse [| "nestedValidationOptionOfIntGTE2.Value" |] [| "The field nestedValidationOptionOfIntGTE2.Value must be between 2 and 2147483647." |],
                { x with nestedValidationOptionOfIntGTE2 = Some 1 }
            """deeplyNestedValidation = Some [| [| validDeeplyNestedValidation; validDeeplyNestedValidation |] |] """, fun x ->
                bodyResponse [| "deeplyNestedValidation.Value[0]" |] [| "The field deeplyNestedValidation.Value[0] must contain only unique items." |],
                { x with deeplyNestedValidation = Some [| [| validDeeplyNestedValidation; validDeeplyNestedValidation |] |] }
            """deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 0 } } |] |] """, fun x ->
                bodyResponse [| "intGTE2Option.Value" |] [| "The field intGTE2Option.Value must be between 2 and 2147483647." |],
                { x with deeplyNestedValidation = Some [| [| { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 0 } } |] |] }
            """deeplyNestedValidation = Some [| [| nonUniqueInvalidItem; nonUniqueInvalidItem |]; [| validItem; invalidItem |] |]""", fun x ->
                bodyResponseMultipleProperty
                    [|
                        "deeplyNestedValidation.Value[0]", "The field deeplyNestedValidation.Value[0] must contain only unique items."
                        "intGTE2Option.Value", "The field intGTE2Option.Value must be between 2 and 2147483647."
                        "intGTE2Option.Value", "The field intGTE2Option.Value must be between 2 and 2147483647."
                        "intGTE2Option.Value", "The field intGTE2Option.Value must be between 2 and 2147483647."
                    |],
                { x with deeplyNestedValidation = Some [|
                        [|
                            { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 0 } }
                            { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 0 } }
                        |]
                        [|
                            validDeeplyNestedValidation
                            { validDeeplyNestedValidation with intGTE2ObjectOption = Some { intGTE2Option = Some 0 } }
                        |]
                    |] }
        |]
    let private validBodyVariations = enumerateValidVariations validBody validBodyForkers

    let private mergeExpectedResults (a: SpecWithValidation.validationErrorResponse) (b: SpecWithValidation.validationErrorResponse) =
        let v: SpecWithValidation.validationErrorResponse =
            {
                otherErrors = None
                validationErrors = Array.append a.validationErrors b.validationErrors
            }
        v
    
    let allValidCases =
        let make description body path query = [| box description; box body; box (path, query) |]
        [|
            for desc, body in validBodyVariations do
                make desc body validPath validQuery
            for desc, path in validPathVariations do
                make desc validBody path validQuery
            for desc, query in validQueryVariations do
                make desc validBody validPath query
        |] |> Array.sortBy (Array.head >> unbox<string>)
    let allInvalidCases =
        let make desc body path query err = [| box desc; box body; box (path, query); err |]
        [|
            for desc, makeInvalid in invalidBodyMakers do
                let struct (err, body) = makeInvalid validBody
                make desc body validPath validQuery err
            for desc, makeInvalid in invalidPathMakers do
                let struct (err, path) = makeInvalid validPath
                make desc validBody path validQuery err
            for desc, makeInvalid in invalidQueryMakers do
                let struct (err, query) = makeInvalid validQuery
                make desc validBody validPath query err
        |] |> Array.sortBy (Array.head >> unbox<string>)

type InvalidCasesEnumerator() = 
    interface seq<obj []> with
        member this.GetEnumerator() = (Seq.ofArray ValidationCases.allInvalidCases).GetEnumerator()
        member this.GetEnumerator() = 
            ValidationCases.allInvalidCases.GetEnumerator()

type ValidCasesEnumerator() = 
    interface seq<obj []> with
        member this.GetEnumerator() = (Seq.ofArray ValidationCases.allValidCases).GetEnumerator()
        member this.GetEnumerator() = 
            ValidationCases.allValidCases.GetEnumerator()
    
type SpecWithValidationTests() =
    let errorsReducer (validation, other) (v, o) =
        Seq.append validation v,
        Option.map2 (sprintf "%s\n%s") other o
        |> Option.orElse other
        |> Option.orElse o
    let rec errInnerToResponse location err =
        match err with
        | SpecWithValidation.ArgumentValidationError res ->
            seq {
                for err in res do
                    let v: SpecWithValidation.validationErrors =
                        {
                            location = location
                            propertyPath = err.MemberNames |> Seq.toArray
                            message = err.ErrorMessage
                        }
                    v
            }, None
        | SpecWithValidation.CombinedArgumentErrors combined ->
            combined
            |> Seq.map (errInnerToResponse location)
            |> Seq.reduce errorsReducer
        | _ -> Seq.empty, SpecWithValidation.argErrorToString 0 err |> Some
    let rec errOuterToResponse err =
        match err with
            | SpecWithValidation.BodyBindingError inner -> errInnerToResponse "body" inner
            | SpecWithValidation.PathBindingError inner -> errInnerToResponse "path" inner
            | SpecWithValidation.QueryBindingError inner -> errInnerToResponse "query" inner
            | SpecWithValidation.CombinedArgumentLocationError combined ->
                combined
                |> Seq.map errOuterToResponse
                |> Seq.reduce errorsReducer
    let errToResponse err =
        let validation, other = errOuterToResponse err
        let e: SpecWithValidation.validationErrorResponse =
            {validationErrors = validation |> Seq.toArray
             otherErrors = other}
        e
    let specWithValidationService=
        {
            new SpecWithValidation.Service() with
                member _.TestValidationInput ((_, _, _)) =
                    task {
                        return Choice1Of2 "ok"
                    }
                member _.TestValidationInputError ((err, _)) =
                    task {
                        return Choice2Of2 (errToResponse err)
                    }
                
        }
        
    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe SpecWithValidation.webApp
        
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

    let webHostBuilder =
        WebHost.CreateDefaultBuilder()
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .ConfigureLogging(configureLogging)
            
    let server = new TestServer(webHostBuilder)
    let client = server.CreateClient()
    
    let toUrlString (path: SpecWithValidation.PostTestValidationPath, query: SpecWithValidation.PostTestValidationQuery) =
        let query =
            seq {
                for prop in query.GetType().GetProperties(System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Instance) do
                    let value = prop.GetValue query
                    if value <> null then
                        sprintf "%s=%s" prop.Name (string value |> Uri.EscapeDataString)
            } |> String.concat "&"
        let pathStringParam = if isNull path.stringOfRestrictedToBeBetween3And8Length then "" else Uri.EscapeDataString path.stringOfRestrictedToBeBetween3And8Length
        sprintf "/test-validation/%d/%s?%s" path.versionEnum pathStringParam query
    let toContent (s: SpecWithValidation.TestValidationPostBodyJson) =
            let json = JsonConvert.SerializeObject([|s|], jsonSettings)
            let content = new StringContent(json, Encoding.UTF8, "application/json")
            content
    
    [<Theory>]
    [<ClassData(typeof<ValidCasesEnumerator>)>]
    let ``Valid request -> 200 "ok"`` (description: string, body, pnq)  = task {
        do ignore description
        use content = toContent body
        let url = toUrlString pnq
        let! response = client.PostAsync(url, content)
        let! responseText = response.Content.ReadAsStringAsync()
        do Assert.Equal("\"ok\"", responseText)
        do Assert.Equal(HttpStatusCode.OK, response.StatusCode)
    }
    
    [<Theory>]
    [<ClassData(typeof<InvalidCasesEnumerator>)>]
    let ``Invalid request -> 400 "{json description}"`` (description: string, body, pnq, err) = task {
        do ignore description
        use content = toContent body
        let url = toUrlString pnq
        let! response = client.PostAsync(url, content)
        let! responseText = response.Content.ReadAsStringAsync()
        let responseJson = JsonConvert.DeserializeObject<SpecWithValidation.validationErrorResponse>(responseText, jsonSettings)
        do Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        do Assert.Equal(err, responseJson)
    }