[<RequireQualifiedAccess>]
module SimpleAPIoverview

open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

type apis =
    { apiKey: string
      apiVersionNumber: string
      apiUrl: System.Uri
      apiCount: int64
      apiAvg: double
      isInternal: bool
      start: System.DateTimeOffset
      apiHash: byte array }

and dataSetList =
    { total: int
      apis: apis array }

[<AbstractClass>]
type Service() =
    abstract listVersionsv2: HttpHandler
    abstract listVersionsv2Input: HttpContext -> Task<Choice<dataSetList, string>>
    abstract listVersionsv2Output: Choice<dataSetList, string> -> HttpHandler
    abstract getVersionDetailsv2: HttpHandler
    abstract getVersionDetailsv2Input: HttpContext -> Task<Choice<dataSetList, bool>>
    abstract getVersionDetailsv2Output: Choice<dataSetList, bool> -> HttpHandler

let webApp: HttpHandler =
    fun next ctx ->
        task {
            let service = ctx.GetService<Service>()
            return! choose
                        [ route "/" >=> GET >=> service.listVersionsv2
                          route "/v2" >=> GET >=> service.getVersionDetailsv2
                           ] next ctx
        }
