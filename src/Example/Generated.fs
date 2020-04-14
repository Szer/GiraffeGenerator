///<summary>
///This is very simple API
///</summary>
[<RequireQualifiedAccess>]
module SimpleAPIoverview

open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

///<summary>
///This is apis
///</summary>
type apis =
    { apiKey: string
      apiVersionNumber: string
      apiUrl: System.Uri
      apiCount: int64
      apiAvg: double
      isInternal: bool
      start: System.DateTimeOffset
      apiHash: byte array }

///<summary>
///This is data set list
///</summary>
and dataSetList =
    { total: int
      apis: apis array }

[<AbstractClass>]
type Service() =

    ///<summary>
    ///This is very cool API for list API versions
    ///</summary>
    ///<remarks>
    ///List API versions
    ///</remarks>
    abstract listVersionsv2: HttpHandler

    abstract listVersionsv2Input: HttpContext -> Task<Choice<dataSetList, bool>>
    abstract listVersionsv2Output: Choice<dataSetList, bool> -> HttpHandler

    ///<summary>
    ///This is even cooler API for listing detail versions
    ///</summary>
    ///<remarks>
    ///List API version details
    ///</remarks>
    abstract getVersionDetailsv2: HttpHandler

    abstract getVersionDetailsv2Input: HttpContext -> Task<Choice<{| subscriptionId: string |}, bool>>
    abstract getVersionDetailsv2Output: Choice<{| subscriptionId: string |}, bool> -> HttpHandler

let webApp: HttpHandler =
    fun next ctx ->
        task {
            let service = ctx.GetService<Service>()
            return! choose
                        [ route "/" >=> GET >=> service.listVersionsv2
                          route "/v2" >=> GET >=> service.getVersionDetailsv2
                           ] next ctx
        }
