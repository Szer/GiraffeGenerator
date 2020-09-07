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
[<CLIMutable>]
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
and [<CLIMutable>] dataSetList = { total: int; apis: apis array }

[<AbstractClass>]
type Service() =
    ///<summary>
    ///This is very cool API for list API versions
    ///</summary>
    ///<remarks>
    ///List API versions
    ///</remarks>
    abstract ListVersionsv2: HttpHandler

    override this.ListVersionsv2 =
        fun next ctx ->
            task {
                let! input = this.ListVersionsv2Input ctx
                return! this.ListVersionsv2Output input next ctx
            }

    abstract ListVersionsv2Input: HttpContext -> Task<Choice<dataSetList, bool>>
    abstract ListVersionsv2Output: Choice<dataSetList, bool> -> HttpHandler

    override this.ListVersionsv2Output input =
        match input with
        | Choice1Of2 responseOn200 -> json responseOn200
        | Choice2Of2 responseOn300 -> setStatusCode 300 >=> json responseOn300

    ///<summary>
    ///This is even cooler API for listing detail versions
    ///</summary>
    ///<remarks>
    ///List API version details
    ///</remarks>
    abstract GetVersionDetailsv2: HttpHandler

    override this.GetVersionDetailsv2 =
        fun next ctx ->
            task {
                let! input = this.GetVersionDetailsv2Input ctx
                return! this.GetVersionDetailsv2Output input next ctx
            }

    abstract GetVersionDetailsv2Input: HttpContext -> Task<Choice<{| subscriptionId: string |}, bool>>
    abstract GetVersionDetailsv2Output: Choice<{| subscriptionId: string |}, bool> -> HttpHandler

    override this.GetVersionDetailsv2Output input =
        match input with
        | Choice1Of2 responseOn200 -> json responseOn200
        | Choice2Of2 responseOn203 -> setStatusCode 203 >=> json responseOn203

let webApp: HttpHandler =
    fun next ctx ->
        task {
            let service = ctx.GetService<Service>()
            return! choose
                        [ GET >=> route "/" >=> service.ListVersionsv2
                          GET
                          >=> route "/v2"
                          >=> service.GetVersionDetailsv2
                           ]
                        next
                        ctx
        }
