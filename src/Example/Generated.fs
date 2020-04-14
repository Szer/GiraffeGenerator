[<RequireQualifiedAccess>]
module SimpleAPIoverview

open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe

type Service =
    abstract listVersionsv2: HttpHandler
    abstract getVersionDetailsv2: HttpHandler

let webApp: HttpHandler =
    fun next ctx ->
        task {
            let service = ctx.GetService<Service>()
            return! choose
                        [ route "/" >=> GET >=> service.listVersionsv2
                          route "/v2" >=> GET >=> service.getVersionDetailsv2
                           ] next ctx
        }
