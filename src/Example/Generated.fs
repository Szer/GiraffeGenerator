///<summary>
///This is very simple API
///</summary>
[<RequireQualifiedAccess>]
module SimpleAPIoverview

open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

[<CLIMutable>]
type GetListSearchableFields =
    { dataset: string
      version: string }

and [<CLIMutable>] PostPerformSearch =
    { version: string
      dataset: string }

///<summary>
///This is apis
///</summary>
and [<CLIMutable>] apis =
    { apiKey: string
      apiVersionNumber: string
      apiUrl: System.Uri
      apiDocumentationUrl: System.Uri
      apiCount: int64
      apiAvg: double
      isInternal: bool
      start: System.DateTimeOffset
      apiHash: byte array }

///<summary>
///This is data set list
///</summary>
and [<CLIMutable>] dataSetList =
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
    abstract ListVersionsv2: HttpHandler

    abstract ListVersionsv2Input: HttpContext -> Task<Choice<dataSetList, bool>>
    abstract ListVersionsv2Output: Choice<dataSetList, bool> -> HttpHandler

    ///<summary>
    ///This is even cooler API for listing detail versions
    ///</summary>
    ///<remarks>
    ///List API version details
    ///</remarks>
    abstract GetVersionDetailsv2: HttpHandler

    abstract GetVersionDetailsv2Input: HttpContext -> Task<Choice<{| subscriptionId: string |}, bool>>
    abstract GetVersionDetailsv2Output: Choice<{| subscriptionId: string |}, bool> -> HttpHandler

    ///<summary>
    ///This GET API returns the list of all the searchable field names that are in the oa_citations. Please see the 'fields' attribute which returns an array of field names. Each field or a combination of fields can be searched using the syntax options shown below.
    ///</summary>
    ///<remarks>
    ///Provides the general information about the API and the list of fields that can be used to query the dataset.
    ///</remarks>
    abstract ListSearchableFields: GetListSearchableFields -> HttpHandler

    abstract ListSearchableFieldsInput: (GetListSearchableFields * HttpContext) -> Task<Choice<string, string>>
    abstract ListSearchableFieldsOutput: Choice<string, string> -> HttpHandler

    ///<summary>
    ///This API is based on Solr/Lucense Search. The data is indexed using SOLR. This GET API returns the list of all the searchable field names that are in the Solr Index. Please see the 'fields' attribute which returns an array of field names. Each field or a combination of fields can be searched using the Solr/Lucene Syntax. Please refer https://lucene.apache.org/core/3_6_2/queryparsersyntax.html#Overview for the query syntax. List of field names that are searchable can be determined using above GET api.
    ///</summary>
    ///<remarks>
    ///Provides search capability for the data set with the given search criteria.
    ///</remarks>
    abstract PerformSearch: PostPerformSearch -> HttpHandler

    abstract PerformSearchInput: (PostPerformSearch * HttpContext) -> Task<Choice<obj array, unit>>
    abstract PerformSearchOutput: Choice<obj array, unit> -> HttpHandler

let webApp: HttpHandler =
    fun next ctx ->
        task {
            let service = ctx.GetService<Service>()
            return! choose
                        [ GET >=> route "/" >=> service.ListVersionsv2
                          GET >=> route "/v2" >=> service.GetVersionDetailsv2
                          GET >=> routeBind "/{dataset}/{version}/fields" service.ListSearchableFields
                          POST >=> routeBind "/{dataset}/{version}/records" service.PerformSearch
                           ] next ctx
        }
