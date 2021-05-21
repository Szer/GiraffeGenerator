[<RequireQualifiedAccess>]
module Configuration

type DateTimeGeneratedType =
    | ZonedDateTime
    | OffsetDateTime
    | LocalDateTime
    | Instant

type Configuration =
    {
        UseNodaTime: bool
        AllowUnqualifiedAccess: bool
        MapDateTimeInto: DateTimeGeneratedType
        ModuleName: string option
        TaskBuilderNamespace: string
    }

let mutable value =
    { UseNodaTime = false
      AllowUnqualifiedAccess = false
      MapDateTimeInto = OffsetDateTime
      ModuleName = None
      TaskBuilderNamespace = "FSharp.Control.Tasks.V2.ContextInsensitive" }