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
    }

let mutable value =
    { UseNodaTime = false
      AllowUnqualifiedAccess = false
      MapDateTimeInto = OffsetDateTime
      ModuleName = None }