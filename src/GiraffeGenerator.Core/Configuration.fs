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
        MapDateTimeInto: DateTimeGeneratedType
        ModuleName: string option
    }

let mutable value =
    { UseNodaTime = false
      MapDateTimeInto = OffsetDateTime
      ModuleName = None }