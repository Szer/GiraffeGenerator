[<AutoOpen>]
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

