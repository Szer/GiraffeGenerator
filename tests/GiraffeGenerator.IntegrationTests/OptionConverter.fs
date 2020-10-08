namespace GiraffeGenerator.IntegrationTests

[<AutoOpen>]
module OptionConverter =

  open Newtonsoft.Json
  open Microsoft.FSharp.Reflection
  open System

  /// see `optionConverter`
  type OptionConverter() =
    inherit JsonConverter()

    override __.CanConvert t =
      t.IsGenericType
      && typedefof<option<_>>.Equals (t.GetGenericTypeDefinition())

    override __.WriteJson(writer, value, serializer) =
      let value =
        if isNull value then
          null
        else 
          let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
          fields.[0]
      serializer.Serialize(writer, value)

    override __.ReadJson(reader, t, _, serializer) =
      let innerType = t.GetGenericArguments().[0]

      let innerType = 
        if innerType.IsValueType then
          typedefof<Nullable<_>>.MakeGenericType([| innerType |])
        else
          innerType

      let value = serializer.Deserialize(reader, innerType)
      let cases = FSharpType.GetUnionCases t

      if isNull value then
        FSharpValue.MakeUnion(cases.[0], [||])
      else
        FSharpValue.MakeUnion(cases.[1], [|value|])

  /// `Newtonsoft.Json` converter which could (de-)serialize `Option<'a>` values:
  ///
  /// from:
  /// ```fsharp
  /// type Record =
  ///     { Id  : string
  ///       Name: string option }
  /// let withName = { Id = "1"; Name = Some "A" }
  /// let woName = { Id = "2"; Name = None }
  /// ```
  ///
  /// to jsons:
  ///
  /// ```json
  /// {
  ///     "Id": "1",
  ///     "Name": "A"
  /// }
  /// ```
  ///
  /// ```json
  /// {
  ///     "Id": "2",
  ///     "Name": null
  /// }
  /// ```
  let optionConverter = OptionConverter() :> JsonConverter