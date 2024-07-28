open Fable.Core.Testing
open Thoth.Json.Core
open Thoth.Json.JavaScript

module State =
    type State =
        | On
        | Off

    let On = On
    let Off = Off

    let encoder: Encoder<State> =
        function
        | On -> Encode.string "on"
        | Off -> Encode.string "off"

    let decoder: Decoder<State> =
        Decode.string
        |> Decode.andThen (function
            | "on" -> Decode.succeed On
            | "off" -> Decode.succeed Off
            | other -> Decode.fail $"Invalid dataset {other}")

let data = """{"state":"On"}"""

match Decode.fromString State.decoder data with
| Ok value ->
    let eq = value = State.On

    let refEq = LanguagePrimitives.PhysicalEquality value State.On

    Assert.AreEqual(eq, true)
    Assert.AreEqual(refEq, true)
    let json = Encode.toString 0 (State.encoder value)
    Assert.AreEqual(json, """{"state":"On"}""")
| Error error -> failwith error
