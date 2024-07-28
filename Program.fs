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
        | On -> Encode.string "On"
        | Off -> Encode.string "Off"

    let decoder: Decoder<State> =
        Decode.string
        |> Decode.andThen (function
            | "On" -> Decode.succeed On
            | "Off" -> Decode.succeed Off
            | other -> Decode.fail $"Invalid dataset {other}")

let data = """{"state":"On"}"""

type Data = { state: State.State }

module Data =
    let decoder: Decoder<Data> =
        Decode.object (fun o -> { state = o.Required.Field "state" State.decoder })

    let encoder: Encoder<Data> =
        fun data -> Encode.object [ "state", State.encoder data.state ]

match Decode.fromString Data.decoder data with
| Ok value ->
    let eq = value = { state = State.On }

    let refEq = LanguagePrimitives.PhysicalEquality value.state State.On

    Assert.AreEqual(eq, true)
    Assert.AreEqual(refEq, true)
    let json = Encode.toString 0 (Data.encoder value)
    Assert.AreEqual(json, """{"state":"On"}""")
| Error error -> failwith error
