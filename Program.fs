open Thoth.Json
open Fable.Core.Testing

module State =
    type State =
        | On
        | Off

    let On = On
    let Off = Off

    let decoder: Thoth.Json.Decoder<State> =
        let decoder = Decode.Auto.generateDecoder<State> ()

        decoder
        |> Decode.map (function
            | On -> On
            | Off -> Off)

    let encoder: Thoth.Json.Encoder<State> = Encode.Auto.generateEncoder<State> ()

type Wrapper = { state: State.State }

module Wrapper =
    let extra = Extra.empty |> Extra.withCustom State.encoder State.decoder
    let decoder = Decode.Auto.generateDecoder<Wrapper> (extra = extra)
    let encoder = Encode.Auto.generateEncoder<Wrapper> ()

let data = """{"state":"On"}"""

match Decode.fromString Wrapper.decoder data with
| Ok value ->
    let eq = value.state = State.On

    let refEq = LanguagePrimitives.PhysicalEquality value.state State.On

    Assert.AreEqual(eq, true)
    Assert.AreEqual(refEq, true)
    let json = Encode.toString 0 value
    Assert.AreEqual(json, """{"state":"On"}""")
| Error error -> failwith error
