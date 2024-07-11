open Fable.Core
open Thoth.Json

// NOTE: with StringEnum, below prints "success: Invalid"
//       without StringEnum, it prints "The following `failure` occurred with the decoder: Cannot find case Invalid in Program.State"
[<StringEnum>]
type State =
    | On
    | Off

let inline decoder<'T> = Decode.Auto.generateDecoder<'T> ()

match Decode.fromString decoder<State> "\"Invalid\"" with
| Error error -> printfn "failure: %s" error
| Ok value -> printfn "success: %s" (value.ToString())
