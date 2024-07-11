#if FABLE_COMPILER
open Fable.Core
open Thoth.Json
#else
open Thoth.Json.Net
#endif

// NOTE: With StringEnum, below prints "success: Invalid"
//       Without StringEnum, it prints "The following `failure` occurred with the decoder: Cannot find case Invalid in Program.State"
//       Running with `dotnet run` gives the same result
#if FABLE_COMPILER
[<StringEnum>]
#endif
type State =
    | On
    | Off

let inline decoder<'T> = Decode.Auto.generateDecoder<'T> ()

match Decode.fromString decoder<State> "\"Invalid\"" with
| Error error -> printfn "failure: %s" error
| Ok value -> printfn "success: %s" (value.ToString())
