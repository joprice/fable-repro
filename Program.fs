open FSharp.Reflection

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
[<RequireQualifiedAccess>]
type State =
    | On
    | Off

type State2 =
    | On
    | Off

// #if FABLE_COMPILER
// [<Erase>]
// #endif
[<StringEnum>]
type State3 =
    | On
    | Off

module State3 =
    // let On: State3 = unbox "On"
    // let Off: State3 = unbox "Off"

    let encoder: Encoder<State3> =
        function
        | On -> Encode.string "On"
        | Off -> Encode.string "Off"

    let decoder: Decoder<State3> =
        Decode.string
        |> Decode.andThen (function
            | "On" -> Decode.succeed On
            | "Off" -> Decode.succeed Off
            | other -> Decode.fail $"Invalid enum value: {other}. Expected one of On,Off")

[<Erase>]
type State4 =
    | On
    | Off

type State5 = State5 of string

let inline decode<'T> value =
    printfn "%s" typeof<'T>.FullName
    let extra = Extra.empty |> Extra.withCustom State3.encoder State3.decoder
    let decoder = Decode.Auto.generateDecoder<'T> (extra = extra)

    match Decode.fromString decoder value with
    | Error error -> printfn "failure: %s" error
    | Ok value -> printfn "success: %s" (value.ToString())

let name1 = typeof<State>.FullName
let name3 = typeof<State3>.FullName
let name4 = typeof<State4>.FullName
printfn "%s %s %s" name1 name3 name4

let a = State4.On

decode<State> "\"Invalid\""
decode<string> "\"Invalid\""
decode<State4> "\"Invalid\""
decode<State5> "\"Invalid\""
// decode<State2> "\"Invalid\""
// decode<State3> "\"Invalid\""

// let t = typeof<State>
// let isUnion = FSharpType.IsUnion(t, allowAccessToPrivateRepresentation = true)
// // let uci = FSharpType.GetUnionCases(t, allowAccessToPrivateRepresentation = true)
//
// // let info, fields =
// //     FSharpValue.GetUnionFields(On, t, allowAccessToPrivateRepresentation = true)
//
// printfn "union: %b " isUnion
// //printfn "union: %b %s" isUnion (info.ToString())
