module SerializationTests

open Fable.Core
open Thoth.Json.JavaScript
// open Client

module CodecUtil =

    type Encoder<'a> = | Encoder
    type Decoder<'a> = | Decoder

    type Codec<'a> =
        { encoder: Encoder<'a>
          decoder: Decoder<'a> }

    open Thoth.Json.Core
    open System.Runtime.InteropServices
    // open FSharpPlus.Control
    // open FSharpPlus
    // open FSharpPlus.Internals

    // type Default6 = class end
    // type Default5 =
    //   class
    //     inherit Default6
    //   end
    // type Default4 =
    //   class
    //     inherit Default5
    //   end
    // type Default3 =
    //   class
    //     inherit Default4
    //   end
    // type Default2 =
    //   class
    //     inherit Default3
    //   end
    // type Default1 =
    //   class
    //     inherit Default2
    //   end

    // type Codecs =
    //   inherit Default2
    //   static member inline codec(_: Default1) : Codec<string> = Codec.string
    //   static member inline codec(_: Codecs) : Codec<int> = Codec.int


    (* Other way to define custom type class *)
    type Codecs =
        // inherit Default1

        static member Codecs(_output: int, _mthd: Codecs) : Codec<int> =
            { encoder = Encoder; decoder = Decoder }

        static member Codecs(_output: bool, _mthd: Codecs) : Codec<bool> =
            { encoder = Encoder; decoder = Decoder }

        static member Codecs(_output: string, _mthd: Codecs) : Codec<string> =
            { encoder = Encoder; decoder = Decoder }
        // static member Codecs(_output: float, _mthd: Codecs) : Codec<float> = Codec.float
        // static member Codecs(_output: bool, _mthd: Codecs) : Codec<bool> = Codec.bool
        // static member Codecs(_output: int, _mthd: Codecs) : Codec<int> = Codec.int
        // static member Codecs(_output: string, _mthd: Codecs) : Codec<string> = Codec.string
        // static member inline Codecs< ^a when (^a): (static member codecs: Codec< ^a >)>
        //   (_output: ^a, _mthd: Codecs)
        //   : Codec< ^a >
        //   =
        //   'a.codecs

        static member inline Invoke() =
            let inline call_2 (a: ^a, b: ^b) =
                ((^a or ^b): (static member Codecs: _ * _ -> _) b, a)

            let inline call (a: 'a) =
                call_2 (a, Unchecked.defaultof<'r>): Codec<'r>

            call Unchecked.defaultof<Codecs>

        static member inline Codecs< ^a, ^b, ^c
            when (^a): (static member codecInner: Codec< ^b > * Codec< ^c > -> Codec< ^a >)
            and (Codecs or ^b): (static member Codecs: ^b * Codecs -> Codec< ^b >)
            and (Codecs or ^c): (static member Codecs: ^c * Codecs -> Codec< ^c >)>
            (_output: ^a, _mthd: Codecs)
            : Codec< ^a > =
            // let a: Codec< ^b > = Codecs.Invoke()
            // let inline b = ((Codecs or ^b): (static member Codecs: ^b * Codecs -> Codec< ^b >) _output, _mthd)
            // let c: Codec< ^c > = Codecs.Invoke()
            // let f: (Codec< ^b > * Codec< ^c >) -> Codec< ^a > = 'a.codecInner
            'a.codecInner (Codecs.Invoke(), Codecs.Invoke())


    // static member inline Invoke() : '``Alternative<'T>`` =
    //   let inline call (mthd: ^M, output: ^R) =
    //     ((^M or ^R): (static member codec: _ * _ -> _) output, mthd)
    //   call (Unchecked.defaultof<Codecs>, Unchecked.defaultof<'``Alternative<'T>``>)


    type Codecs with
        // static member inline Codecs(_: System.Tuple<'a>, _: Codecs) =
        //   System.Tuple<_>(Codecs.Invoke()): System.Tuple<Codec<'a>>
        // static member inline Codecs(_: Id<'a>, _: Codecs) = Id<_>(Codecs.Invoke())
        static member inline Codecs(_: System.ValueTuple<Codec<'a>>, _: Codecs) =
            System.ValueTuple<_>(Codecs.Invoke()): System.ValueTuple<Codec<'a>>

        static member inline Codecs(_: 'T -> Codec<'Monoid>, _: Codecs) =
            (fun _ -> Codecs.Invoke()): 'T -> Codec<'Monoid>

    module Codecs =
        let inline codec< ^a when (Codecs or ^a): (static member Codecs: ^a * Codecs -> Codec< ^a >)> : Codec< ^a > =
            Codecs.Invoke()


    // let inline getCodec () : Codec<'a> = Codecs.Invoke()

    // let inline zero< ^Monoid
    //   when (Codecs or ^Monoid): (static member codec: ^Monoid * Codecs -> Codec< ^Monoid >)>
    //   : Codec< ^Monoid > =
    //   Codecs.Invoke()

    // let inlnie getCodec = Codecs.Invoke()
    //
    // let _: Codec<Test> = getCodec () //.codec<Test>
    // let _: Codec<Test> = getCodec () //.codec<Test>

    // module YesNoTypeClass =
    //   (* Other way to define custom type class *)
    //   type YesNoClass =
    //     | YesNoClass
    //
    //     static member YesNo(x: int) : bool =
    //       match x with
    //       | 0 -> false
    //       | _ -> true
    //
    //     static member YesNo(xs: List<_>) : int =
    //       match xs with
    //       | [] -> 1
    //       | _ -> 0
    //
    //     static member YesNo(b: bool) : bool = b
    //
    //   // static member YesNo(op: option<_>) =
    //   //     match op with
    //   //     | Some _ -> true
    //   //     | None -> false
    //
    //   module internal YesNoOverloads =
    //     let inline Invoke (_: ^a, b: ^b) : ^c =
    //       ((^a or ^b): (static member YesNo: ^b -> ^c) b)
    //
    //   let inline yesno (x: 'a) : 'b = YesNoOverloads.Invoke(YesNoClass, x)
    //

    // type Test =
    //   | Test of string
    //
    //   static member inline YesNo(_b: Test) : int = 0
    //
    //   static member codec(_: Test) : Codec<Test> =
    //     Codec.string |> Codec.map (Test) (fun (Test x) -> x)
    //
    // let _: bool = YesNoTypeClass.yesno 1
    // let _: int = YesNoTypeClass.yesno [ 1; 2 ]
    // let _: int = YesNoTypeClass.yesno (Test "a")

    // type Codecs with
    //   static member inline codec(_: 'T) : Codec<int> = Codec.int
    // static member inline codec<'T when ('T): (static member codec: Thoth.Json.Codec.Codec<'T>)> : Codec<'T> = 'T.codec
    // type HasCodec2<'T when (Codecs or 'T): (static member codec: ^a -> Thoth.Json.Codec.Codec<'T>)> = 'T

    type HasCodec<'T when 'T: (static member codec: Codec<'T>)> = 'T

open CodecUtil

module ApiResult =
    [<RequireQualifiedAccess>]
    type ApiResult<'T, 'E> =
        | Success of 'T
        | Error of 'E

        static member inline codecInner<'T, 'E> (t: Codec<'T>) (e: Codec<'E>) : Codec<ApiResult<'T, 'E>> =
            let encoder = t.encoder
            let errorEncoder = e.encoder
            { encoder = Encoder; decoder = Decoder }


    let inline codecDerive< ^T, ^E when HasCodec< ^E > and HasCodec< ^T >> : Codec<ApiResult< ^T, ^E >> =
        ApiResult.codecInner<'T, 'E> ('T.codec) ('E.codec)

type ApiResult<'T, 'E> = ApiResult.ApiResult<'T, 'E>

//         encoder
//         Codec.create
//           (function
//           | ApiResult.Success data -> encoder data
//           | ApiResult.Error error -> errorEncoder error)
//          //NOTE: this is only for testing and will not behave correctly. only encoder is used
//           (Decode.oneOf [
//             t.Decoder |> Decode.map ApiResult.Success
//             e.Decoder |> Decode.map ApiResult.Error
//           ])

//
// type Test =
//   | Test of string
//
//   static member codec(_: Test) : Codec<Test> =
//     Codec.string |> Codec.map (Test) (fun (Test x) -> x)
// // static member codec: (Codecs) -> Codec<Test> =
// //   fun _ -> Codec.string |> Codec.map (Test) (fun (Test x) -> x)
//
// let inline x< ^T when (Codecs or ^T): (static member codec: ^T -> Thoth.Json.Codec.Codec<'T>)>
//   (x: ^T)
//   = ((Codecs or ^T):  (static member codec: ^T -> Thoth.Json.Codec.Codec<'T>) x)
// // let y = x<int>
// let y = x<Test>
// let y = x<string>

// type System.String with
//   static member codec = Codec.string


// module CodecsTest =
//     open CodecUtil
//
type Test =
    | Test of string

    // static member codec: Codec<Test> = Codec.string |> Codec.map (Test) (fun (Test x) -> x)
    // static member codec(_: Test, _: YesNoTypeStaticClass.Codecs) : Codec<Test> =

    // static member get_Zero() = Test "a"
    // static member get_Codecs() : Codec<Test> =
    //   Codec.string |> Codec.map (Test) (fun (Test x) -> x)
    // static member val codec: Codec<Test> = Codec.string |> Codec.map (Test) (fun (Test x) -> x)
    static member codecs: Codec<Test> = { encoder = Encoder; decoder = Decoder }
// Codec.string |> Codec.map (Test) (fun (Test x) -> x)
//
//     // let x: Test = Zero.Invoke() // let _: Codec<int> = YesNoTypeStaticClass.codec
//     let _: Codec<string> = Codecs.Invoke() //Codecs.getCodec ()
//     // let _: Codec<Test> = Codecs.Invoke()
//
//     // let inline encode< ^T when (Codecs or ^T): (static member Codecs: ^T * Codecs -> Codec< ^T >)> (value: ^T) =
//     //     let codec: Codec<'T> = Codecs.Invoke()
//     //     let encoded = codec.Encoder value
//     //     Encode.toString 2 encoded
//     //
//     // let inline decode<'T when (Codecs or ^T): (static member Codecs: ^T * Codecs -> Codec< ^T >)> (value: string) : 'T =
//     //     // let codec: Codec<'T> = Codecs.getCodec ()
//     //     let codec = Codecs.Invoke()
//     //
//     //     Decode.fromString codec.Decoder value
//     //     |> Result.defaultWith (fun e -> failwith (e.ToString()))
//     //
//     // let inline roundTrip< ^T
//     //     when ^T: equality
//     //     and
//     //     // and (^T): (static member codecs: Codec< ^T >)
//     //     (Codecs or ^T): (static member Codecs: ^T * Codecs -> Codec< ^T >)>
//     //     (value: ^T)
//     //     =
//     //     let encoded = encode value
//     //     let decoded = decode encoded
//     //     Expect.equal value decoded "roundTrip"
//     //     Expect.isTrue true "a"
//
//     // let inline get () =
//     //   let x = 1
//     //   x
//
//     // let inline x () =
//     //   let a = Codecs.Invoke()
//     //   let b = Codecs.Invoke()
//     //   (a, b)
//
let inline x2<'a, 'b
    when (Codecs or ^a): (static member Codecs: ^a * Codecs -> Codec< ^a >)
    and (Codecs or ^b): (static member Codecs: ^b * Codecs -> Codec< ^b >)>
    ()
    =
    let a: Codec<'a> = Codecs.Invoke()
    let b: Codec<'b> = Codecs.Invoke()
    (a, b)
//
//     // type Id<'X when HasCodec<'X>> =
//     //   | Id of 'X
//     //
//     //   static member inline codec: Codec<Id<'X>> = 'X.codec |> Codec.map Id (fun (Id i) -> i)
//
let z () =
    let y: (Codec<ApiResult<Test, Test>> * Codec<ApiResult<Test, Test>>) =
        //let y: (Codec<ApiResult.ApiResult<Test, Test>> * Codec<ApiResult.ApiResult<Test, Test>>) =
        x2 ()

    ()
//
//     let tests =
//         [ test "x2" {
//               z ()
//               // let y: (Codec<ApiResult.ApiResult<Test, Test>> * Codec<ApiResult.ApiResult<Test, Test>>) =
//               //   x2 ()
//               ()
//           }
//           // test "json" {
//           //   let z: (Codec<ApiResult<ApiSuccess, ApiError>> * Codec<ApiResult<ApiSuccess, ApiError>>) =
//           //     x ()
//           //   ()
//           // // let _: Codec<ApiResult<ApiSuccess, ApiError>> = Codecs.getCodec ()
//           // // let _: Codec<ApiResult<ApiSuccess, ApiError>> = Codecs.getCodec ()
//           // // let a: Codec<ApiResult<ApiSuccess, ApiError>> = get ()
//           // // let b: Codec<ApiResult<ApiSuccess, ApiError>> = get ()
//           // }
//           // test "roundtrip - int" { roundTrip 1 }
//           // test "roundtrip - string" { roundTrip "a" }
//           // test "roundtrip - bool" { roundTrip true }
//           // test "roundtrip - float" { roundTrip 1.1 }
//           // test "roundtrip - custom 1" { roundTrip (Test "a") }
//           // test "roundtrip - custom 2" { roundTrip { error = "User already exists" } }
//           test "roundtrip - custom 3" {
//               roundTrip (
//                   if true then
//                       Http.ApiResult.Success { success = true }
//                   else
//                       Http.ApiResult.Error { error = "User already exists" }
//               )
//           } ]

// [<Erase>]
// type UserId = UserId of string
//
// type Params = {| x: UserId option |}
//
// let inline encode<'T> value =
//     let encoder = Thoth.Json.Encode.Auto.generateEncoder<'T> ()
//     let encoded = encoder value
//     Thoth.Json.Encode.toString 2 encoded
//
// let inline decode<'T> (value: string) : 'T =
//     let decoder = Thoth.Json.Decode.Auto.generateDecoder<'T> ()
//
//     Thoth.Json.Decode.fromString decoder value
//     |> Result.defaultWith (fun e -> failwith (e.ToString()))
//
// let inline roundTrip<'T when 'T: equality> (value: 'T) =
//     let encoded = encode value
//     let decoded = decode encoded
//     Expect.equal value decoded "roundTrip"
//
// let tests =
//     testList
//         "Serialization"
//         [ yield! CodecsTest.tests
//           test "parses new type" {
//               let json = encode {| x = UserId "a" |}
//
//               Expect.equal
//                   json
//                   """{
//   "x": "a"
// }"""
//                   "json"
//           }
//
//           test "round trip - erased union" {
//               let json = {| x = UserId "b" |}
//               roundTrip json
//           }
//
//           test "has parse error " {
//               let decoder = Auth.User.codec
//               let value = {| x = "b" |}
//               let result = JsonUtil.decode decoder value
//
//               Expect.equal
//                   result
//                   (Error(
//                       """Error at: ``
// Expecting an object with a field named `id` but instead got:
// {
//     "x": "b"
// }"""
//                   ))
//                   //           """Error at: `$.settings`
//                   // Expecting an object but instead got: undefined""")
//                   "json"
//           }
//
//
//           ]
