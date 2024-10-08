module Client

let x (a: int) = 1
let y (a: int, b: string) = 2
let z (a: int) (b: string) = 3

type Encoder<'a> = | Encoder
type Decoder<'a> = | Decoder

type Codec<'a> =
    { encoder: Encoder<'a>
      decoder: Decoder<'a> }

type Codecs =
    static member Codecs(_output: int, _mthd: Codecs) : Codec<int> =
        { encoder = Encoder; decoder = Decoder }

    static member Codecs(_output: bool, _mthd: Codecs) : Codec<bool> =
        { encoder = Encoder; decoder = Decoder }

    static member inline Codecs< ^a when (^a): (static member codec: Codec< ^a >)>
        (_output: ^a, _mthd: Codecs)
        : Codec< ^a > =
        'a.codec

    static member inline Invoke() =
        let inline call_2 (a: ^a, b: ^b) =
            ((^a or ^b): (static member Codecs: _ * _ -> _) b, a)

        let inline call (a: 'a) =
            call_2 (a, Unchecked.defaultof<'r>): Codec<'r>

        call Unchecked.defaultof<Codecs>

type HasCodec<'T when 'T: (static member codec: Codec<'T>)> = 'T

type X<'A, 'B when HasCodec<'A> and HasCodec<'B>> =
    | A of 'A

    static member inline Codec2<'A, 'B> (a: Codec<'A>) (b: Codec<'B>) : Codec<X<'A, 'B>> =
        { encoder = Encoder; decoder = Decoder }

    static member inline codec: Codec<X< ^A, ^B >> = X.Codec2 'A.codec 'B.codec

module ApiResult =
    [<RequireQualifiedAccess>]
    type ApiResult<'T, 'E when HasCodec<'E> and HasCodec<'T>> =
        | Success of 'T
        | Error of 'E

        static member inline codecInner<'T, 'E> (t: Codec<'T>) (e: Codec<'E>) : Codec<ApiResult<'T, 'E>> =
            let encoder = t.encoder
            let errorEncoder = e.encoder
            { encoder = Encoder; decoder = Decoder }

        static member inline codec: Codec<ApiResult< ^T, ^E >> =
            ApiResult.codecInner<'T, 'E> 'T.codec 'E.codec
