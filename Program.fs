module rec SerializationTests

type Codec<'a> = | Codec

type Codecs =
    // at least two overloads are required here to avoid ambiguity
    static member Codecs(_output: int, _mthd: Codecs) : Codec<int> = Codec
    static member Codecs(_output: bool, _mthd: Codecs) : Codec<bool> = Codec

    static member inline Codecs< ^a, ^b
        when (^a): (static member codecInner: Codec< ^b > -> Codec< ^a >)
        and (Codecs or ^b): (static member Codecs: ^b * Codecs -> Codec< ^b >)>
        (_output: ^a, _mthd: Codecs)
        : Codec< ^a > =
        let b: Codec< ^b > = Codecs.Invoke()
        Codec

    static member inline Invoke() =
        let inline call_2 (a: ^a, b: ^b) =
            ((^a or ^b): (static member Codecs: _ * _ -> _) b, a)

        let inline call (a: 'a) =
            call_2 (a, Unchecked.defaultof<'r>): Codec<'r>

        call Unchecked.defaultof<Codecs>

let inline getCodec<'a, 'b
    when (Codecs or ^a): (static member Codecs: ^a * Codecs -> Codec< ^a >)
    and (Codecs or ^b): (static member Codecs: ^b * Codecs -> Codec< ^b >)>
    ()
    : Codec< ^a > * Codec< ^b > =
    let a: Codec<'a> = Codecs.Invoke()
    let b: Codec<'b> = Codecs.Invoke()
    (a, b)

type Container<'T> =
    | Container

    static member codecInner<'T, 'E>(t: Codec<'T>) : Codec<Container<'T>> = Codec

let _: (Codec<Container<int>> * Codec<Container<int>>) = getCodec ()
