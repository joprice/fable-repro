module Program

type Codec<'a> = | Codec

type Codecs =
    // at least two overloads are required here to avoid ambiguity
    static member inline Codecs(_output: int, _mthd: Codecs) : Codec<int> = Codec
    static member inline Codecs(_output: bool, _mthd: Codecs) : Codec<bool> = Codec


    static member inline Invoke() =
        let inline call_2 (a: ^a, b: ^b) =
            ((^a or ^b): (static member Codecs: _ * _ -> _) b, a)

        let inline call (a: 'a) =
            call_2 (a, Unchecked.defaultof<'r>): Codec<'r>

        call Unchecked.defaultof<Codecs>

type Codecs with
    static member inline Codecs< ^a, ^b
        when (^a): (static member codecInner: Codec< ^b > -> Codec< ^a >)
        and (Codecs or ^b): (static member Codecs: ^b * Codecs -> Codec< ^b >)>
        (_output: ^a, _mthd: Codecs)
        : Codec< ^a > =
        // let x = Codecs.Invoke()
        // this line gets inlined without scope handling (Identifier 'x_1' has already been declared)
        let x = 1
        'a.codecInner (Codecs.Invoke())

let inline getCodec<'a when (Codecs or ^a): (static member Codecs: ^a * Codecs -> Codec< ^a >)> () : Codec< ^a > =
    let a: Codec<'a> = Codecs.Invoke()
    a

let inline getCodecs<'a, 'b
    when (Codecs or ^a): (static member Codecs: ^a * Codecs -> Codec< ^a >)
    and (Codecs or ^b): (static member Codecs: ^b * Codecs -> Codec< ^b >)>
    ()
    : Codec< ^a > * Codec< ^b > =
    let a: Codec<'a> = Codecs.Invoke()
    let b: Codec<'b> = Codecs.Invoke()
    (a, b)

type Container<'T> =
    | Container

    static member codecInner<'T, 'E>(t: Codec<'T>) : Codec<Container<'T>> =
        let y = 1
        Codec

// single container works
let _: Codec<Container<int>> = getCodec ()

// pais works
let _: (Codec<int> * Codec<int>) = getCodecs ()

// pair of containers don't generates duplciate code in debug mode
// SyntaxError: Identifier 'x_1' has already been declared
let _: (Codec<Container<int>> * Codec<Container<int>>) = getCodecs ()
