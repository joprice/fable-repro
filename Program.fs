open Fable.Core
open JsInterop

let () =
    promise {
        let! x = importValueDynamic Client.x
        let x = x (1)
        printfn $"{x}"

        // Fails at compile time with
        //  ./Program.fs(10,17): (10,44) error FABLE: The imported value is not coming from a different file
        let! y = importValueDynamic Client.y
        let y = y (1, "a")
        printfn $"{y}"

        // Fails at runtime with
        // TypeError: z(...) is not a function
        let! z = importValueDynamic Client.z
        let z = z 1 "a"
        printfn $"{z}"
    }
    |> ignore
