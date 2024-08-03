module Test


type X = X
    with

        // a newline is added boave this line on each frmat if the union does not begin with a pipe
        static member x = 1

type Y =
    | Y

    static member x = 1
