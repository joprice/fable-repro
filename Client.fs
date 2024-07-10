module Client

type Client =
    abstract member load: unit -> int


module Client =
    let private helper x = x + 1

    // this ends up referencing ClientModule_helper, which is not exported since it is private
    let inline direct () = helper 1

    // this ends up referencing ClientModule_helper, which is not exported since it is private
    let inline makeAnon () : Client =
        { new Client with
            member _.load() = helper 1 }

    type TestClient() =
        interface Client with
            member _.load() = helper 1

    // this works since the test client is defined locally
    let inline makeSub () : Client = TestClient()
