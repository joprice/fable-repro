open Client

let _ = Client.direct ()

let anon = Client.makeAnon ()
let _ = anon.load ()

let sub = Client.makeSub ()
let _ = sub.load ()
