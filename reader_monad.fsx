type Reader<'env,'output> = Reader of ('env -> 'output)


// Reader type is a function that takes in a env and returns output

// if that's the case then it's identy function should just be a function that returns the inital value
let ret v =
  // the default is a function which ignores its input and returns the value given.
  Reader <| fun _ -> v




let fmap (f : 'a -> 'b) (v : Reader<'env,'a>) : Reader<'env,'b> =
  Reader (fun x ->
    let (Reader read) = v in
    f (read x)
  )

let apply (f : Reader<'env,'a -> 'b>) (v : Reader<'env,'a>) : Reader<'env,'b> =
  let (Reader f') = f
  let (Reader v') = v
  Reader (fun env ->
    printfn $"Applying: {f'} {v}'"
    let previous = v' env // this gets me an 'a
    printfn $"Previous is: {previous}"
    f' env previous
  )

let bind (v : Reader<'env,'a>) (f : 'a -> Reader<'env,'b>) : Reader<'env,'b> =
  let (Reader v') = v
  Reader (fun input ->
    let (Reader b) = f (v' input)
    b input
  )

let inline fetchList key =
  printfn $"Fetching {key} from the configuration list"
  Reader (List.find (fun (k,_) -> k = key) >> snd)

let inline fetchMap key =
  printfn $"Fetching {key} from the configuration map"
  Reader (Map.find key)

let locally (modifier : 'env -> 'env) (v : Reader<'env,'a>) : Reader<'env,'a> =
  let (Reader v') = v
  Reader (fun input ->
    printfn "I have a locally-modified environment %A" (modifier input)
    v' (modifier input)
  )

let inEnv e f =
  printfn "Executing function within environment %A" e
  let (Reader f') = f in
  f' e

let func0 =
  let (<*>) = apply
  ret (+) <*> ret 3 <*> (fetchList "v")

let func1 =
  let (>>=) = bind
  ret 5 >>= fun a -> fetchMap "k" >>= fun b -> ret (a + b)

let func2 =
  let (<*>) = apply
  ret (+) <*> locally (Map.add "k" 900000) func1 <*> (fetchMap "k")

let test0 () =
  inEnv ["addr", 6; "v", 90] func0

let test1 () =
  inEnv (Map.ofList ["addr", 6; "k", 77]) func1

let test2 () =
  inEnv (Map.ofList ["addr", 6; "k", 2500]) func2

type ReaderBuilder () =
  member __.Bind (v, f) = bind v f
  member __.Return v = ret v

let reader = new ReaderBuilder ()

let builderFunc =
  reader {
    let! v0 = func1
    let! v1 = func2
    return (v0 - 8, v1 * 2)
  }