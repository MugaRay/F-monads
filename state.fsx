type State<'env,'out> = State of ('env -> 'env * 'out)

let ret (v : 'a) : State<'env,'a> = State (fun _ -> (Map.empty, v))

let fmap (f : 'a -> 'b) (v : State<'env,'a>) : State<'env,'b> =
  let (State v') = v
  State (fun env ->
    let (env', out) = v' env
    env', f out
  )

let apply (f : State<'env,'a -> 'b>) (v : State<'env,'a>) : State<'env,'b> =
  let (State f') = f
  let (State v') = v
  State (fun env ->
    let (env', func) = f' env
    let (env'', out) = v' env'
    env'', func out
  )

let bind (v : State<'env,'a>) (f : 'a -> State<'env,'b>) : State<'env,'b> =
  let (State v') = v
  State (fun env ->
    let (env', out) = v' env
    let (State f') = f out
    let (env'', out') = f' env'
    Map.fold (fun state k v -> Map.add k v state) env' env'', out'
  )

let get k : State<_,_> =
  State (fun e ->
    printfn "Finding key %s in %A" k e
    e, Map.find k e
  )

let set k v =
  printfn $"Setting key {k} to {v}"
  State (fun e -> Map.add k v e, ())

let update k f =
  let (>>=) = bind
  printfn $"Updating key {k} with {f}"
  get k >>= (set k << f)

let inline inEnv (e : 'env) (f : State<'env,'a>) : 'a =
  let (State f') = f
  f' e |> snd

let inline withDefaultEnv f = inEnv Map.empty

let func0 : State<Map<string,int>,int> =
  let (<*>) = apply
  ret (+) <*> ret 3 <*> ret 5

let func1 =
  let (>>=) = bind
  get "x" >>= fun v -> ret ((+) 9 v)

let test2 =
  let (>>=) = bind
  get "x"
  >>= fun v -> set "frodo" v
  >>= (fun () -> func1)
  >>= fun g -> set "bilbo" g

type StateBuilder () =
  member __.Bind (v, f) = bind v f
  member __.Return v = ret v

let state = new StateBuilder ()

let builderFunc =
  state {
    let! z = get "x"
    do! set "mcduck" 1000000
    let! v = get "mcduck"
    let p = v + z
    do! update "mcduck" (fun v -> v / 2)
    let! q = get "mcduck"
    return (p, q, v)
  }