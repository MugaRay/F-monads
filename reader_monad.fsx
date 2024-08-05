type Reader<'env,'output> = Reader of ('env -> 'output)


// Reader type is a function that takes in a env and returns output

// if that's the case then it's identy function should just be a function that returns the inital value
let ret v =
  // the default is a function which ignores its input and returns the value given.
  Reader <| fun _ -> v



// applying f to the function v with x as the argument 
// creates a new function that has f appied to it
let fmap (f : 'a -> 'b) (v : Reader<'env,'a>) : Reader<'env,'b> =
  Reader (fun x ->
    let (Reader read) = v in
    f (read x)
  )


// f is a reader that takes a function that returns a function
// v is a normal reader 
// returns a reader with the appied f function 
let apply (f : Reader<'env,'a -> 'b>) (v : Reader<'env,'a>) : Reader<'env,'b> =
  let (Reader f') = f
  let (Reader v') = v
  Reader (fun env ->
    printfn $"Applying: {f'} {v}'"
    let previous = v' env // this gets me an 'a
    printfn $"Previous is: {previous}"
    f' env previous  // Higher order functions are taking place here. (f' env) returns a function that then uses previous
  )


// standard un-wrap, apply function , re-wrap
let bind (v : Reader<'env,'a>) (f : 'a -> Reader<'env,'b>) : Reader<'env,'b> =
  let (Reader v') = v // unwrap v to get back function 

  // reader is a function that takes env -> a
  // f is a function that takes a -> Reader<'env, b>
  // so we need to apply v to get 'a in this case 
  // Once we do tha we apply f to the output of v and recieve a new reader. which is b in this case
  Reader (fun input ->
    let (Reader b) = f (v' input)
    b input // creating the new function for reader
  )

let inline fetchList key =
  // takes in a list of tuples
  // returns a reader that returns the second element of a tuple that has a particular key
  // Reader (List<a * b> -> b) 
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

// just gives the reader monad an enviroment 
// in this case just appiles the function reader
// creates an actual output
let inEnv e f =
  printfn "Executing function within environment %A" e
  let (Reader f') = f in
  f' e

let func0 =
  let (<*>) = apply
  // ret (+) -> Reader(fun _ -> + ). creates an empty reader that returns the add function 
  // ret (3) -> same thing but returns Reader(fun _ -> 3)
  // fetchList creates a Reader in this case that looks for a tuple in a list that has the 
  // first value as v and returns that value
  // this is basically an add 3 function to any value that has the key v in a list of tuples
  // just an accumlation of different Reader's to make one big reader function
  // it's more or less just this in the end ret (+) (combine ret 3 and fetchList)
  // it also does this recurively  
  ret (+) <*> ret 3 <*> (fetchList "v")

let func1 =
  let (>>=) = bind
  ret 5 >>= fun a -> fetchMap "k" >>= fun b -> ret (a + b)

let func2 =
  let (<*>) = apply
  ret (+) <*> locally (Map.add "k" 900000) func1 <*> (fetchMap "k")

let test0 () =
  // it is a list of tuples in the form of [("addr" , 6), ('v', 90) ]
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