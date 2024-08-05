(*

a) I do not think i need to make chan ges to the Orginal Reader Monad. It can still take in 1 enviroment on the surface but i'll just have to 
   implement helper functions to give the illusion that it actually takes two different enivroments. Reader might be generic and flexible enough to 
   do this

b) The main helper function i need to modify is isEnv because it is the one that takes in the enviroment and executes it  

*)

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


let inEnv e f =
  printfn "Executing function within environment %A" e
  let (Reader f') = f in
  f' e



let ImprovedEnv (old_env: 'a -> Reader<'a, 'b> -> 'b) env f =
    let (<*>) = apply
    let (Reader f') = f in
    f' env <*> ret old_env
