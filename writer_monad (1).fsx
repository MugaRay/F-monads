// this one "writes" to a list -- though, really, it could write to anything.
// tuple ('a, 'out)
type Writer<'a,'out> = Writer of 'a * 'out list


// it takes in 'a and wraps it in the context of a Writer of tuple(a, []) and empty list (creates a writer from value 'a)
let ret v = Writer (v, [])


// just like the normal map (take a function that transforms 'a -> 'b, and just applies it to 'a and leaves 'out alone)
let fmap (f : 'a -> 'b) (v : Writer<'a,'out>) : Writer<'b,'out> =
  let (Writer (v_a, v_out)) = v
  Writer (f v_a, v_out)


// takes a function that is within a writer
// applies the function to the second args <Writer 'a>
// 'f_out and 'v_out are combined to make a new list 
let apply (f : Writer<'a -> 'b, 'out>) (v : Writer<'a,'out>) : Writer<'b,'out> =
  let (Writer (f', f_out)) = f
  let (Writer (v_a, v_out)) = v
  printfn $"Applying: {f'} {v}"
  Writer (f' v_a, f_out @ v_out)



// Unwraps the value 'a in v, uses the function f on that value
// re-wraps the value into a new writer, with a concatinated list
let bind (v : Writer<'a,'out>) (f : 'a -> Writer<'b,'out>) : Writer<'b,'out> =
  let (Writer (v_a, v_out)) = v
  let (Writer (b, b_out)) = f v_a  
  Writer (b, v_out @ b_out)


// creates a new writer (ret creates an empty list)
let log s v = Writer (v, [s])

// just a copy of ret 
let logNothing v = Writer (v, [])

let logPlus a b =
  a + b |> log $"Adding {a} and {b}"
let logMul a b =
  a + b |> log $"Multiplying {a} and {b}"

let silentPlus a b =
  a + b |> logNothing

let test0 () =
  let (>>=) = bind
  logPlus 3 8 >>= logMul 3

let test1 () : Writer<int,string> =
  let (<*>) = apply
  ret (+) <*> ret 4 <*> ret 9

let test2 () =
  let (<*>) = apply
  ret (+) <*> logPlus 4 7 <*> logMul 9 2

let test3 () : Writer<int, string> =
  let (>>=) = bind
  silentPlus 2 9 >>= logPlus 4 >>= logMul 9

type WriterBuilder () =
  member __.Bind (v, f) = bind v f
  member __.Return v = ret v

let writer = new WriterBuilder ()

let builderTest () =
  writer {
    let! a = test0 ()
    let! b = test1 ()
    return (a, b, a+b)
  }