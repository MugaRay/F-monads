type FailureM<'a> =
  | Ok of 'a
  | Errors of string list
with
  // we have to call it 'ret', because 'pure' is a reserved word in F#
  static member ret v = Ok v
  static member (<*>) (f : FailureM<'a -> 'b>, v : FailureM<'a>) : FailureM<'b> =
    match f, v with
    | Errors s0, Errors s1 -> Errors (s0 @ s1)
    | Errors s0, _ -> Errors s0
    | _, Errors s1 -> Errors s1
    | Ok f, Ok v -> FailureM<'b>.ret (f v)
  static member (>>=) (v : FailureM<'a>, f : 'a -> FailureM<'b>) : FailureM<'b> =
    match v with
    | Errors sx -> Errors sx
    | Ok v -> f v
  static member fmap (f : 'a -> 'b) (v : FailureM<'a>) : FailureM<'b> =
    match v with
    | Errors s -> Errors s
    | Ok v -> FailureM<'b>.ret (f v)

let ret = FailureM<_>.ret

// Now let's do some testing.

open System.IO

let file_exists path =
  printfn $"Checking that {path} exists"
  if File.Exists path then Ok path else Errors ["File does not exist"]

let file_can_read (path : string) =
  printfn $"Checking that {path} is readable"
  if int (File.GetUnixFileMode path) &&& int UnixFileMode.OtherRead = 0 then
    Errors ["File is not readable"]
  else
    Ok path

let file_contains_moo (path : string) =
  printfn $"Checking that {path} contains 'moo'"
  if (File.ReadAllText path).Contains "moo" then
    Ok path
  else
    Errors ["File does not contain the essential string 'moo'"]

let test0 () =
  ret "test.txt" >>= file_exists >>= file_can_read >>= file_contains_moo

let test1 () =
  // here I put things in a weird order
  ret "test.txt" >>= file_contains_moo >>= file_exists >>= file_can_read

let test2 () =
  // let me make things very explicit.
  let bind (x : FailureM<'a>) (y : 'a -> FailureM<'a>) = x >>= y
  bind (
    bind (
      bind (ret "test.txt") file_exists
    ) file_can_read
  ) file_contains_moo

let dependentValidations path =
  ret path >>= file_exists >>= file_can_read >>= file_contains_moo

let test3 () =
  // whereas if one uses apply…!!
  let (<*>) a b = FailureM.(<*>)(a, b) // F# type inference is FAR worse than in Haskell.
  (ret (fun a b c -> (a, b, c)))
    <*> (file_exists "test.txt")
    <*> (file_can_read "test.txt")
    <*> (file_contains_moo "test.txt")
