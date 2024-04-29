(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in
  loop cs ls

let keyword (s : string) : unit parser = literal s >> ws >| ()


type opr =
  | Push
  | Trace
  | Pop
  | Add
  | Sub
  | Mul
  | Div

type nointcmd = 
  | And 
  | Or 
  | Not
  | Equal
  | Lte
  | Local
  | Global
  | Lookup



type expr = 
  | Int of int
  | Var of string
  | Name of string
  | Letter of string
  | Cons of string
  | Opr of opr * expr
  | If of expr list * expr list
  | Sc of nointcmd
  | Begin of expr list

(* . | And | Or | Not
   | Equal
   | Lte
   | Local
   | Global
   | Lookup
   | Begin coms End
   | If coms Else coms End *)

(* end of parser combinators *)

let reconstruct f = 
  match f with 
  | Some (x, y) -> Some (List.filter (fun x -> x <> (Var "")) x, y)
  | None -> None

let accessleft stack =
  match stack with
  | None -> []
  | Some (x, y) -> x

let removevals stack x =
  let rec rev_append acc ls x = 
    match ls with
    | [] -> acc
    | h::t -> if x > 0 then rev_append acc t (x-1) else rev_append (h::acc) t 0 in
  rev_append [] stack x

let rev stack = 
  let rec rev_append acc ls = 
    match ls with
    | [] -> acc
    | h::t -> rev_append (h::acc) t in
  rev_append [] stack

let pint =
  (natural >>= fun n -> pure(Int n))<|> 
  (
    char '-' >>= fun _ -> 
    natural >>= fun a -> pure(Int (-a))
  )

let pempty = 
  keyword "()" >>= fun l ->
  pure(Var "()")

let ptrue =
  keyword "True" >>= fun l ->
  pure(Var "True")

let pfalse =
  keyword "False" >>= fun l ->
  pure(Var "False")

(* pname needs work doesn't parse anything other than alpha *)
let pname = 
  many (satisfy is_alpha) >>= fun x ->
  pure(Name (implode x))

let swhitespace =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some (Var "", ls)
    else
      None
  | _ -> None


(* need some master parser, 
   run through pushes until some opr is reached, do the opr in question, keep going until eos*)


(* parse (many (p2<|>p)) src;; *)
(* "Push ()\nPush False\nPush 5\nPush 8\nPush 20\nPop 2\nPop 1\nTrace 2\nPush 10\nPush True\nPush ()\nPush ()\nTrace 2\n" *)
(* TODO *)
(* current list acts as stack, l[0] is top of stack *)

let accessright stack =
  match stack with 
  | None -> []
  | Some (x,y)-> y


let pcmd = 
  (keyword "Push" >| Push) <|> (keyword "Pop" >| Pop) <|> (keyword "Trace" >| Trace) <|>
  (keyword "Mul" >| Mul) <|> (keyword "Div" >| Div) <|>
  (keyword "Add" >| Add) <|> (keyword "Sub" >| Sub)

let pspecial = 
  (keyword "And" >| (Sc And)) <|> (keyword "Not" >| (Sc Not)) <|> (keyword "Or" >| (Sc Or)) <|>
  (keyword "Equal" >| (Sc Equal)) <|> (keyword "Lte" >| (Sc Lte)) <|>
  (keyword "Local" >| (Sc Local)) <|> (keyword "Global" >| (Sc Global)) <|> (keyword "Lookup" >| (Sc Lookup))

(* need to figure out how to link everything to be mutually recursive *)

let cmd_parser =              
  let* cmd = pcmd in 
  let* x = pempty <|> ptrue <|> pfalse <|> pint <|> pname in
  pure(Opr (cmd, x))

let pif =
  let* _ = keyword "If" in
  let* _ = ws in
  let* i = many (cmd_parser <|> swhitespace) in 
  let* _ = ws in
  let* _ = keyword "Else" in
  let* e = many (cmd_parser <|> swhitespace) in 
  let* _ = ws in
  let* _ = keyword "End" in
  pure(If (List.filter (fun x -> x <> (Var "")) i, List.filter (fun x -> x <> (Var "")) e))


let rec cparser = 
  (cmd_parser <|> pspecial <|> swhitespace <|> pif)

let pbegin =
  let* _ = keyword "Begin" in
  let* content = many cparser  in
  let* _ = keyword "End" in
  pure(Begin (List.filter (fun x -> x <> (Var "")) content))

let finalparser =   (many (cmd_parser <|> pspecial <|> swhitespace <|> pif <|> pbegin))

(* problem with pbegin is that it cannot end yet parse (many (cmd_parser <|> pspecial <|> swhitespace)*)
(* let pbegin = 
   (keyword "Begin") >>= fun x -> 
   (* (many (pspecial <|> cmd_parser <|> swhitespace)) >>= fun s -> *)
   pure(Opr (Begin)) *)

let rmvws ls = 
  let rec aux ls acc= 
    match ls with
    | h::t -> (match h with (Var "") -> aux t acc | x -> aux t (t::acc))
    | [] -> acc in
  aux ls []

let push stack value = 
  [value] @ stack


let add stack i =
  let rec aux stack i sum =
    match i > 0 with
    | true -> (match stack with 
        | (Int x)::t -> aux t (i-1) (sum+x)
        | _ -> (stack, ["Error"]))
    | false -> (([Int sum] @ stack), []) in
  match i with 0 -> (([Int 0] @ stack), []) | _ -> aux stack i 0

let sub stack i =
  let rec aux stack i sum =
    match i > 0 with
    | true -> (match stack with 
        | (Int x)::t -> aux t (i-1) (sum-x)
        | _ -> (stack, ["Error"]))
    | false -> (([Int sum] @ stack), []) in
  match i with 0 -> (([Int 0] @ stack), []) 
             | _ -> match stack with (Int x)::t -> aux t (i-1) x
                                   | _ -> (stack, ["Error"])

let mul stack i =
  let rec aux stack i sum =
    match i > 0 with
    | true -> (match stack with 
        | (Int x)::t -> aux t (i-1) (sum*x)
        | _ -> (stack, ["Error"]))
    | false -> (([Int sum] @ stack), []) in
  match i with 0 -> (([Int 1] @ stack), []) | _ -> aux stack i 1

let div stack i =
  let rec aux stack i sum =
    match i > 0 with
    | true -> (match stack with 
        | (Int x)::t -> if x = 0 then (stack, ["Error"]) else aux t (i-1) (sum/x)
        | _ -> (stack, ["Error"]))
    | false ->(([Int sum] @ stack), []) in
  match i with 0 -> (([Int 1] @ stack), []) 
             | _ -> match stack with (Int x)::t -> aux t (i-1) x
                                   | _ -> (stack, ["Error"])

let trace stack i =
  let rec aux ls i acc = 
    match i > 0 with
    | true -> (match ls with 
        | [] -> (ls, [Var "Error"])
        | h::t -> aux t (i-1) ([h] @ acc))
    | false -> (ls, acc) in
  match i with
  | 0 -> (stack, [])
  | _ -> if i < 0 then (stack, [Var "Error"]) else (aux stack i [])


let pand stack =
  match stack with 
  | h1::h2::t -> (match h1, h2, t with
      | Var "True", Var "True", _ -> (([Var "True"] @ t), [])
      | Var "False", Var "True", _ -> (([Var "False"] @ t), [])
      | Var "True", Var "False", _ -> (([Var "False"] @ t), [])
      | Var "False", Var "False", _ -> (([Var "False"] @ t), [])
      | _ -> (t, [Var "Error"]))
  | _ -> (stack, [Var "Error"])   

let por stack =
  match stack with 
  | h1::h2::t -> (match h1, h2, t with
      | Var "True", Var "True", _ -> (([Var "True"] @ t), [])
      | Var "False", Var "True", _ -> (([Var "True"] @ t), [])
      | Var "True", Var "False", _ -> (([Var "True"] @ t), [])
      | Var "False", Var "False", _ -> (([Var "False"] @ t), [])
      | _ -> (t, [Var "Error"]))
  | _ -> (stack, [Var "Error"]) 

let pnot stack =
  match stack with
  | h::t -> (match h with 
      | Var "True" -> (([Var "False"] @ t), [])
      | Var "False" -> (([Var "True"] @ t), [])
      | _ -> (t, [Var "Error"]))
  | _ -> stack, [Var "Error"]

let lint f stack = 
  match stack with 
  | h1::h2::t -> (match h1, h2, t with
      | Int i, Int j, _ -> if (f i j) then (([Var "True"] @ t), []) else (([Var "False"] @ t), [])
      | _ -> (t, [Var "Error"]))
  | _ -> (stack, [Var "Error"])  

let exprls2strls exprls = 
  let rec aux exprls acc =
    match exprls with
    | [] -> acc
    | h::t -> match h with 
      | Int x -> aux t ([string_of_int x] @ acc)
      | Var x -> aux t ([x] @ acc)
      | _ -> acc in
  rev(aux exprls [])

let eql stack =
  lint (fun i j -> i = j) stack

let lte stack =
  lint (fun i j -> i > j) stack

let createpair stack = 
  match stack with
  | (Name h1)::h2::t -> (t, [], [(Name h1, h2)])
  | _ -> (stack, [Var "Error"], [])

let ldict dict = 
  match dict with 
  | (x,y) -> x

let rdict dict = 
  match dict with 
  | (x,y) -> y

let lookup stack local global = 
  let rec aux (v : expr) (dict : (expr * expr) list) = 
    (match dict with
     | [] -> Var "None"
     | (x, y)::t -> if x = v then y else aux v t) in 
  match stack with 
  | (x) :: t -> (match (aux x local) with 
      | Var "None" -> (t, aux x global)
      | y -> (t, y))
  | _ -> (stack, Var "None")

let a3 stack = 
  match (createpair stack) with 
  | (_, _, t) -> t

let checkif stack = 
  match stack with 
  | (Var "True") :: t -> (true, t, [])
  | (Var "False") :: t -> (false, t, [])
  | _ -> (true, [], [Var "Error"])
(* should be something like parse (begin p end) and pure p *)

let eval cmds rs os =
  let rec evalhelper cmd rs os local global = 
    let trycmds cm nrs nos local global c =
      match c with 
      | Sc And -> (match (pand nrs) with 
          | (trs, [Var "Error"]) -> [Var "Error"]
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Not -> (match (pnot nrs) with           
          | (trs, [Var "Error"]) -> [Var "Error"]
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Or -> (match (por nrs) with           
          | (trs, [Var "Error"]) -> [Var "Error"]
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Equal -> (match (eql nrs) with           
          | (trs, [Var "Error"]) -> [Var "Error"]
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Lte -> (match (lte nrs) with           
          | (trs, [Var "Error"]) -> [Var "Error"]
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Local -> (match (createpair nrs) with
          | (trs, [], [x]) -> evalhelper cm (push trs (Var "()")) nos ([x] @ (local)) (global)
          | _ -> [Var "Error"])
      | Sc Global -> (match (createpair nrs) with
          | (trs, [], [x]) -> evalhelper cm (push trs (Var "()")) nos (local) ([x] @ (global)) 
          | _ -> [Var "Error"])
      | Sc Lookup -> (match (lookup nrs local global) with
          | ( _ , Var "None") -> [Var "Error"]
          | (trs, x) -> evalhelper cm (push trs (x)) nos local global)
      | Opr (Trace, Int x) -> (match (trace nrs x) with
          | (trs, [Var "Error"]) -> [Var "Error"]
          | (trs, tos) -> evalhelper cm trs (tos @ nos) local global)
      | Opr (Pop, Int x) -> (match (trace nrs x) with
          | (trs, [Var "Error"]) -> [Var "Error"]
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Opr (Add, Int x) -> (match (add nrs x) with
          | (trs, []) -> evalhelper cm trs nos local global
          | _ -> [Var "Error"])
      | Opr (Sub, Int x) -> (match (sub nrs x) with
          | (trs, []) -> evalhelper cm trs nos local global
          | _ -> [Var "Error"])
      | Opr (Mul, Int x) -> (match (mul nrs x) with
          | (trs, []) -> evalhelper cm trs nos local global
          | _ -> [Var "Error"])
      | Opr (Div, Int x) -> (match (div nrs x) with
          | (trs, []) -> evalhelper cm trs nos local global
          | _ -> [Var "Error"])
      | Opr (Push, x) -> evalhelper cm (push nrs (x)) (nos) local global
      | If (i, e) -> (match checkif nrs with
          | (_, trs,[Var "Error"]) -> [Var "Error if"]
          | (true, trs, _) -> evalhelper cm (i @ trs) (nos) local global
          | (false, trs, _) -> evalhelper cm (e @ trs) (nos) local global)
      | Begin (substack) -> failwith "unimplemented"
      | _ -> [Var "Error no case"] in
    match cmd with
    | [] -> os
    | h::t -> match h with 
      | Sc s -> trycmds t rs os local global (Sc s)
      | Opr (c, v) -> trycmds t rs os local global (Opr (c, v))
      | _ -> [Var "Error"] in 
  evalhelper cmds rs os [] []

let t1 s =
  match s with
  | h::t -> h
  | _ -> []

let interp (src : string) : string list = 
  let opt = (parse (many (cmd_parser <|> pspecial <|> swhitespace)) src) in
  match opt with
  | Some (e, []) -> (let cmds = (List.filter (fun x -> x <> (Var "")) e) in exprls2strls (eval cmds [] []))
  | _ -> ["Error"]

let accesscm filename =
  List.filter (fun x -> x <> (Var "")) (accessleft(parse (many (cmd_parser <|> pspecial <|> swhitespace)) (readlines filename)))

let reconstruct f = 
  match f with 
  | Some (x, y) -> Some (List.filter (fun x -> x <> (Var "")) x, y)
  | None -> None
(* 
let interp2 (src : string)= 
let opt = (parse (many (cmd_parser <|> swhitespace)) src) in
match opt with
| Some (e, []) -> (let cmds = (List.filter (fun x -> x <> (Var "")) e) in eval2 cmds [] [])
| _ -> [Var "Error"] *)
(* (rev (accessreturn (parse ((many p2) >>= ws) src))) @ runningstack *)

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src
(* 
let main2 fname =
let src = readlines fname in
interp2 src
*)


(* legacy code below *)

(* 
let checktopxints stack i =
let rec aux stack i =
match i with 
| 0 -> true
| _ -> match stack with
| [] -> false
| h::t -> match h with
| Int y -> aux t (i-1)
| _ -> false in
aux stack i *)


(* 
let sub stack i = 
let rec aux stack i sum=
match i > 0 with 
| true -> (match stack with 
| [] -> Error ("Stack too small") 
| h::t -> aux t (i-1) (sum-h))
| false -> Ok([sum] @ stack) in
match i with
| 0 -> Ok([0] @ stack)
| _ -> if i < 0 then Error ("Neg i") else (match stack with [] -> Error ("Stack too small")
  | h1::t -> aux t (i-1) h1) *)

(* 
let add stack i = 
let rec aux stack i sum=
match i > 0 with 
| true -> (match stack with 
| [] -> Error ("Stack too small") 
| h::t -> aux t (i-1) (sum+h))
| false -> Ok([sum] @ stack) in
match i with
| 0 -> Ok([1] @ stack)
| _ -> if i < 0 then Error ("Neg i") else (aux stack (i) 0) *)
(* 
let mul stack i = 
let rec aux stack i prod=
match i > 0 with 
| true -> (match stack with 
| [] -> Error ("Stack too small") 
| h::t -> aux t (i-1) (prod*h))
| false -> Ok([prod] @ stack) in
match i with
| 0 -> Ok([1] @ stack)
| _ -> if i < 0 then Error ("Neg i") else (match stack with [] -> Error ("Stack too small")
  | h1::t -> aux t (i-1) h1) *)

(* let div stack i = 
   let rec aux stack i tot=
   match i > 0 with 
   | true -> (match stack with 
   | [] -> (stack, ["Error"])
   | h::t -> aux t (i-1) (tot/h))
   | false -> if tot = 0 then (stack, ["Error"]) else ([tot] @ stack, []) in
   match i with
   | 0 -> Ok([1] @ stack)
   | _ -> if i < 0 then (stack, ["Error"]) else (match stack with [] -> Error ("Stack too small")
      | h1::t -> aux t (i-1) h1) *)
