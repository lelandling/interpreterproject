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

let reserved =
  [ "Push"
  ; "True"
  ; "False"
  ; "Pop"
  ; "Add"
  ; "Sub"
  ; "Mul"
  ; "Div"
  ; "Equal"
  ; "Lte"
  ; "And"
  ; "Or"
  ; "Not"
  ; "Trace"
  ; "Local"
  ; "Global"
  ; "Lookup"
  ; "Begin"
  ; "If"
  ; "Else"
  ; "Fun"
  ; "End"
  ; "Call"
  ; "Try"
  ; "Switch"
  ; "Case"
  ]

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
  | Call

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
  | Fun of expr * expr * expr list
  | Clo of expr * expr * expr list * ((expr * expr) list)
  | Try of expr list
  | Switch of (expr * expr list) list
  (* | Clo env name name coms *)

(* . | And | Or | Not
   | Equal
   | Lte
   | Local
   | Global
   | Lookup
   | Begin coms End
   | If coms Else coms End *)

(* end of parser combinators *)

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


let pname =
  let* c = satisfy is_alpha in
  let* cs = many (satisfy (fun c -> is_alphanum c || c = '_' || c = '\'')) in
  let s = implode (c :: cs) in
  pure(Name s) << ws

let swhitespace =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some (Var "", ls)
    else
      None
  | _ -> None



(* current list acts as stack, l[0] is top of stack *)

let pcmd = 
  (keyword "Push" >| Push) <|> (keyword "Pop" >| Pop) <|> (keyword "Trace" >| Trace) <|>
  (keyword "Mul" >| Mul) <|> (keyword "Div" >| Div) <|>
  (keyword "Add" >| Add) <|> (keyword "Sub" >| Sub)

let pspecial = 
  (keyword "And" >| (Sc And)) <|> (keyword "Not" >| (Sc Not)) <|> (keyword "Or" >| (Sc Or)) <|>
  (keyword "Equal" >| (Sc Equal)) <|> (keyword "Lte" >| (Sc Lte)) <|> (keyword "Local" >| (Sc Local)) <|> 
  (keyword "Global" >| (Sc Global)) <|> (keyword "Lookup" >| (Sc Lookup)) <|> (keyword "Call" >| (Sc Call))

(* need to figure out how to link everything to be mutually recursive *)

let popr =
  let* cmd = ws >> pcmd in 
  let* x = pempty <|> ptrue <|> pfalse <|> pint <|> pname in
  pure(Opr (cmd, x)) << ws 

let fil x = List.filter (fun x -> x <> (Var "")) x

let rec cmd_parser () =              
  popr <|> pspecial <|> swhitespace

and pif () =
  let* _ = ws >> keyword "If" in
  let* i = ws >> finalparser () in 
  let* _ = ws >> keyword "Else" in
  let* e = ws >> finalparser () in 
  let* _ = ws >> keyword "End" in
  pure(If (fil i, fil e)) << ws

and pbegin () =
  let* _ = ws >> keyword "Begin" in
  let* content = ws >> finalparser () in
  let* _ = ws >> keyword "End" in
  pure(Begin (fil content)) << ws

and pfun () = 
  let* _ = keyword "Fun" in
  let* fname = ws >> pname in
  let* arg = ws >> pname in
  let* cmds = finalparser () in
  let* _ = ws >> keyword "End" in
  pure(Fun (fname , arg, cmds)) << ws

and ptry () = 
  let* _ = keyword "Try" in 
  let* cmds = finalparser () in
  let* _ = ws >> keyword "End" in
  pure(Try cmds)

and pcase () = 
  let* _ = keyword "Case" in
  let* n = ws >> pint in 
  let* cmds = finalparser () in
  pure((n, fil cmds))

and pswitch () = 
  let* _ = keyword "Switch" in
  let* cases = ws >> many (ws >> pcase ()) in
  let* _ = ws >> keyword "End" in
  pure(Switch cases)

and finalparser () = (many (cmd_parser () <|> pif () <|> pbegin () <|> pfun () <|> ptry () <|> pswitch ()))

let masterparser = finalparser ()

let sfun () = 
  let* _ = keyword "Fun" in
  let* fname = ws >> pname in
  let* arg = ws >> pname in
  let* cmds = finalparser () in 
  let* _ = ws >> keyword "End" in 
  pure((fname, cmds, arg)) << ws

let push stack value = 
  [value] @ stack


let add stack i =
  let rec aux stack i sum =
    match i > 0 with
    | true -> (match stack with 
        | (Int x)::t -> aux t (i-1) (sum+x)
        | _ -> (stack, ["Error"]))
    | false -> (((Int sum) :: stack), []) in
  match i with 0 -> (((Int 0) :: stack), []) | _ -> aux stack i 0

let sub stack i =
  let rec aux stack i sum =
    match i > 0 with
    | true -> (match stack with 
        | (Int x)::t -> aux t (i-1) (sum-x)
        | _ -> (stack, ["Error"]))
    | false -> (((Int sum) :: stack), []) in
  match i with 0 -> (((Int 0) :: stack), []) 
             | _ -> match stack with (Int x)::t -> aux t (i-1) x
                                   | _ -> (stack, ["Error"])

let mul stack i =
  let rec aux stack i sum =
    match i > 0 with
    | true -> (match stack with 
        | (Int x)::t -> aux t (i-1) (sum*x)
        | _ -> (stack, ["Error"]))
    | false -> (((Int sum) :: stack), []) in
  match i with 0 -> (((Int 1) :: stack), []) | _ -> aux stack i 1

let div stack i =
  let rec aux stack i sum =
    match i > 0 with
    | true -> (match stack with 
        | (Int x)::t -> if x = 0 then (stack, ["Error"]) else aux t (i-1) (sum/x)
        | _ -> (stack, ["Error"]))
    | false ->(((Int sum) :: stack), []) in
  match i with 0 -> (((Int 1) :: stack), []) 
             | _ -> match stack with (Int x)::t -> aux t (i-1) x
                                   | _ -> (stack, ["Error"])

let trace stack i =
  let rec aux ls i acc = 
    match i > 0 with
    | true -> (match ls with 
        | [] -> (ls, [Var "Error"])
        | h::t -> aux t (i-1) (h :: acc))
    | false -> (ls, acc) in
  match i with
  | 0 -> (stack, [])
  | _ -> if i < 0 then (stack, [Var "Error"]) else (aux stack i [])

let pand stack =
  match stack with 
  | h1::h2::t -> (match h1, h2, t with
      | Var "True", Var "True", _ -> (((Var "True") :: t), [])
      | Var "False", Var "True", _ -> (((Var "False") :: t), [])
      | Var "True", Var "False", _ -> (((Var "False") :: t), [])
      | Var "False", Var "False", _ -> (((Var "False") :: t), [])
      | _ -> (t, [Var "Error"]))
  | _ -> (stack, [Var "Error"])   

let por stack =
  match stack with 
  | h1::h2::t -> (match h1, h2, t with
      | Var "True", Var "True", _ -> (((Var "True") :: t), [])
      | Var "False", Var "True", _ -> (((Var "True") :: t), [])
      | Var "True", Var "False", _ -> (((Var "True") :: t), [])
      | Var "False", Var "False", _ -> (((Var "False") :: t), [])
      | _ -> (t, [Var "Error"]))
  | _ -> (stack, [Var "Error"]) 

let pnot stack =
  match stack with
  | h::t -> (match h with 
      | Var "True" -> (((Var "False") :: t), [])
      | Var "False" -> (((Var "True") :: t), [])
      | _ -> (t, [Var "Error"]))
  | _ -> stack, [Var "Error"]

let lint f stack = 
  match stack with 
  | h1::h2::t -> (match h1, h2, t with
      | Int i, Int j, _ -> if (f i j) then (((Var "True") :: t), []) else (((Var "False") :: t), [])
      | _ -> (t, [Var "Error"]))
  | _ -> (stack, [Var "Error"])  

let eql stack =
  lint (fun i j -> i = j) stack

let lte stack =
  lint (fun i j -> i <= j) stack

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

let checkcases (sw) (Int i) =
  let rec aux sw i = 
    match sw with 
    | (Int n, cms)::t -> if n = i then cms else (aux t i)
    | _ -> [] in
  aux sw i

let eval cmds rs os =
  let rec evalhelper cmd rs os local global = 
    let trycmds cm nrs nos local global c =
      match c with 
      | Sc And -> (match (pand nrs) with 
          | (trs, [Var "Error"]) -> ([], [Var "Error"], local, global)
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Not -> (match (pnot nrs) with           
          | (trs, [Var "Error"]) -> ([], [Var "Error"], local, global)
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Or -> (match (por nrs) with           
          | (trs, [Var "Error"]) -> ([], [Var "Error"], local, global)
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Equal -> (match (eql nrs) with           
          | (trs, [Var "Error"]) -> ([], [Var "Error"], local, global)
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Lte -> (match (lte nrs) with           
          | (trs, [Var "Error"]) -> ([], [Var "Error"], local, global)
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Sc Local -> (match (createpair nrs) with
          | (trs, [], [x]) -> evalhelper cm (push trs (Var "()")) nos (x :: (local)) (global)
          | _ -> ([], [Var "Error"], local, global))
      | Sc Global -> (match (createpair nrs) with
          | (trs, [], [x]) -> evalhelper cm (push trs (Var "()")) nos (local) (x :: (global)) 
          | _ -> ([], [Var "Error"], local, global))
      | Sc Lookup -> (match (lookup nrs local global) with
          | ( _ , Var "None") -> ([], [Var "Error"], local, global)
          | (trs, x) -> evalhelper cm (push trs (x)) nos local global)
      | Sc Call -> (match nrs with
          | (Clo (fname, arg, sub, slocal))::a::t -> (match evalhelper sub [] nos ((fname , Clo (fname, arg, sub, slocal)) :: (arg, a) :: slocal) global with 
              | (x, [Var "Error"], local, global) -> (x, [Var "Error"], local, global)
              | (h::trs, tos, nlocal, nglobal) ->  evalhelper cm (h :: t) tos local nglobal
              | _ -> ([] , [Var "Error"], local, global))
          | _ -> ([] , [Var "Error"], local, global))
      | Opr (Trace, Int x) -> (match (trace nrs x) with
          | (trs, [Var "Error"]) -> ([], [Var "Error"], local, global)
          | (trs, tos) -> evalhelper cm trs (tos @ nos) local global)
      | Opr (Pop, Int x) -> (match (trace nrs x) with
          | (trs, [Var "Error"]) -> ([], [Var "Error"], local, global)
          | (trs, tos) -> evalhelper cm trs nos local global)
      | Opr (Add, Int x) -> (match (add nrs x) with
          | (trs, []) -> evalhelper cm trs nos local global
          | _ -> ([], [Var "Error"], local, global))
      | Opr (Sub, Int x) -> (match (sub nrs x) with
          | (trs, []) -> evalhelper cm trs nos local global
          | _ -> ([], [Var "Error"], local, global))
      | Opr (Mul, Int x) -> (match (mul nrs x) with
          | (trs, []) -> evalhelper cm trs nos local global
          | _ -> ([], [Var "Error"], local, global))
      | Opr (Div, Int x) -> (match (div nrs x) with
          | (trs, []) -> evalhelper cm trs nos local global
          | _ -> ([], [Var "Error"], local, global))
      | Opr (Push, x) -> evalhelper cm (push nrs (x)) (nos) local global
      | If (i, e) -> (match checkif nrs with
          | (_, trs,[Var "Error"]) -> ([], [Var "Error"], local, global)
          | (true, trs, _) -> evalhelper (i @ cm) (trs) (nos) local global
          | (false, trs, _) -> evalhelper (e @ cm) (trs) (nos) local global)
      | Begin (substack) -> (match evalhelper substack [] os local global with
          | (_, [Var "Error"], _, _) ->  ([], [Var "Error"], local, global)
          | (rs, nos, nlocal, nglobal) -> (match rs with 
              | [] -> ([], [Var "Error"], nlocal, nglobal)
              | h::t -> evalhelper cm (h :: nrs) (nos) nlocal nglobal))
      | Fun (fname, arg, sub) -> evalhelper cm rs nos (((fname, Clo (fname, arg, sub, local))) :: local) global
      | Try (cmds) -> (match evalhelper (cmds) nrs (nos) local global with 
          | ([], [Var "Error"], nlocal, nglobal) -> evalhelper cm nrs (nos) local nglobal
          (* | ([], os, nlocal, nglobal) -> ([], [Var "Error here"], local, global) *)
          | (trs, nos, nlocal, nglobal) -> (match trs with 
              | [] -> ([], [Var "Error"], nlocal, nglobal)
              | h::t -> evalhelper cm (h :: nrs) (nos) nlocal nglobal))
      (* evalhelper cm ([h] @ nrs) (nos) nlocal nglobal)) *)
      | Switch (cmds) -> (match nrs with 
          | h::t -> (match checkcases cmds h with 
              | [] -> ([], [Var "Error"], local, global)
              | case -> evalhelper (case @ cm) t (nos) local global)
          | _ ->  ([], [Var "Error"], local, global))
      | _ -> ([], [Var "Error"], local, global) in
    match cmd with
    | [] -> (rs, os, local, global)
    | h::t -> match h with 
      | Sc s -> trycmds t rs os local global (Sc s)
      | Opr (c, v) -> trycmds t rs os local global (Opr (c, v))
      | If (i, e) -> trycmds t rs os local global (If (i,e))
      | Begin (substack) -> trycmds t rs os local global (Begin substack)
      | Fun (fname, arg, sub) -> trycmds t rs os local global (Fun (fname, arg, sub))
      | Try (cmds) -> trycmds t rs os local global (Try cmds)
      | Switch (cmds) -> trycmds t rs os local global (Switch cmds)
      | _ -> ([], [Var "Error"], [], []) in 
  evalhelper cmds rs os [] []



let exprls2strls exprls = 
  let rec aux exprls acc =
    match exprls with
    | [] -> acc
    | h::t -> match h with 
      | Int x -> aux t ((string_of_int x) :: acc)
      | Var x -> aux t ((x) :: acc)
      | Name x -> aux t ((x) :: acc)
      | _ -> acc in
  rev(aux exprls [])

let accessleft stack =
  match stack with
  | None -> []
  | Some (x, y) -> x


let accessright stack =
  match stack with 
  | None -> []
  | Some (x,y)-> y

let accessos stack =
  match stack with (w,x,y,z) -> x

let accesscm filename =
  List.filter (fun x -> x <> (Var "")) (accessleft(parse (masterparser) (readlines filename)))

let interp (src : string) : string list = 
  let opt = (parse (masterparser) src) in
  match opt with
  | Some (e, []) -> (let cmds = (List.filter (fun x -> x <> (Var "")) e) in exprls2strls (accessos (eval cmds [] [])))
  | _ -> ["Error"]

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src
