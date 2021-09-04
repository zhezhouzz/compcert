open Utils

type ret_tp = RetInt | RetVoid

type var = string
type funcid = string
type op =
  | Plus
  | Minus
  | Lt
  | Lit of int

let spf = Printf.sprintf

let layout_op op args = (
  match op, args with
  | Plus, [a;b] -> spf "%s + %s" a b
  | Minus, [a;b] -> spf "%s - %s" a b
  | Lt, [a;b] -> spf "%s < %s" a b
  | Lit i, [] -> spf "%i" i
  | _ -> failwith "wrong op!"
)

type statement =
  | Skip
  | Assign of var * var
  | AssignOp of var * op * var list
  | Apply of var option * funcid * var list
  | Return of var option
  | Seq of statement * statement
  | Loop of statement
  | Block of statement
  | Break
  | Ite of var * statement * statement

let layout_statement = (
  let rec aux = function
    | Skip -> "skip"
    | Assign (x, y) -> spf "%s = %s" x y
    | AssignOp(x, op, args) -> spf "%s = %s" x (layout_op op args)
    | Apply(varopt, fid, args) ->
      let app = spf "%s(%s)" fid (StrList.to_string args) in
      (match varopt with
       | None -> spf "%s" app
       | Some x -> spf "%s = %s" x app)
    | Return None -> "return"
    | Return (Some x) -> spf "return %s" x
    | Seq (s1, s2) -> spf "%s; %s" (aux s1) (aux s2)
    | Loop s -> spf "loop %s" (aux s)
    | Block s -> spf "{%s}" (aux s)
    | Break -> "break"
    | Ite (c, s1, s2) -> spf "if (%s) then %s else %s" c (aux s1) (aux s2)
  in
  aux
)

let make_seq ss =
  match List.rev ss with
  | [] -> Skip
  | last :: prev ->
    List.fold_right (fun s res -> Seq (s, res)) (List.rev prev) last

type fd = {
  name: string;
  signature: string list * ret_tp;
  vars: string list;
  body: statement}

type g = (fd StrMap.t) ref

(*
int foo (int a, int b) {
    loop {
        tmp = a < b;
        if (tmp){ break; } else {tmp = 1; a = a - tmp; b = b + tmp;}
    }
    a = a + a;
    a = a + b;
    return a;
}
int main() {
    int x, y;
    x = 0;
    y = 3;
    z = foo(y, x);
    return z;
}
*)

let default_g = StrMap.from_kv_list [
    ("main",
     {name = "main";
      signature = [], RetInt; vars = ["x"; "y"; "z"];
      body = make_seq [AssignOp("x", Lit 0, []); AssignOp("y", Lit 3, []);
                       Apply(Some "z", "foo", ["y"; "x"]); Return(Some "z")]}
    );
    ("foo",
     {name = "foo";
      signature = ["a"; "b"], RetInt; vars = ["tmp"];
      body = make_seq [
          Block(Loop(Seq(AssignOp("tmp", Lt, ["a"; "b"]), Ite("tmp",
                                                        Break,
                                                        make_seq [AssignOp("tmp", Lit 1, []); AssignOp("a", Minus, ["a"; "tmp"]); AssignOp("b", Plus, ["b"; "tmp"])]
                                                       ))));
          AssignOp("a", Plus, ["a"; "a"]); AssignOp("a", Plus, ["a"; "b"]); Return(Some "a")]}
    )
  ]
let layout_func fd =
  let args, ret_tp = fd.signature in
  spf "%s %s(%s){\nint %s = 0;\n%s\n}"
    (match ret_tp with RetVoid -> "void" | RetInt -> "int") fd.name (StrList.to_string args)
    (StrList.to_string fd.vars)
    (layout_statement fd.body)

(* let example = {
 *   entry = "main";
 *   funcs = ref funcs;
 * } *)

type continuation =
  | Stop
  | Continue of statement * continuation
  | EndBlock of continuation
  | ReturnTo of {retk_id: var option; retk_e: int StrMap.t; retk_k: continuation}

let layout_e e = List.to_string (fun (k, v) -> spf "%s:=%i" k v) (StrMap.to_kv_list e)
let rec layout_k = function
  | Stop -> "stop"
  | Continue (s, k) -> spf "{K %s;; %s}" (layout_statement s) (layout_k k)
  | EndBlock k -> spf "{EB %s}" (layout_k k)
  | ReturnTo {retk_id; retk_e; retk_k} ->
    let rt = match retk_id with None -> "" | Some x -> x in
    spf "{RT return to %s with poped env: [%s] and %s}" rt (layout_e retk_e) (layout_k retk_k)

type state =
  | RegularState of {s: statement; k: continuation; e: int StrMap.t}
  | CallState of {fd: fd; v: int list; k: continuation}
  | ReturnState of {retv: int option; k: continuation}

let layout_state = function
  | RegularState {s; k; e} -> spf "Regular\n\ts: %s\n\tk: %s\n\te: %s" (layout_statement s) (layout_k k) (layout_e e)
  | CallState {v; k; fd} -> spf "Call\n\tfunc: %s\n\targs: %s\n\tk: %s" fd.name (List.to_string string_of_int v) (layout_k k)
  | ReturnState {retv; k} -> spf "Return\n\tv: %s\n\tk: %s" (match retv with | None -> "void" | Some i -> spf "%i" i) (layout_k k)

let callcont k = (
  let rec aux k =
    match k with
    | Continue (_, k) -> aux k
    | EndBlock k -> k
    | _ -> k
  in
  aux k
)

let eval e var = StrMap.find (Printf.sprintf "cannot find value of %s" var) e var
let update e var v = StrMap.update var (function
    | None -> failwith (Printf.sprintf "cannot find value of %s" var)
    | Some _ -> Some v) e

let machine_regular g (s, k, e) = (
  let next_k k new_e =
    match k with
    | Stop -> ReturnState {retv = None; k = Stop}
    | Continue (s, k) -> RegularState { s = s; k = k; e = new_e}
    | EndBlock k -> Printf.printf "drop end block\n"; RegularState { s = Skip ; k = k; e = new_e}
    | ReturnTo {retk_id; _} ->
      match retk_id with
      | None -> ReturnState {retv = None; k = k}
      | Some id -> ReturnState {retv = Some (eval e id); k = k}
  in
  match s with
  | Skip -> next_k k e
  | Assign (x, y) ->
    RegularState { s = Skip; k = k; e = (update e x (eval e y))}
  | AssignOp (x, op, args) ->
    let argsvalue = List.map (eval e) args in
    let e' =
      match op, argsvalue with
      | Plus, [a; b] -> update e x (a + b)
      | Minus, [a; b] -> update e x (a - b)
      | Lt, [a; b] -> update e x (if a < b then 1 else 0)
      | Lit i, [] -> update e x i
      | _ -> failwith "wrong op"
    in
    RegularState { s = Skip; k = k; e = e'}
  | Seq (s1, s2) -> RegularState { s = s1; k = Continue (s2, k); e = e}
  | Loop s -> RegularState { s = s; k = Continue (Loop s, k); e = e}
  | Block s -> RegularState { s = s; k = EndBlock k; e = e}
  | Break ->
    (* what happen if we call callcont here? *)
    (match k with
     | Continue (_, k) -> RegularState { s = Break; k = k; e = e}
     | EndBlock k -> RegularState { s = Skip; k = k; e = e}
     | _ -> failwith "should not happen!")
  | Ite (x, s1, s2) ->
    let s' = if (eval e x) == 1 then s1 else s2 in
    RegularState { s = s'; k = k; e = e}
  | Apply (retvar, fid, args) ->
    let v = List.map (eval e) args in
    let fd = StrMap.find (Printf.sprintf "cannot find def of %s" fid) !g fid in
    CallState {fd = fd; v = v;
               k = ReturnTo {retk_id = retvar; retk_e = e; retk_k = k}}
  | Return x ->
    ReturnState {
      retv = (match x with
          | None -> None
          | Some x -> Some (eval e x));
      k = callcont k}
)

let machine_return (retv, k) = (
  match k with
  | Stop -> None
  | Continue (_, _) | EndBlock _ -> failwith "should not happen!"
  | ReturnTo {retk_id; retk_e; retk_k} ->
    match retk_id, retv with
    | Some id, Some v -> Some (RegularState {s = Skip; k = retk_k; e = update retk_e id v})
    | None, None -> Some (RegularState {s = Skip; k = retk_k; e = retk_e})
    | _ -> failwith "return type error!"
)

let machine_call (cur_fd, v, k) = (
  let args, _ = cur_fd.signature in
  if List.length args != List.length v then failwith "function args type error!" else
    let e = StrMap.from_kv_list ((List.map (fun x -> (x, 0)) cur_fd.vars) @ (List.combine args v)) in
    RegularState { s = cur_fd.body; k = k; e = e}
)

let machine g state =
  let step state =
    match state with
    | RegularState {s; k; e} -> Some (machine_regular g (s, k, e))
    | CallState {fd; v; k} -> Some (machine_call (fd, v, k))
    | ReturnState {retv; k} -> machine_return (retv, k)
  in
  let rec loop n state =
    match step state with
    | Some s ->
      Printf.printf "%i: %s\n" n (layout_state s);
      let _ = read_line ()  in loop (n + 1) s
    | None -> Printf.printf "end\n"
  in
  loop 0 state
