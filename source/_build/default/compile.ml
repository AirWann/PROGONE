(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...

*)

open Format
open Ast
open Tast
open X86_64
open Typing
let debug = ref false

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let malloc n = movq (imm n) !%rdi ++ call "malloc"
let allocz n = movq (imm n) !%rdi ++ call "allocz"

let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

type env = {
  exit_label: string;
  ofs_this: int;
  nb_locals: int ref; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}

let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = ref 0; next_local = 0 }

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) !%rdi ++ jmp l_end ++
  label l_true ++ movq (imm 1) !%rdi ++ label l_end
let rec expr (env : env) e = match e.expr_desc with
  | TEskip ->
    nop
  | TEconstant (Cbool true) ->
    movq (imm 1) !%rdi
  | TEconstant (Cbool false) ->
    movq (imm 0) !%rdi
  | TEconstant (Cint x) ->
    movq (imm64 x) !%rdi
  | TEnil ->
    xorq !%rdi !%rdi
  | TEconstant (Cstring s) ->
    movq (ilab (alloc_string s)) !%rdi
  | TEbinop (Band, e1, e2) -> (* on l'écrit : "SI non e1 ALORS false SINON e2" *)
    expr env
      ( mk_bool
        (
          TEif
          (
            mk_bool (TEunop(Unot, e1)),
            mk_bool (TEconstant (Cbool false)),
            e2
          )
        )
      )
  | TEbinop (Bor, e1, e2) -> (* "SI e1 ALORS vrai SINON e2" *)
  expr env
  ( mk_bool
    (
      TEif
      (
        e1,
        mk_bool (TEconstant (Cbool true)),
        e2
      )
    )
  )
  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) -> (* on compile e1 : il se retrouve dans rdi. Ensuite, on le déplace, on met e2 dans rdi, et on compare *)
    expr env e1 ++
    movq !%rdi !%rax ++
    expr env e2 ++
    cmpq !%rdi !%rax ++
    compile_bool (
      match op with
        |Blt -> jl
        |Ble -> jle
        |Bgt -> jg
        |Bge -> jge
        |_ -> error dummy_loc "does not happen"
    )

  | TEbinop (Badd | Bsub | Bmul | Bdiv | Bmod as op, e1, e2) ->
    expr env e1 ++
    movq !%rdi !%rax ++
    expr env e2 ++
    (
    match op with
    | Badd -> addq !%rax !%rdi
    | Bsub -> subq !%rax !%rdi
    | Bmul -> imulq !%rax !%rdi
    | Bdiv -> 
      movq (imm 0) !%rdx ++
      idivq !%rdi ++
      movq !%rax !%rdi
    | Bmod -> 
      movq (imm 0) !%rdx ++
      idivq !%rdi ++
      movq !%rdx !%rdi
    |_ -> error dummy_loc "does not happen"
    )
  | TEbinop (Beq | Bne as op, e1, e2) ->
    expr env e1 ++
    movq !%rdi !%rax ++
    expr env e2 ++
    cmpq !%rdi !%rax ++
    compile_bool 
    (
      match op with
      |Beq -> je
      |Bne -> jne
      | _ -> error dummy_loc "does not happen"
    )
  | TEunop (Uneg, e1) ->
    expr env e1 ++
    negq !%rdi
  | TEunop (Unot, e1) ->
    expr env e1 ++
    notq !%rdi
  | TEunop (Uamp, e1) ->
    (* TODO code pour & *) assert false 
  | TEunop (Ustar, e1) ->
    (* TODO code pour * *) assert false 
  | TEprint el ->
    let rec aux = function
    |[] -> nop
    |e::xs -> 
      expr env e ++
      (
        match e.expr_typ with
        |Tint -> call "print_int"
        |Tbool -> call "print_bool"
        |Tstring -> call "print_string"
        |Tstruct s -> nop (* TODO *)
        |Tptr _ -> nop
        |_ -> error dummy_loc "does not happen"
      ) ++ call "print_space" ++ aux xs

    in comment "début print" ++ aux el ++ comment "fin print"
  | TEident x ->
    (* TODO code pour x *) assert false 
  | TEassign ([{expr_desc=TEident x}], [e1]) ->
    (* TODO code pour x := e *) assert false 
  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... := e1,... *) assert false 
  | TEassign (_, _) ->
     assert false
  | TEblock el ->
     comment "début bloc" ++
     let rec aux = function
     |[] -> nop
     |{expr_desc = TEvars(v)}::xs ->
      List.fold_left 
      (
        fun res var ->
        (if var.v_name = "_" then nop else (incr env.nb_locals; pushq (imm 0))) 
        ++ res
      ) nop v ++ 
      aux xs
     |e::xs -> expr env e ++ aux xs
     in
     aux el ++ 
     comment "fin bloc"
  | TEif (e1, e2, e3) ->
     (* TODO code pour if *) assert false
  | TEfor (e1, e2) ->
     (* TODO code pour for *) assert false
  | TEnew ty ->
     (* TODO code pour new S *) assert false
  | TEcall (f, el) ->
     (* TODO code pour appel fonction *) assert false
  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *) assert false
  | TEvars _ ->
     assert false (* fait dans block *)
  | TEreturn [] ->
    (* TODO code pour return e *) assert false
  | TEreturn [e1] ->
    (* TODO code pour return e1,... *) assert false
  | TEreturn _ ->
     assert false
  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *) assert false


let rec nvars e = match e.expr_desc with
|TEvars v -> List.length (List.filter (fun x -> x.v_name <> "_") v)
|TEblock b -> List.fold_left (fun x y -> x + (nvars y)) 0 b
|_ -> 0

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* TODO code pour fonction *) 
  let s = f.fn_name in 
  let n = nvars e in
  let nparams = List.length f.fn_params in
  let env = { exit_label = ""; ofs_this = nparams - 1; nb_locals = ref 0; next_local = n} in
  
  label ("F_" ^ s) ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  expr env e ++

  movq !%rbp !%rsp ++
  popq rbp ++
  ret



let decl code = function
  | TDstruct _ -> code
  | TDfunction (f, e) -> code ++ function_ f e

let file ?debug:(b=false) dl =
  debug := b;
  (* TODO calcul offset champs *)
  (* TODO code fonctions *) let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (!%rax) (!%rax) ++
      ret ++
      funs ++ 
      inline "
print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret
print_bool:
        test    %rdi, %rdi
        jz      print_false
        mov     $S_true, %rdi
        call    printf
        xorq    %rax, %rax
        ret
print_false:
        mov     $S_false, %rdi
        call    printf
        xorq    %rax, %rax
        ret
print_string:
        test    %rdi, %rdi
        jz      print_nil
        mov     %rdi, %rsi
        mov     $S_string, %rdi
        xorq    %rax, %rax
        call    printf
        ret
print_nil:
        mov     $S_nil, %rdi
        xorq    %rax, %rax
        call    printf
        ret      
print_space:
        mov     $S_space, %rdi
        xorq    %rax, %rax
        call    printf
        ret   
"; (* TODO print pour d'autres valeurs *)
   (* TODO appel malloc de stdlib *)
    data =
      label "S_int" ++ string "%ld" ++
      label "S_true" ++ string "true" ++
      label "S_false" ++ string "false" ++
      label "S_string" ++ string "%s" ++
      label "S_nil" ++ string "<nil>" ++
      label "S_space" ++ string " " ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
