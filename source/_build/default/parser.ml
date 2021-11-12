
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | VERTICALBARVERTICALBAR
    | VAR
    | TYPE
    | STRUCT
    | STRING of (
# 11 "parser.mly"
       (string)
# 18 "parser.ml"
  )
    | STAR
    | SLASH
    | SEMICOLON
    | RIGHTPAR
    | RIGHTBRACE
    | RETURN
    | PLUSPLUS
    | PLUS
    | PERCENT
    | PACKAGE
    | NIL
    | MINUSMINUS
    | MINUS
    | LEFTPAR
    | LEFTBRACE
    | IMPORT
    | IF
    | IDENT of (
# 10 "parser.mly"
       (string)
# 40 "parser.ml"
  )
    | FUNC
    | FOR
    | EQ
    | EOF
    | ELSE
    | DOT
    | CST of (
# 9 "parser.mly"
       (Ast.constant)
# 51 "parser.ml"
  )
    | COMP of (
# 21 "parser.mly"
       (Ast.binop)
# 56 "parser.ml"
  )
    | COMMA
    | COLONEQ
    | BANG
    | AMPERSANDAMPERSAND
    | AMP
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState149
  | MenhirState144
  | MenhirState143
  | MenhirState132
  | MenhirState127
  | MenhirState123
  | MenhirState121
  | MenhirState119
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState109
  | MenhirState105
  | MenhirState103
  | MenhirState102
  | MenhirState100
  | MenhirState99
  | MenhirState94
  | MenhirState93
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState56
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState32
  | MenhirState30
  | MenhirState25
  | MenhirState23
  | MenhirState15
  | MenhirState14
  | MenhirState13
  | MenhirState10
  | MenhirState9
  | MenhirState1

# 4 "parser.mly"
  
  open Ast
  let mk_expr loc d = { pexpr_desc = d; pexpr_loc = loc }

# 151 "parser.ml"

let rec _menhir_goto_statements : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_statements -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv611 * _menhir_state * 'tv_stmt) * Lexing.position) * _menhir_state * 'tv_statements) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv609 * _menhir_state * 'tv_stmt) * Lexing.position) * _menhir_state * 'tv_statements) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (s : 'tv_stmt)), _endpos__2_), _, (l : 'tv_statements)) = _menhir_stack in
        let _v : 'tv_statements = 
# 111 "parser.mly"
                                ( s :: l )
# 166 "parser.ml"
         in
        _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv610)) : 'freshtv612)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv615 * Lexing.position * _menhir_state) * _menhir_state * 'tv_statements) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv613 * Lexing.position * _menhir_state) * _menhir_state * 'tv_statements) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s), _, (l : 'tv_statements)) = _menhir_stack in
        let _v : 'tv_statements = 
# 112 "parser.mly"
                                ( l      )
# 178 "parser.ml"
         in
        _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv614)) : 'freshtv616)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv623 * _menhir_state * Lexing.position) * _menhir_state * 'tv_statements) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHTBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv619 * _menhir_state * Lexing.position) * _menhir_state * 'tv_statements) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv617 * _menhir_state * Lexing.position) * _menhir_state * 'tv_statements) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (l : 'tv_statements)) = _menhir_stack in
            let _endpos = _endpos__3_ in
            let _v : 'tv_block = let _endpos = _endpos__3_ in
            let _startpos = _startpos__1_ in
            
# 105 "parser.mly"
  ( { pexpr_desc = PEblock l; pexpr_loc = _startpos, _endpos } )
# 202 "parser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv618)) : 'freshtv620)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv621 * _menhir_state * Lexing.position) * _menhir_state * 'tv_statements) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv622)) : 'freshtv624)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv607 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv601 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CST _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFTBRACE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFTPAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMICOLON ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VAR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RIGHTBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv599 * _menhir_state * 'tv_stmt) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (s : 'tv_stmt)), _endpos__2_) = _menhir_stack in
            let _v : 'tv_statements = 
# 110 "parser.mly"
                                ( [s]    )
# 268 "parser.ml"
             in
            _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv600)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv602)
    | RIGHTBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv603 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_statements = 
# 109 "parser.mly"
                                ( [s]    )
# 282 "parser.ml"
         in
        _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv605 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv606)) : 'freshtv608)

and _menhir_goto_if_stmt_desc : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_if_stmt_desc -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv597) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_if_stmt_desc) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv595) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : 'tv_if_stmt_desc) : 'tv_if_stmt_desc) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos_d_ in
    let _v : 'tv_if_stmt = let _endpos = _endpos_d_ in
    let _startpos = _startpos_d_ in
    
# 148 "parser.mly"
    ( { pexpr_desc = d; pexpr_loc = _startpos, _endpos } )
# 313 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv593) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_if_stmt) = _v in
    ((match _menhir_s with
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv587 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_if_stmt) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv585 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block)) = Obj.magic _menhir_stack in
        let (_endpos_s2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((s2 : 'tv_if_stmt) : 'tv_if_stmt) = _v in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _endpos_s1_, _, (s1 : 'tv_block)) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s2_ in
        let _v : 'tv_if_stmt_desc = 
# 155 "parser.mly"
  ( PEif (e, s1, s2) )
# 338 "parser.ml"
         in
        _menhir_goto_if_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv586)) : 'freshtv588)
    | MenhirState46 | MenhirState99 | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv591) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_if_stmt) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589) = Obj.magic _menhir_stack in
        let (_endpos_s_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((s : 'tv_if_stmt) : 'tv_if_stmt) = _v in
        ((let _v : 'tv_stmt = 
# 122 "parser.mly"
    ( s )
# 355 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv590)) : 'freshtv592)
    | _ ->
        _menhir_fail ()) : 'freshtv594)) : 'freshtv596)) : 'freshtv598)

and _menhir_goto_simple_stmt_desc : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_simple_stmt_desc -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv583) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_simple_stmt_desc) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv581) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : 'tv_simple_stmt_desc) : 'tv_simple_stmt_desc) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _v : 'tv_simple_stmt = let _endpos = _endpos_d_ in
    let _startpos = _startpos_d_ in
    
# 172 "parser.mly"
  ( { pexpr_desc = d; pexpr_loc = _startpos, _endpos } )
# 380 "parser.ml"
     in
    _menhir_goto_simple_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv582)) : 'freshtv584)

and _menhir_goto_stmt_desc : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_stmt_desc -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv579) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_stmt_desc) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv577) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : 'tv_stmt_desc) : 'tv_stmt_desc) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _v : 'tv_stmt = let _endpos = _endpos_d_ in
    let _startpos = _startpos_d_ in
    
# 124 "parser.mly"
    ( { pexpr_desc = d; pexpr_loc = _startpos, _endpos } )
# 403 "parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)) : 'freshtv580)

and _menhir_goto_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_parameters -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv571) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_parameters) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv569) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_parameters) : 'tv_parameters) = _v in
        ((let _v : 'tv_loption_parameters_ = 
# 144 "<standard.mly>"
    ( x )
# 422 "parser.ml"
         in
        _menhir_goto_loption_parameters_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv570)) : 'freshtv572)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv575 * _menhir_state * 'tv_ids_and_type) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_parameters) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv573 * _menhir_state * 'tv_ids_and_type) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xl : 'tv_parameters) : 'tv_parameters) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_ids_and_type)), _) = _menhir_stack in
        let _v : 'tv_parameters = 
# 76 "parser.mly"
                                           ( x :: xl )
# 438 "parser.ml"
         in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v) : 'freshtv574)) : 'freshtv576)
    | _ ->
        _menhir_fail ()

and _menhir_goto_return_types : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_types -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv559 * Lexing.position * _menhir_state * 'tv_type_expr) * _menhir_state) * _menhir_state * 'tv_return_types) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv557 * Lexing.position * _menhir_state * 'tv_type_expr) * _menhir_state) * _menhir_state * 'tv_return_types) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos_ty_, _menhir_s, (ty : 'tv_type_expr)), _), _, (tyl : 'tv_return_types)) = _menhir_stack in
        let _v : 'tv_return_types = 
# 91 "parser.mly"
                                            ( ty :: tyl )
# 457 "parser.ml"
         in
        _menhir_goto_return_types _menhir_env _menhir_stack _menhir_s _v) : 'freshtv558)) : 'freshtv560)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv567 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_return_types) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv563 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_return_types) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv561 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_return_types) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _, (tyl : 'tv_return_types)) = _menhir_stack in
            let _v : 'tv_return_type = 
# 86 "parser.mly"
                                      ( tyl )
# 478 "parser.ml"
             in
            _menhir_goto_return_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv562)) : 'freshtv564)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv565 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_return_types) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_decl_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv551) * _menhir_state * 'tv_ident_main) * Lexing.position) * 'tv_boption_import_fmt_) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv547) * _menhir_state * 'tv_ident_main) * Lexing.position) * 'tv_boption_import_fmt_) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv545) * _menhir_state * 'tv_ident_main) * Lexing.position) * 'tv_boption_import_fmt_) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _, _), _endpos__3_), (imp : 'tv_boption_import_fmt_)), _, (dl : 'tv_list_decl_)) = _menhir_stack in
            let _v : (
# 39 "parser.mly"
      (Ast.pfile)
# 510 "parser.ml"
            ) = 
# 47 "parser.mly"
  ( imp, dl )
# 514 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv543) = _menhir_stack in
            let (_v : (
# 39 "parser.mly"
      (Ast.pfile)
# 521 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv541) = Obj.magic _menhir_stack in
            let (_v : (
# 39 "parser.mly"
      (Ast.pfile)
# 528 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv539) = Obj.magic _menhir_stack in
            let ((_1 : (
# 39 "parser.mly"
      (Ast.pfile)
# 535 "parser.ml"
            )) : (
# 39 "parser.mly"
      (Ast.pfile)
# 539 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv540)) : 'freshtv542)) : 'freshtv544)) : 'freshtv546)) : 'freshtv548)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv549) * _menhir_state * 'tv_ident_main) * Lexing.position) * 'tv_boption_import_fmt_) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv550)) : 'freshtv552)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv555 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv553 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_decl)), _, (xs : 'tv_list_decl_)) = _menhir_stack in
        let _v : 'tv_list_decl_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 558 "parser.ml"
         in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv554)) : 'freshtv556)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState115 | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv533) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_simple_stmt) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv531) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((s : 'tv_simple_stmt) : 'tv_simple_stmt) = _v in
        ((let _v : 'tv_opt_simple_stmt = 
# 165 "parser.mly"
                  ( s )
# 579 "parser.ml"
         in
        _menhir_goto_opt_simple_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv532)) : 'freshtv534)
    | MenhirState46 | MenhirState99 | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv537) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_simple_stmt) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv535) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((s : 'tv_simple_stmt) : 'tv_simple_stmt) = _v in
        ((let _v : 'tv_stmt = 
# 118 "parser.mly"
    ( s )
# 594 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv536)) : 'freshtv538)
    | _ ->
        _menhir_fail ()

and _menhir_goto_incdec : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_incdec -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv529 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_incdec) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv527 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_ : _menhir_state) = _menhir_s in
    let ((i : 'tv_incdec) : 'tv_incdec) = _v in
    ((let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_simple_stmt_desc = 
# 181 "parser.mly"
  ( PEincdec (e, i) )
# 618 "parser.ml"
     in
    _menhir_goto_simple_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv528)) : 'freshtv530)

and _menhir_goto_opt_simple_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_simple_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv523 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv519 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMP ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CST _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFTPAR ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv520)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv521 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv522)) : 'freshtv524)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv525 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_opt_simple_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTBRACE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv526)
    | _ ->
        _menhir_fail ()

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run99 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTBRACE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMICOLON ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | VAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHTBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv517 * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos__1_, _menhir_s) = _menhir_stack in
        let _v : 'tv_statements = 
# 113 "parser.mly"
                                ( []     )
# 740 "parser.ml"
         in
        _menhir_goto_statements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv518)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_block -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv487 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv481 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFTBRACE ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv482)
        | RIGHTBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv483 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _endpos_s_, _, (s : 'tv_block)) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_s_ in
            let _v : 'tv_if_stmt_desc = let _endpos = _endpos_s_ in
            let _startpos = _startpos__1_ in
            
# 153 "parser.mly"
  ( PEif (e, s, { pexpr_desc = PEskip; pexpr_loc = _startpos, _endpos }) )
# 783 "parser.ml"
             in
            _menhir_goto_if_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv484)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv485 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv491 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block)) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv489 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block)) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_), _endpos_s1_, _, (s1 : 'tv_block)), _endpos_s2_, _, (s2 : 'tv_block)) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s2_ in
        let _v : 'tv_if_stmt_desc = 
# 157 "parser.mly"
  ( PEif (e, s1, s2) )
# 804 "parser.ml"
         in
        _menhir_goto_if_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv490)) : 'freshtv492)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv495 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv493 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let (((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _, (s1 : 'tv_opt_simple_stmt)), _endpos__3_), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_), _endpos__5_, _), _, (s3 : 'tv_opt_simple_stmt)), _endpos_b_, _, (b : 'tv_block)) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_b_ in
        let _v : 'tv_stmt_desc = let _endpos = _endpos_b_ in
        let _startpos = _startpos__1_ in
        
# 141 "parser.mly"
  ( let loc = _startpos, _endpos in
    let b = mk_expr loc (PEblock [b; s3]) in
    PEblock [s1; mk_expr loc (PEfor (e2, b))] )
# 822 "parser.ml"
         in
        _menhir_goto_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv494)) : 'freshtv496)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv499 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv497 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : 'tv_expr), _startpos_e1_), _endpos_b_, _, (b : 'tv_block)) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_b_ in
        let _v : 'tv_stmt_desc = 
# 138 "parser.mly"
  ( PEfor (e1, b) )
# 836 "parser.ml"
         in
        _menhir_goto_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv498)) : 'freshtv500)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv503 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_b_, _, (b : 'tv_block)) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_b_ in
        let _v : 'tv_stmt_desc = let _endpos = _endpos_b_ in
        let _startpos = _startpos__1_ in
        
# 134 "parser.mly"
  ( let loc = _startpos, _endpos in
    let etrue = mk_expr loc (PEconstant (Cbool true)) in
    PEfor (etrue, b) )
# 854 "parser.ml"
         in
        _menhir_goto_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv502)) : 'freshtv504)
    | MenhirState46 | MenhirState99 | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv507 * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv505 * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_b_, _menhir_s, (b : 'tv_block)) = _menhir_stack in
        let _v : 'tv_stmt = 
# 120 "parser.mly"
    ( b )
# 866 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv515 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) * Lexing.position) * _menhir_state * 'tv_loption_return_type_) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv511 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) * Lexing.position) * _menhir_state * 'tv_loption_return_type_) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv509 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) * Lexing.position) * _menhir_state * 'tv_loption_return_type_) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
            let (_endpos__8_ : Lexing.position) = _endpos in
            ((let (((((((_menhir_stack, _menhir_s), _endpos_id_, _, (id : 'tv_ident), _startpos_id_), _endpos__3_, _startpos__3_), _, (pl : 'tv_loption_parameters_)), _endpos__5_), _, (ty : 'tv_loption_return_type_)), _endpos_b_, _, (b : 'tv_block)) = _menhir_stack in
            let _v : 'tv_decl = 
# 62 "parser.mly"
  ( PDfunction { pf_name = id;
                 pf_params = List.flatten pl;
                 pf_typ = ty;
                 pf_body = b } )
# 890 "parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)) : 'freshtv512)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv513 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) * Lexing.position) * _menhir_state * 'tv_loption_return_type_) * Lexing.position * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv514)) : 'freshtv516)
    | _ ->
        _menhir_fail ()

and _menhir_run100 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHTBRACE | SEMICOLON ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run109 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTBRACE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMICOLON ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState100 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv441) = Obj.magic _menhir_stack in
        let (_endpos_x_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        let (_startpos_x_ : Lexing.position) = _startpos in
        ((let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 1016 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_expr), _startpos_x_), _) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1038 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv446)) : 'freshtv448)
    | MenhirState46 | MenhirState99 | MenhirState132 | MenhirState121 | MenhirState119 | MenhirState115 | MenhirState109 | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv477) = Obj.magic _menhir_stack in
        let (_endpos_el_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((el : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        let (_startpos_el_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_el_ in
        let _endpos = _endpos_el_ in
        let _v : 'tv_exprs = 
# 190 "parser.mly"
                                            ( el )
# 1059 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv475) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_exprs) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        match _menhir_s with
        | MenhirState51 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv457) * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv455) * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_el_, _, (el : 'tv_exprs), _startpos_el_) = _menhir_stack in
            let _endpos = _endpos_el_ in
            let _v : 'tv_init = 
# 160 "parser.mly"
              ( el )
# 1079 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv453) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_init) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_init) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
            let (_endpos_x_ : Lexing.position) = _endpos in
            let ((x : 'tv_init) : 'tv_init) = _v in
            ((let _endpos = _endpos_x_ in
            let _v : 'tv_loption_init_ = 
# 144 "<standard.mly>"
    ( x )
# 1097 "parser.ml"
             in
            _menhir_goto_loption_init_ _menhir_env _menhir_stack _endpos _v) : 'freshtv450)) : 'freshtv452)) : 'freshtv454)) : 'freshtv456)) : 'freshtv458)
        | MenhirState46 | MenhirState99 | MenhirState132 | MenhirState109 | MenhirState115 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv465 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLONEQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv459 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AMP ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BANG ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CST _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFTPAR ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NIL ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STAR ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv460)
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv461 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AMP ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BANG ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CST _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFTPAR ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NIL ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STAR ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119) : 'freshtv462)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv463 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)) : 'freshtv466)
        | MenhirState119 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv469 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position)) * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv467 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position)) * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_lvl_, _menhir_s, (lvl : 'tv_exprs), _startpos_lvl_), _endpos_el_, _, (el : 'tv_exprs), _startpos_el_) = _menhir_stack in
            let _startpos = _startpos_lvl_ in
            let _endpos = _endpos_el_ in
            let _v : 'tv_simple_stmt_desc = 
# 176 "parser.mly"
  ( PEassign (lvl, el) )
# 1180 "parser.ml"
             in
            _menhir_goto_simple_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv468)) : 'freshtv470)
        | MenhirState121 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv473 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position)) * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv471 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position)) * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_lvl_, _menhir_s, (lvl : 'tv_exprs), _startpos_lvl_), _endpos_el_, _, (el : 'tv_exprs), _startpos_el_) = _menhir_stack in
            let _startpos = _startpos_lvl_ in
            let _endpos = _endpos_el_ in
            let _v : 'tv_simple_stmt_desc = 
# 178 "parser.mly"
  ( let var = function {pexpr_desc=PEident id} -> id | _ -> raise Error in
    PEvars (List.map var lvl, None, el) )
# 1195 "parser.ml"
             in
            _menhir_goto_simple_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv472)) : 'freshtv474)
        | _ ->
            _menhir_fail ()) : 'freshtv476)) : 'freshtv478)) : 'freshtv480)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv435 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv431 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv429 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _endpos = _endpos__3_ in
            let _v : 'tv_arguments = let l = 
# 232 "<standard.mly>"
    ( xs )
# 1226 "parser.ml"
             in
            
# 231 "parser.mly"
  ( l )
# 1231 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv427) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_arguments) = _v in
            ((match _menhir_s with
            | MenhirState72 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv421 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_arguments) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv419 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos_el_ : Lexing.position) = _endpos in
                let (_ : _menhir_state) = _menhir_s in
                let ((el : 'tv_arguments) : 'tv_arguments) = _v in
                ((let (((_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_), _), _endpos_id_, _, (id : 'tv_ident), _startpos_id_) = _menhir_stack in
                let _startpos = _startpos_e_ in
                let _endpos = _endpos_el_ in
                let _v : 'tv_expr_desc = 
# 214 "parser.mly"
   ( match e.pexpr_desc, id.id with
     | PEident {id="fmt"}, "Print" -> PEcall ({id with id = "fmt.Print"}, el)
     | _ -> raise Error )
# 1258 "parser.ml"
                 in
                _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv420)) : 'freshtv422)
            | MenhirState60 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv425 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_arguments) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv423 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos_el_ : Lexing.position) = _endpos in
                let (_ : _menhir_state) = _menhir_s in
                let ((el : 'tv_arguments) : 'tv_arguments) = _v in
                ((let (_menhir_stack, _endpos_id_, _menhir_s, (id : 'tv_ident), _startpos_id_) = _menhir_stack in
                let _startpos = _startpos_id_ in
                let _endpos = _endpos_el_ in
                let _v : 'tv_expr_desc = 
# 212 "parser.mly"
   ( PEcall (id, el) )
# 1278 "parser.ml"
                 in
                _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv424)) : 'freshtv426)
            | _ ->
                _menhir_fail ()) : 'freshtv428)) : 'freshtv430)) : 'freshtv432)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv433 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv437 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_stmt_desc = let el = 
# 232 "<standard.mly>"
    ( xs )
# 1301 "parser.ml"
         in
        
# 132 "parser.mly"
  ( PEreturn el )
# 1306 "parser.ml"
         in
        _menhir_goto_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv438)) : 'freshtv440)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_init_ : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_loption_init_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv417 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) * Lexing.position * _menhir_state * 'tv_option_type_expr_) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_loption_init_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv415 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) * Lexing.position * _menhir_state * 'tv_option_type_expr_) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let ((i : 'tv_loption_init_) : 'tv_loption_init_) = _v in
    ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_ids_, _, (ids : 'tv_separated_nonempty_list_COMMA_ident_)), _endpos_ty_, _, (ty : 'tv_option_type_expr_)) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_stmt_desc = 
# 130 "parser.mly"
  ( PEvars (ids, ty, i) )
# 1328 "parser.ml"
     in
    _menhir_goto_stmt_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv416)) : 'freshtv418)

and _menhir_goto_option_COMMA_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_COMMA_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_option_COMMA_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv407 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_ : 'tv_option_COMMA_) = _v in
        ((let (_menhir_stack, _endpos_ty_, _menhir_s, (ty : 'tv_type_expr)) = _menhir_stack in
        let _v : 'tv_return_types = 
# 90 "parser.mly"
                                            ( [ty] )
# 1348 "parser.ml"
         in
        _menhir_goto_return_types _menhir_env _menhir_stack _menhir_s _v) : 'freshtv408)) : 'freshtv410)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_option_COMMA_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_ : 'tv_option_COMMA_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_ids_and_type)) = _menhir_stack in
        let _v : 'tv_parameters = 
# 75 "parser.mly"
                                           ( [x]     )
# 1364 "parser.ml"
         in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v) : 'freshtv412)) : 'freshtv414)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fields : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fields -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv401 * _menhir_state * 'tv_ids_and_type) * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fields) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv399 * _menhir_state * 'tv_ids_and_type) * Lexing.position) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xl : 'tv_fields) : 'tv_fields) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_ids_and_type)), _endpos__2_) = _menhir_stack in
        let _v : 'tv_fields = 
# 72 "parser.mly"
                                           ( x :: xl )
# 1386 "parser.ml"
         in
        _menhir_goto_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)) : 'freshtv402)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv405) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_fields) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv403) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_fields) : 'tv_fields) = _v in
        ((let _v : 'tv_loption_fields_ = 
# 144 "<standard.mly>"
    ( x )
# 1401 "parser.ml"
         in
        _menhir_goto_loption_fields_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)) : 'freshtv406)
    | _ ->
        _menhir_fail ()

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_decl_ = 
# 211 "<standard.mly>"
    ( [] )
# 1412 "parser.ml"
     in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState10 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_reduce74 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
    let _v : 'tv_simple_stmt = 
# 170 "parser.mly"
  ( e )
# 1448 "parser.ml"
     in
    _menhir_goto_simple_stmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_run124 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv397) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_incdec = 
# 185 "parser.mly"
             ( Inc )
# 1463 "parser.ml"
     in
    _menhir_goto_incdec _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv398)

and _menhir_run125 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_incdec = 
# 186 "parser.mly"
             ( Dec )
# 1478 "parser.ml"
     in
    _menhir_goto_incdec _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv396)

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos__0_) = Obj.magic _menhir_stack in
    let _v : 'tv_opt_simple_stmt = let _endpos = _endpos__0_ in
    let _startpos = _endpos__0_ in
    
# 164 "parser.mly"
                  ( { pexpr_desc = PEskip; pexpr_loc = _startpos, _endpos } )
# 1490 "parser.ml"
     in
    _menhir_goto_opt_simple_stmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTBRACE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHTBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv393 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState46 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_block = let _endpos = _endpos__2_ in
        let _startpos = _startpos__1_ in
        
# 103 "parser.mly"
  ( { pexpr_desc = PEblock []; pexpr_loc = _startpos, _endpos } )
# 1539 "parser.ml"
         in
        _menhir_goto_block _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv392)) : 'freshtv394)
    | SEMICOLON ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | VAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_reduce70 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_expr), _startpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1563 "parser.ml"
     in
    _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run67 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run74 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run76 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run78 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run80 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run71 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run82 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> (
# 21 "parser.mly"
       (Ast.binop)
# 1757 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState82 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState82 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState82 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run86 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run84 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 1851 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 1858 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv389) = Obj.magic _menhir_stack in
    let (_endpos_s_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 11 "parser.mly"
       (string)
# 1869 "parser.ml"
    )) : (
# 11 "parser.mly"
       (string)
# 1873 "parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_expr_desc = 
# 202 "parser.mly"
    ( PEconstant (Cstring s) )
# 1881 "parser.ml"
     in
    _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv390)

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run54 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv387) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_expr_desc = 
# 204 "parser.mly"
    ( PEnil )
# 1927 "parser.ml"
     in
    _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv388)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run57 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.constant)
# 1992 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv385) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 9 "parser.mly"
       (Ast.constant)
# 2003 "parser.ml"
    )) : (
# 9 "parser.mly"
       (Ast.constant)
# 2007 "parser.ml"
    )) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_expr_desc = 
# 200 "parser.mly"
    ( PEconstant c )
# 2015 "parser.ml"
     in
    _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv386)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_loption_return_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_return_type_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv383 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) * Lexing.position) * _menhir_state * 'tv_loption_return_type_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTBRACE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv384)

and _menhir_goto_option_type_expr_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_option_type_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv381 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) * Lexing.position * _menhir_state * 'tv_option_type_expr_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CST _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFTPAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NIL ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv376)
    | RIGHTBRACE | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv377) = Obj.magic _menhir_stack in
        ((let (_, _endpos) = Obj.magic _menhir_stack in
        let _v : 'tv_loption_init_ = 
# 142 "<standard.mly>"
    ( [] )
# 2135 "parser.ml"
         in
        _menhir_goto_loption_init_ _menhir_env _menhir_stack _endpos _v) : 'freshtv378)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) * Lexing.position * _menhir_state * 'tv_option_type_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)

and _menhir_goto_return_type : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv373) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_return_type) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv371) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : 'tv_return_type) : 'tv_return_type) = _v in
    ((let _v : 'tv_loption_return_type_ = 
# 144 "<standard.mly>"
    ( x )
# 2159 "parser.ml"
     in
    _menhir_goto_loption_return_type_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)) : 'freshtv374)

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_COMMA_ = 
# 114 "<standard.mly>"
    ( None )
# 2183 "parser.ml"
     in
    _menhir_goto_option_COMMA_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce59 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let x = () in
    let _v : 'tv_option_COMMA_ = 
# 116 "<standard.mly>"
    ( Some x )
# 2194 "parser.ml"
     in
    _menhir_goto_option_COMMA_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> 'tv_option_SEMICOLON_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
    let (_v : 'tv_option_SEMICOLON_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv367 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
    let (_ : 'tv_option_SEMICOLON_) = _v in
    ((let (_menhir_stack, _menhir_s, (x : 'tv_ids_and_type)) = _menhir_stack in
    let _v : 'tv_fields = 
# 71 "parser.mly"
                                           ( [x]     )
# 2210 "parser.ml"
     in
    _menhir_goto_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv368)) : 'freshtv370)

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv365 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | TYPE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | EOF ->
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv366)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_boption_import_fmt_ : _menhir_env -> 'ttv_tail -> 'tv_boption_import_fmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv363) * _menhir_state * 'tv_ident_main) * Lexing.position) * 'tv_boption_import_fmt_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TYPE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | EOF ->
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv364)

and _menhir_goto_expr_desc : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_expr_desc -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv361) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_expr_desc) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : 'tv_expr_desc) : 'tv_expr_desc) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_expr = let _endpos = _endpos_d_ in
    let _startpos = _startpos_d_ in
    
# 195 "parser.mly"
  ( { pexpr_desc = d; pexpr_loc = _startpos, _endpos } )
# 2278 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv357) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_expr) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState121 | MenhirState119 | MenhirState100 | MenhirState51 | MenhirState86 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSANDAMPERSAND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | COMMA ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | COMP _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VERTICALBARVERTICALBAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | COLONEQ | EQ | LEFTBRACE | RIGHTBRACE | RIGHTPAR | SEMICOLON ->
            _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv292)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv295 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSANDAMPERSAND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | COMP _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLONEQ | COMMA | EQ | LEFTBRACE | MINUSMINUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv293 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr_desc = let op = 
# 242 "parser.mly"
                           ( Bor  )
# 2351 "parser.ml"
             in
            
# 218 "parser.mly"
  ( PEbinop (op, e1, e2) )
# 2356 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv294)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv296)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv299 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv297 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _, _startpos__1_), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr_desc = let op = 
# 237 "parser.mly"
             ( Bmul )
# 2380 "parser.ml"
             in
            
# 218 "parser.mly"
  ( PEbinop (op, e1, e2) )
# 2385 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv300)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv301 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr_desc = let op = 
# 238 "parser.mly"
             ( Bdiv )
# 2409 "parser.ml"
             in
            
# 218 "parser.mly"
  ( PEbinop (op, e1, e2) )
# 2414 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv304)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv307 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv305 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr_desc = let op = 
# 235 "parser.mly"
             ( Badd )
# 2444 "parser.ml"
             in
            
# 218 "parser.mly"
  ( PEbinop (op, e1, e2) )
# 2449 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv306)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv308)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr_desc = let op = 
# 239 "parser.mly"
             ( Bmod )
# 2473 "parser.ml"
             in
            
# 218 "parser.mly"
  ( PEbinop (op, e1, e2) )
# 2478 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv310)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv312)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _, _startpos__1_), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr_desc = let op = 
# 236 "parser.mly"
             ( Bsub )
# 2508 "parser.ml"
             in
            
# 218 "parser.mly"
  ( PEbinop (op, e1, e2) )
# 2513 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv314)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv316)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv319 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * (
# 21 "parser.mly"
       (Ast.binop)
# 2525 "parser.ml"
        )) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUSMINUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv317 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * (
# 21 "parser.mly"
       (Ast.binop)
# 2547 "parser.ml"
            )) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _, (c : (
# 21 "parser.mly"
       (Ast.binop)
# 2552 "parser.ml"
            ))), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr_desc = let op = 
# 240 "parser.mly"
             ( c )
# 2559 "parser.ml"
             in
            
# 218 "parser.mly"
  ( PEbinop (op, e1, e2) )
# 2564 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv320)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv323 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMP _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | EQ | LEFTBRACE | MINUSMINUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv321 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expr), _startpos_e1_), _), _endpos_e2_, _, (e2 : 'tv_expr), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expr_desc = let op = 
# 241 "parser.mly"
                           ( Band )
# 2600 "parser.ml"
             in
            
# 218 "parser.mly"
  ( PEbinop (op, e1, e2) )
# 2605 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv322)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv324)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv327 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv325 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : 'tv_expr), _startpos_e1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e1_ in
            let _v : 'tv_expr_desc = 
# 224 "parser.mly"
  ( PEunop (Uamp, e1) )
# 2629 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv326)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv328)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv331 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv329 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : 'tv_expr), _startpos_e1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e1_ in
            let _v : 'tv_expr_desc = 
# 220 "parser.mly"
  ( PEunop (Unot, e1) )
# 2653 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv332)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv337 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSANDAMPERSAND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | COMP _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv335 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState91 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv333 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_expr_desc = 
# 206 "parser.mly"
  ( e.pexpr_desc )
# 2694 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv334)) : 'freshtv336)
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VERTICALBARVERTICALBAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv338)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv339 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : 'tv_expr), _startpos_e1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e1_ in
            let _v : 'tv_expr_desc = 
# 222 "parser.mly"
  ( PEunop (Uneg, e1) )
# 2724 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv340)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv342)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv345 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv343 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : 'tv_expr), _startpos_e1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e1_ in
            let _v : 'tv_expr_desc = 
# 226 "parser.mly"
  ( PEunop (Ustar, e1) )
# 2748 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv344)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv346)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv347 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSANDAMPERSAND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | COMP _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LEFTBRACE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VERTICALBARVERTICALBAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv348)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv351 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSANDAMPERSAND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | COMP _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv349 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState114 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMP ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CST _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFTPAR ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NIL ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFTBRACE ->
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv350)
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VERTICALBARVERTICALBAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv352)
    | MenhirState46 | MenhirState99 | MenhirState132 | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSANDAMPERSAND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | COMMA ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | COMP _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUSMINUS ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUSPLUS ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VERTICALBARVERTICALBAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LEFTBRACE | RIGHTBRACE | SEMICOLON ->
            _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack)
        | COLONEQ | EQ ->
            _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv354)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSANDAMPERSAND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | COMMA ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | COMP _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | DOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LEFTBRACE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUSMINUS ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127
        | PERCENT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PLUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PLUSPLUS ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127
        | SLASH ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | STAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VERTICALBARVERTICALBAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | SEMICOLON ->
            _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack)
        | COLONEQ | EQ ->
            _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv356)
    | _ ->
        _menhir_fail ()) : 'freshtv358)) : 'freshtv360)) : 'freshtv362)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMP ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CST _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFTPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NIL ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHTPAR ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_goto_loption_parameters_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_parameters_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv289 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHTPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv285 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv281) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState35 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv282)
        | STAR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFTBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState35 in
            ((let _v : 'tv_loption_return_type_ = 
# 142 "<standard.mly>"
    ( [] )
# 3003 "parser.ml"
             in
            _menhir_goto_loption_return_type_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv286)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv287 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)

and _menhir_goto_separated_nonempty_list_COMMA_ident_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_ident_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState144 | MenhirState32 | MenhirState23 | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv271 * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv272)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv273 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_ident), _startpos_x_), _endpos_xs_, _, (xs : 'tv_separated_nonempty_list_COMMA_ident_)) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_ident_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3046 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv274)) : 'freshtv276)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv279 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EQ | RIGHTBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv277) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState48 in
            ((let (_, _endpos) = Obj.magic _menhir_stack in
            let _v : 'tv_option_type_expr_ = 
# 114 "<standard.mly>"
    ( None )
# 3067 "parser.ml"
             in
            _menhir_goto_option_type_expr_ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv278)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv280)
    | _ ->
        _menhir_fail ()

and _menhir_goto_type_expr : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_type_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv237 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_ty_, _, (ty : 'tv_type_expr)) = _menhir_stack in
        let _endpos = _endpos_ty_ in
        let _v : 'tv_type_expr = 
# 98 "parser.mly"
   ( PTptr ty )
# 3091 "parser.ml"
         in
        _menhir_goto_type_expr _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv236)) : 'freshtv238)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv255 * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_ids_, _menhir_s, (ids : 'tv_separated_nonempty_list_COMMA_ident_)), _endpos_ty_, _, (ty : 'tv_type_expr)) = _menhir_stack in
        let _v : 'tv_ids_and_type = 
# 81 "parser.mly"
   ( List.map (fun x -> x, ty) ids )
# 3103 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ids_and_type) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState23 | MenhirState13 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHTBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv239 * Lexing.position) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _endpos_x_) = _menhir_stack in
                    let x = () in
                    let _v : 'tv_option_SEMICOLON_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3135 "parser.ml"
                     in
                    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _v) : 'freshtv240)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv242)
            | RIGHTBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv243) = Obj.magic _menhir_stack in
                ((let _v : 'tv_option_SEMICOLON_ = 
# 114 "<standard.mly>"
    ( None )
# 3148 "parser.ml"
                 in
                _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _v) : 'freshtv244)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
        | MenhirState144 | MenhirState32 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState143 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHTPAR ->
                    _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144) : 'freshtv250)
            | RIGHTPAR ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143) : 'freshtv252)
        | _ ->
            _menhir_fail ()) : 'freshtv254)) : 'freshtv256)) : 'freshtv258)
    | MenhirState38 | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv259 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState37 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RIGHTPAR ->
                _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv260)
        | RIGHTPAR ->
            _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv262)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv265 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv263 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_ty_, _menhir_s, (ty : 'tv_type_expr)) = _menhir_stack in
        let _v : 'tv_return_type = 
# 85 "parser.mly"
                                               ( [ty] )
# 3227 "parser.ml"
         in
        _menhir_goto_return_type _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)) : 'freshtv266)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv269 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_type_expr)) = _menhir_stack in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_type_expr_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3240 "parser.ml"
         in
        _menhir_goto_option_type_expr_ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv268)) : 'freshtv270)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_fields_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_fields_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv233 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position) * _menhir_state * 'tv_loption_fields_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHTBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv229 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position) * _menhir_state * 'tv_loption_fields_) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv225 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position) * _menhir_state * 'tv_loption_fields_) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv223 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position) * _menhir_state * 'tv_loption_fields_) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__7_ : Lexing.position) = _endpos in
            ((let (((((_menhir_stack, _menhir_s), _endpos_id_, _, (id : 'tv_ident), _startpos_id_), _startpos__4_), _, (fl : 'tv_loption_fields_)), _endpos__6_) = _menhir_stack in
            let _v : 'tv_decl = 
# 67 "parser.mly"
  ( PDstruct { ps_name = id; ps_fields = List.flatten fl; } )
# 3274 "parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv227 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position) * _menhir_state * 'tv_loption_fields_) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv231 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position) * _menhir_state * 'tv_loption_fields_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)) : 'freshtv234)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_ids_and_type) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * 'tv_ids_and_type) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * 'tv_stmt) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * Lexing.position * _menhir_state * 'tv_exprs * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv101 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_opt_simple_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv103 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv105 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * Lexing.position * _menhir_state * Lexing.position) * _menhir_state * 'tv_opt_simple_stmt) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv111 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_block)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv133 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv137 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * (
# 21 "parser.mly"
       (Ast.binop)
# 3430 "parser.ml"
        )) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * (
# 21 "parser.mly"
       (Ast.binop)
# 3439 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv141 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv159 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv165 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv186)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv193 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) * Lexing.position) * _menhir_state * 'tv_loption_return_type_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv195 * Lexing.position * _menhir_state * 'tv_type_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * Lexing.position * _menhir_state * 'tv_type_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv201 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_parameters_) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv203 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv209 * _menhir_state * 'tv_ids_and_type) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * Lexing.position * _menhir_state * 'tv_separated_nonempty_list_COMMA_ident_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv215 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv219) * _menhir_state * 'tv_ident_main) * Lexing.position) * 'tv_boption_import_fmt_) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv222)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 10 "parser.mly"
       (string)
# 3650 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv83) = Obj.magic _menhir_stack in
    let (_endpos_id_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((id : (
# 10 "parser.mly"
       (string)
# 3661 "parser.ml"
    )) : (
# 10 "parser.mly"
       (string)
# 3665 "parser.ml"
    )) = _v in
    let (_startpos_id_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_id_ in
    let _endpos = _endpos_id_ in
    let _v : 'tv_ident = let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    
# 246 "parser.mly"
             ( { loc = _startpos, _endpos; id = id } )
# 3675 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv81) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_ident) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRUCT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFTBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv9 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) = Obj.magic _menhir_stack in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHTBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState13 in
                    ((let _v : 'tv_loption_fields_ = 
# 142 "<standard.mly>"
    ( [] )
# 3714 "parser.ml"
                     in
                    _menhir_goto_loption_fields_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv10)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv11 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)) : 'freshtv14)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv15 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)) : 'freshtv18)
    | MenhirState35 | MenhirState48 | MenhirState36 | MenhirState38 | MenhirState14 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_id_, _menhir_s, (id : 'tv_ident), _startpos_id_) = _menhir_stack in
        let _endpos = _endpos_id_ in
        let _v : 'tv_type_expr = 
# 96 "parser.mly"
   ( PTident id )
# 3745 "parser.ml"
         in
        _menhir_goto_type_expr _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv20)) : 'freshtv22)
    | MenhirState32 | MenhirState144 | MenhirState47 | MenhirState13 | MenhirState25 | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv23 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv24)
        | EQ | IDENT _ | RIGHTBRACE | SEMICOLON | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv25 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_ident), _startpos_x_) = _menhir_stack in
            let _endpos = _endpos_x_ in
            let _v : 'tv_separated_nonempty_list_COMMA_ident_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3774 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv26)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv27 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)) : 'freshtv30)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv33 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RIGHTPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv31) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState32 in
                ((let _v : 'tv_loption_parameters_ = 
# 142 "<standard.mly>"
    ( [] )
# 3808 "parser.ml"
                 in
                _menhir_goto_loption_parameters_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv32)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv34)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv35 * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)) : 'freshtv38)
    | MenhirState46 | MenhirState99 | MenhirState132 | MenhirState109 | MenhirState121 | MenhirState119 | MenhirState115 | MenhirState113 | MenhirState102 | MenhirState100 | MenhirState51 | MenhirState53 | MenhirState55 | MenhirState56 | MenhirState58 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState69 | MenhirState67 | MenhirState61 | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTPAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | DOT | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv39 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_id_, _menhir_s, (id : 'tv_ident), _startpos_id_) = _menhir_stack in
            let _startpos = _startpos_id_ in
            let _endpos = _endpos_id_ in
            let _v : 'tv_expr_desc = 
# 208 "parser.mly"
   ( PEident id )
# 3839 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv42)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTPAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPERSANDAMPERSAND | COLONEQ | COMMA | COMP _ | DOT | EQ | LEFTBRACE | MINUS | MINUSMINUS | PERCENT | PLUS | PLUSPLUS | RIGHTBRACE | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv43 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expr), _startpos_e_), _), _endpos_id_, _, (id : 'tv_ident), _startpos_id_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_id_ in
            let _v : 'tv_expr_desc = 
# 210 "parser.mly"
   ( PEdot (e, id) )
# 3863 "parser.ml"
             in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv44)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv46)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * Lexing.position * _menhir_state * 'tv_ident * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_id_, _menhir_s, (id : 'tv_ident), _startpos_id_) = _menhir_stack in
        let _v : 'tv_ident_main = 
# 51 "parser.mly"
             ( if id.id <> "main" then raise Error )
# 3879 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ident_main) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73) * _menhir_state * 'tv_ident_main) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv69) * _menhir_state * 'tv_ident_main) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IMPORT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv63) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | STRING _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv59) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_v : (
# 11 "parser.mly"
       (string)
# 3912 "parser.ml"
                    )) = _v in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | SEMICOLON ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ('freshtv55) * Lexing.position * (
# 11 "parser.mly"
       (string)
# 3924 "parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ('freshtv53) * Lexing.position * (
# 11 "parser.mly"
       (string)
# 3932 "parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        let (_endpos__3_ : Lexing.position) = _endpos in
                        ((let (_menhir_stack, _endpos_s_, (s : (
# 11 "parser.mly"
       (string)
# 3938 "parser.ml"
                        )), _startpos_s_) = _menhir_stack in
                        let _v : 'tv_import_fmt = 
# 55 "parser.mly"
                            ( if s <> "fmt" then raise Error )
# 3943 "parser.ml"
                         in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv51) = _menhir_stack in
                        let (_v : 'tv_import_fmt) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
                        let (_v : 'tv_import_fmt) = _v in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
                        let (_ : 'tv_import_fmt) = _v in
                        ((let _v : 'tv_boption_import_fmt_ = 
# 135 "<standard.mly>"
    ( true )
# 3957 "parser.ml"
                         in
                        _menhir_goto_boption_import_fmt_ _menhir_env _menhir_stack _v) : 'freshtv48)) : 'freshtv50)) : 'freshtv52)) : 'freshtv54)) : 'freshtv56)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ('freshtv57) * Lexing.position * (
# 11 "parser.mly"
       (string)
# 3967 "parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        (raise _eRR : 'freshtv58)) : 'freshtv60)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv61) = Obj.magic _menhir_stack in
                    (raise _eRR : 'freshtv62)) : 'freshtv64)
            | EOF | FUNC | TYPE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv65) = Obj.magic _menhir_stack in
                ((let _v : 'tv_boption_import_fmt_ = 
# 133 "<standard.mly>"
    ( false )
# 3982 "parser.ml"
                 in
                _menhir_goto_boption_import_fmt_ _menhir_env _menhir_stack _v) : 'freshtv66)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv67) * _menhir_state * 'tv_ident_main) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)) : 'freshtv70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv71) * _menhir_state * 'tv_ident_main) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)) : 'freshtv76)) : 'freshtv78)) : 'freshtv80)
    | _ ->
        _menhir_fail ()) : 'freshtv82)) : 'freshtv84)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 39 "parser.mly"
      (Ast.pfile)
# 4017 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PACKAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv4)) : 'freshtv6))

# 269 "<standard.mly>"
  

# 4053 "parser.ml"
