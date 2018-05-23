
module Rw = Imandra_lib.Rewrite_parsetree
module L = Reason_lexer

let rec skip_phrase lexbuf =
  try
    match L.token lexbuf with
    | Reason_parser.SEMI | Reason_parser.SEMISEMI | Reason_parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
  | L.Error (L.Unterminated_comment _, _)
  | L.Error (L.Unterminated_string, _)
  | L.Error (L.Unterminated_string_in_comment _, _)
  | L.Error (L.Illegal_character _, _) -> skip_phrase lexbuf

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Reason_parser.SEMI
  || Parsing.is_current_lookahead Reason_parser.SEMISEMI
  || Parsing.is_current_lookahead Reason_parser.EOF
  then ()
  else skip_phrase lexbuf

let wrap_internal lex_fun parsing_fun lexbuf =
  try
    Docstrings.init ();
    let ast = parsing_fun lex_fun lexbuf in
    Parsing.clear_parser();
    Docstrings.warn_bad_docstrings ();
    ast
  with
  | Reason_lexer.Error(Reason_lexer.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//"->
    skip_phrase lexbuf;
    raise err
  | Syntaxerr.Error _ as err
    when !Location.input_name = "//toplevel//" ->
    maybe_skip_phrase lexbuf;
    raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
    let loc = Location.curr lexbuf in
    if !Location.input_name = "//toplevel//"
    then maybe_skip_phrase lexbuf;
    raise(Syntaxerr.Error(Syntaxerr.Other loc))

let wrap ~post parsing_fun lexbuf =
  wrap_internal Reason_lexer.token parsing_fun lexbuf |> post

let toplevel_phrase lexbuf =
  wrap Reason_parser.toplevel_phrase lexbuf ~post:(fun x ->
    x
    |> Migrate_parsetree_404_403_migrate.copy_toplevel_phrase
    |> Rewrite_parsetree.toplevel_phrase)

let use_file lexbuf =
  wrap Reason_parser.use_file lexbuf ~post:(fun x ->
    x
    |> List.map Migrate_parsetree_404_403_migrate.copy_toplevel_phrase
    |> List.map Rewrite_parsetree.toplevel_phrase)

let implementation lexbuf =
  wrap Reason_parser.implementation lexbuf ~post:(fun x ->
    x
    |> Migrate_parsetree_404_403_migrate.copy_structure
    |> Rewrite_parsetree.structure)

let init () =
  let open Imandra_lib.Syntax.Raw in
  let plugin = { implementation; use_file; toplevel_phrase}  in
  register_reason plugin
