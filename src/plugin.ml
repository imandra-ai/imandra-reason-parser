
module L = Reason_lexer
module P = Reason_toolchain.Reason_syntax

let init_lexer = lazy (
  L.init ();
)

let wrap_internal parsing_fun lexbuf =
  Lazy.force init_lexer;
  Location.init lexbuf "toplevel"; (* remove smartass printing *)
  try
    Docstrings.init ();
    let ast = parsing_fun lexbuf in
    Parsing.clear_parser();
    Docstrings.warn_bad_docstrings ();
    ast
  with
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
    let loc = Location.curr lexbuf in
    raise(Syntaxerr.Error(Syntaxerr.Other loc))

let wrap ~post parsing_fun lexbuf =
  wrap_internal parsing_fun lexbuf |> post

let toplevel_phrase lexbuf =
  wrap P.toplevel_phrase lexbuf ~post:(fun x ->
    x
    |> Migrate_parsetree_404_403_migrate.copy_toplevel_phrase)

let use_file lexbuf =
  wrap P.use_file lexbuf ~post:(fun x ->
    x
    |> List.map Migrate_parsetree_404_403_migrate.copy_toplevel_phrase)

let implementation lexbuf =
  wrap P.implementation lexbuf ~post:(fun x ->
    x
    |> Migrate_parsetree_404_403_migrate.copy_structure)
