
module L = Reason_lexer
module P = Reason_toolchain.RE
module Util = Reason_syntax_util

let init_lexer = lazy (
  L.init ();
)

let wrap ~post parsing_fun lexbuf =
  parsing_fun lexbuf |> post

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

let report_exn out (e:exn): bool =
  match e with
    | Util.Error (loc, Util.Syntax_error msg) ->
      Format.fprintf out "%a@\n@{<Red>Reason syntax error@}:@ %s@." Location.print_loc loc msg;
      true
    | _ -> false
