
module P = Reason_toolchain.RE
module Util = Reason_syntax_util

module OMP = Reason_migrate_parsetree

(* TODO : remove? *)
let init_lexer = ()

let wrap ~post parsing_fun lexbuf =
  parsing_fun lexbuf |> post

module Conv = OMP.Convert(OMP.OCaml_408)(OMP.OCaml_current)

let expr lexbuf =
  wrap P.expression lexbuf ~post:(fun x ->
    x
    |> Conv.copy_expression)

let toplevel_phrase lexbuf =
  wrap P.toplevel_phrase lexbuf ~post:(fun x ->
    x
    |> Conv.copy_toplevel_phrase)

let use_file lexbuf =
  wrap P.use_file lexbuf ~post:(fun x ->
    x
    |> List.map Conv.copy_toplevel_phrase)

let implementation lexbuf =
  wrap P.implementation lexbuf ~post:(fun x ->
    x
    |> Conv.copy_structure)

(*
let report_exn out (e:exn): bool =
  match e with
    | Util.Error (loc, Util.Syntax_error msg) ->
      Format.fprintf out "%a@\n@{<Red>Reason syntax error@}:@ %s@." Location.print_loc loc msg;
      true
    | _ -> false
   *)
