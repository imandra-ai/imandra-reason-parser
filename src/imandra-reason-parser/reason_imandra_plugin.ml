
module P = Reason_toolchain.RE

module OMP = Reason_migrate_parsetree

let wrap ~post parsing_fun lexbuf =
  try parsing_fun lexbuf |> post
  with
  | (End_of_file | Sys.Break | Unix.Unix_error _) as e -> raise e
  | Parsing.Parse_error ->
    let loc = Location.curr lexbuf in
    raise (Syntaxerr.Error (Syntaxerr.Other loc))
  | Syntaxerr.Error _ as e -> raise e
  | _e ->
    let loc = Location.curr lexbuf in
    raise (Syntaxerr.Error (Syntaxerr.Other loc))
(*     raise (Error (Format.asprintf "error in reason:\n%s" (Printexc.to_string e))) *)

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
