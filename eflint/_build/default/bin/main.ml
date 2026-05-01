open Typedtree
open Tast_iterator

(* cmtファイルからTypedtreeを抽出する*)
let get_typed_ast_from_cmt filename =
  let cmt_info = Cmt_format.read_cmt filename in
  match cmt_info.cmt_annots with
  | Implementation typedtree -> Some typedtree
  | _ -> None

let my_visitor = {
  default_iterator with
  expr = fun sub e ->
    Printf.printf "Found expression: %s\n"
    (match e.exp_desc with
    | Texp_apply _ -> "function application"
    | Texp_ident _ -> "identifier"
    | _ -> "other");
  default_iterator.expr sub e
}

let analyze_structure typedtree =
  my_visitor.structure my_visitor typedtree

let () =
  (*test.mlではなくてコンパイル後のtest.cmtを読み込む*)
  let cmt_filename = "input/test.cmt" in
  match get_typed_ast_from_cmt cmt_filename with
  | Some ast ->
    Printf.printf "AST successfully obtained from CMT\n";
    analyze_structure ast
  | None ->
    Printf.printf "Failed to load or not an implementation file\n"