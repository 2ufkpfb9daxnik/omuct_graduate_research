open Typedtree
open Tast_iterator

(*=== 補助関数 状態定義 ===*)

(* CMTファイルから型付きASTを取得する *)
let get_typed_ast_from_cmt filename =
  let cmt_info = Cmt_format.read_cmt filename in
  match cmt_info.cmt_annots with
  | Implementation typedtree -> Some typedtree
  | _ -> None

(* 式からパス名を取得する *)
let get_path_name (e : expression) : string option =
  match e.exp_desc with
  | Texp_ident (path, _, _) -> Some (Path.name path)
  | _ -> None

(* 継続の状態を表す *)
type k_state =
  | Unused (* 継続未使用 *)
  | Consumed (* 1回消費された*)
  | Double (* 2回以上消費された (エラー) *)

(* 2つの分岐ルートの結果を結合 *)
let merge_state s1 s2 =
  match s1, s2 with
  | Consumed, Consumed -> Consumed
  | Unused, Unused -> Unused
  | Double, _ | _, Double -> Double
  | Consumed, Unused | Unused, Consumed ->
    (* 片方のルートでしか消費されていない場合は、消費漏れのリスクあり *)
    Printf.printf "[Warning] 分岐によって継続の消費漏れが発生する可能性があります\n";
    Consumed (* 後続の解析のために一旦Consumed扱いにするかは調整可能 *)

(*=== Linterの状態 ===*)
let handler_depth = ref 0 (* ハンドラの深さ *)
let current_k_state = ref Unused (* 現在の継続の状態 *)

(*=== Linter ===*)
let my_linter = {
  default_iterator with

  expr = fun sub e ->
    begin match e.exp_desc with

    (* 関数呼び出しの解析 *)
    | Texp_apply (func_expr, _args) ->
      begin match get_path_name func_expr with
      | Some "Stdlib.Effect.perform" ->
        if !handler_depth = 0 then
          Printf.printf "[Warning] 行 %d: ハンドラの外側でperformが呼ばれています\n" e.exp_loc.loc_start.pos_lnum;
        default_iterator.expr sub e
        
      | Some "Stdlib.Effect.Deep.try_with"
      | Some "Stdlib.Effect.Deep.match_with" ->
        incr handler_depth;
        default_iterator.expr sub e;
        decr handler_depth;
      
      | Some "Stdlib.Effect.Deep.continue" ->
        (* 状態を更新する *)
        begin match !current_k_state with
        | Unused -> current_k_state := Consumed
        | Consumed | Double -> current_k_state := Double
        end;
        default_iterator.expr sub e
      
      | _ -> default_iterator.expr sub e
      end

    (* if式*)
    | Texp_ifthenelse (cond_expr, then_expr, else_expr_opt) ->
      (* 条件式を巡回 *)
      sub.expr sub cond_expr;

      (* 分岐に入る前の状態を保存 *)
      let state_before = !current_k_state in

      (* then側を巡回し、結果を保存 *)
      sub.expr sub then_expr;
      let state_then = !current_k_state in

      (* 状態を分岐前に戻す *)
      current_k_state := state_before;

      (* else側を巡回し、結果を保存 *)
      let state_else =
        match else_expr_opt with
        | Some else_expr ->
          sub.expr sub else_expr;
          !current_k_state
        | None ->
          state_before (* elseがなかったら、何もしなかったのと同じ状態 *)
      in

      (* 分岐の結果を結合 *)
      current_k_state := merge_state state_then state_else

    (* その他の式 *)
    | _ -> default_iterator.expr sub e
    end
}

(* メインの処理 *)
let () =
  let cmt_filename = "input/test_if.cmt" in
  match get_typed_ast_from_cmt cmt_filename with
  | Some ast ->
    Printf.printf "lint開始\n";
    handler_depth := 0;
    current_k_state := Unused;
    
    my_linter.structure my_linter ast;

    Printf.printf "lint終了\n";
    begin match !current_k_state with
    | Unused -> Printf.printf "[Error] 継続 k が再開されていません\n"
    | Consumed -> Printf.printf "[OK] 継続 k は正しく1回だけ再開されています\n"
    | Double -> Printf.printf "[Error] 継続 k が複数回再開されています\n"
    end
  
  | None ->
    Printf.printf "ファイルの読み込みに失敗しました\n"
