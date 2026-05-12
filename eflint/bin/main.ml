open Typedtree
open Tast_iterator

(* ==========================================
   ヘルパー関数
   ========================================== *)

(* .cmtファイルから Typedtree.structure を抽出する関数 *)
let get_typed_ast_from_cmt filename =
  let cmt_info = Cmt_format.read_cmt filename in
  match cmt_info.cmt_annots with
  | Implementation typedtree -> Some typedtree
  | _ -> None

(* 式(expression)が識別子(Texp_ident)なら、そのパス名を文字列で返す関数 *)
let get_path_name (e : expression) : string option =
  match e.exp_desc with
  | Texp_ident (path, _, _) -> Some (Path.name path)
  | _ -> None


(* ==========================================
   Linterの状態管理（状態変数）
   ========================================== *)

(* [Warningレベル用] 現在いくつのハンドラにネストして囲まれているか *)
let handler_depth = ref 0

(* [Errorレベル用] 継続の呼び出し回数をカウントする *)
(* ※今回は簡易版として、ファイル全体での continue の呼び出し回数を数えます *)
let continue_call_count = ref 0


(* ==========================================
   Linterのメインエンジン（カスタムイテレータ）
   ========================================== *)

let my_linter = {
  (* default_iterator をベースにして、一部の処理だけを上書き（オーバーライド）する *)
  default_iterator with

  (* 式(expression)を巡回する際の処理を上書き *)
  expr = fun sub e ->
    begin match e.exp_desc with
    (* もし今見ている式が「関数呼び出し(Texp_apply)」だった場合... *)
    | Texp_apply (func_expr, _args) ->
        
        (* その関数の名前をチェックする *)
        begin match get_path_name func_expr with
        
        (* === [Warning] performの呼び出しを検知 === *)
        | Some "Stdlib.Effect.perform" ->
            (* もしハンドラの外側(depth=0)で呼ばれていたら警告を出す *)
            if !handler_depth = 0 then begin
              Printf.printf "[Warning] 行 %d: ハンドラの外側で perform が呼ばれています！\n" 
                e.exp_loc.loc_start.pos_lnum
            end;
            (* その後、通常通り子ノードの巡回を続ける *)
            default_iterator.expr sub e
            
        (* === [Context] ハンドラの生成を検知 === *)
        | Some "Stdlib.Effect.Deep.try_with" 
        | Some "Stdlib.Effect.Deep.match_with" ->
            (* ハンドラの中に入るので深さを+1する *)
            incr handler_depth;
            (* ハンドラの中身（子ノード）を巡回させる *)
            default_iterator.expr sub e; 
            (* ハンドラから出るので深さを-1する *)
            decr handler_depth
            
        (* === [Error] continueの呼び出しを検知 === *)
        | Some "Stdlib.Effect.Deep.continue" ->
            (* continue が呼ばれた回数をカウントアップする *)
            incr continue_call_count;
            (* その後、通常通り子ノードの巡回を続ける *)
            default_iterator.expr sub e
            
        (* 上記のいずれの関数でもなかった場合は、通常通り子ノードを巡回する *)
        | _ -> 
            default_iterator.expr sub e
        end
        
    (* 関数呼び出し(Texp_apply)以外のすべての式の場合は、通常通り子ノードを巡回する *)
    (* 【修正ポイント1】このワイルドカードが抜けていたため、網羅性エラー(warning 8)が出ていました *)
    | _ -> 
        default_iterator.expr sub e
    end
}


(* ==========================================
   メイン処理
   ========================================== *)

let () =
  let cmt_filename = "input/test.cmt" in
  match get_typed_ast_from_cmt cmt_filename with
  | Some ast ->
      Printf.printf "=== Linter 解析開始 ===\n";
      
      (* カウンタを初期化 *)
      handler_depth := 0;
      continue_call_count := 0;
      
      (* 自作した Linter を AST 全体に走らせる *)
      my_linter.structure my_linter ast;
      
      (* 解析結果（Errorレベル）の集計と出力 *)
      (* ※現段階では簡易的に「1回以外ならエラー」としています *)
      Printf.printf "=== 解析終了 ===\n";
      Printf.printf "continue の呼び出し回数: %d\n" !continue_call_count;
      if !continue_call_count = 0 then
        Printf.printf "[Error] 継続 k が再開されていません (未再開のリスク)\n"
      else if !continue_call_count > 1 then
        Printf.printf "[Error] 継続 k が複数回再開されています (二重再開のリスク)\n"
      else
        Printf.printf "[OK] 継続 k は正しく1回だけ再開されています\n"
        
  | None ->
      Printf.printf "ファイルの読み込みに失敗しました、または正しいcmtファイルではありません。\n"