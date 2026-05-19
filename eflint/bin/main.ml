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
  | Consumed of int list (* 1回消費された (引数は消費された行番号のリスト)*)
  | Double of int list (* 2回以上消費された (エラー) (引数は消費された行番号のリスト)*)
  | Missing of int list (* 分岐による未再開のリスク  (引数は未再開の行番号のリスト)*)

(* 2つの分岐ルートの結果を結合 (if) *)
let merge_state e s1 s2 =
  match s1, s2 with
  | Consumed l1, Consumed l2 -> Consumed (l1 @ l2)
  | Unused, Unused -> Unused
  
  (* Double は最強のエラーなのでそのまま伝播 *)
  | Double lines, _ | _, Double lines -> Double lines
  
  (* Missing状態の伝播 *)
  | Missing l1, Missing l2 -> Missing (l1 @ l2)
  | Missing l, Consumed _ | Consumed _, Missing l -> Missing l
  | Missing l, Unused | Unused, Missing l -> Missing l
  
  (* 片方だけ消費された場合、厳格にMissing(エラー)に遷移させる *)
  | Consumed l, Unused | Unused, Consumed l ->
      Missing l

(* N個の分岐ルートの結果を結合 (match) *)
let merge_state_list e states =
  match states with
  | [] -> Unused
  | hd :: tl -> List.fold_left (merge_state e) hd tl

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

        (* continue が呼ばれた行番号*)
        let line = e.exp_loc.loc_start.pos_lnum in

        (* 状態を更新する *)
        begin match !current_k_state with
        | Unused -> current_k_state := Consumed [line]
        | Consumed lines -> current_k_state := Double (lines @ [line])
        | Double lines -> current_k_state := Double (lines @ [line])
        | Missing lines -> current_k_state := Double (lines @ [line])
        end;
        default_iterator.expr sub e

      | _ -> default_iterator.expr sub e
      end

    (* if式 *)
    | Texp_ifthenelse (cond_expr, then_expr, else_expr_opt) ->
      (* 条件式を巡回 *)
      sub.expr sub cond_expr;
      
      (*もし条件式を評価した時点で、すでに2回以上呼び出し(Double)になっていたら終了*)
      if match !current_k_state with Double _ -> true | _ -> false then ()

      (* もし条件式の中で、すでに1回消費されていた場合 *)
      else if match !current_k_state with Consumed _ -> true | _ -> false then begin
        (* 条件式の中で消費されたなら、then/elseのどちらに進んでも消費済みなので、中身を巡回するが、分岐による消費漏れ(Missing)が入ることはないので、最終的に条件式が変わった時点でのConsumed状態を維持させる *)
        let state_after_cond = !current_k_state in

        (* then側を確認 *)
        sub.expr sub then_expr;
        let state_after_then = !current_k_state in
        
        (* 状態を戻してelse側を確認 *)
        current_k_state := state_after_cond;
        (match else_expr_opt with Some else_expr -> sub.expr sub else_expr | None -> ());
        let state_after_else = !current_k_state in

        (* もしどちらかのルートでさらにcontinueが呼ばれてDoubleになっていたら、Doubleを優先する *)
        if match state_after_then with Double _ -> true | _ -> false ||
           match state_after_else with Double _ -> true | _ -> false then
          current_k_state := Double (match state_after_then, state_after_else with Double l, _ | _, Double l -> l | _ -> [])
        else
          current_k_state := state_after_cond
      end

      (* 条件式の中ではまだ継続が消費されていない場合 *)
      else begin
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
      current_k_state := merge_state e state_then state_else
    end

    (* match 式 *)
    | Texp_match (match_expr, cases, exn_cases, _partial) ->
      (* マッチ対象の式を巡回 *)
      sub.expr sub match_expr;

      (* 分岐にはいる前の状態を保存 *)
      let state_before = !current_k_state in

      (* 各ケースを巡回して最終状態のリストを作る *)
      let analyze_cases case_list =
        List.filter_map (fun c ->
          current_k_state := state_before;
          sub.case sub c;

          (* エフェクトハンドラの _ -> None のように、明示的にNoneを返すルートは処理の放棄であるため、消費漏れの計算から除外する *)
          match c.c_rhs.exp_desc with
          | Texp_construct (_, {cstr_name="None"; _}, _) -> None
          | _ -> Some !current_k_state
        ) case_list
      in

      (* 通常のcaseと、例外ハンドラ(exception)のcaseをそれぞれ解析 *)
      let states_from_cases = analyze_cases cases in
      let states_from_exns = analyze_cases exn_cases in

      (* すべての分岐の結果を結合 *)
      let all_states = states_from_cases @ states_from_exns in

      if all_states = [] then
        current_k_state := state_before (* 分岐が0個なら状態はそのまま *)
      else
        current_k_state := merge_state_list e all_states

    (* while *)
    | Texp_while (cond_expr, body_expr) ->
      (* 条件式を巡回 *)
      sub.expr sub cond_expr;

      (* ループに入る前の状態を保存 *)
      let state_before = !current_k_state in

      (* ループの内部を巡回 *)
      sub.expr sub body_expr;

      (* 状態が変化したか(内部でcontinueが呼ばれたか)を確認 *)
      if state_before <> !current_k_state then begin
        Printf.printf "[Warning] 行 %d: whileループ内部で継続が消費されています(0回または複数回実行される可能性があります)\n" e.exp_loc.loc_start.pos_lnum;

        (* 内部で呼ばれていた場合、エラー(Double)に遷移させる *)
        let lines = match !current_k_state with
          | Unused -> []
          | Consumed l | Double l | Missing l -> l
      in
      current_k_state := Double lines
    end

    (* for *)
    | Texp_for (_, _, start_expr, end_expr, _, body_expr) ->
      (* forの開始値と終了値を巡回 *)
      sub.expr sub start_expr;
      sub.expr sub end_expr;
      
      (* ループに入る前の状態を保存 *)
      let state_before = !current_k_state in

      (* ループの内部を巡回 *)
      sub.expr sub body_expr;

      (* 状態が変化したか(内部でcontinueが呼ばれたか)を確認 *)
      if state_before <> !current_k_state then begin
        Printf.printf "[Warning] 行 %d: forループ内部で継続が消費されています(0回または複数回実行される可能性があります)\n" e.exp_loc.loc_start.pos_lnum;

        (* 内部で呼ばれていた場合、エラー(Double)に遷移させる *)
        let lines = match !current_k_state with
          | Unused -> []
          | Consumed l | Double l | Missing l -> l
      in
      current_k_state := Double lines
    end

    (* その他の式 *)
    | _ -> default_iterator.expr sub e
    end
}

(* メインの処理 *)
let () =
  let cmt_filename = "input/test_err_or.cmt" in
  match get_typed_ast_from_cmt cmt_filename with
  | Some ast ->
    Printf.printf "ファイル名 %s でlint開始\n" cmt_filename;
    handler_depth := 0;
    current_k_state := Unused;
    
    my_linter.structure my_linter ast;

    Printf.printf "lint終了\n";
  
    begin match !current_k_state with
    | Unused -> 
        Printf.printf "[Error] 継続 k が一度も再開されていません\n"
    | Consumed lines -> 
        let lines_str = String.concat ", " (List.map string_of_int lines) in
        Printf.printf "[OK] 継続 k は正しく1回だけ再開されています (消費されている行: %s)\n" lines_str
    | Double lines -> 
        let lines_str = String.concat ", " (List.map string_of_int lines) in
        Printf.printf "[Error] 継続 k が同一パス上で複数回再開されるリスクがあります (発生行: %s)\n" lines_str
    | Missing lines -> 
        let lines_str = String.concat ", " (List.map string_of_int lines) in
        Printf.printf "[Error] 分岐（if/match/論理演算）によって継続が消費されないルートが存在します (行: %s)\n" lines_str
    end
  
  | None ->
    Printf.printf "ファイルの読み込みに失敗しました\n"
