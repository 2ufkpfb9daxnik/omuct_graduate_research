open Effect
open Effect.Deep

(* 文字列を要求するAskエフェクトを定義 *)
type _ Effect.t += Ask : string Effect.t

(* エフェクトを発生させる関数 *)
let perform_ask () =
  Printf.printf "1. perform Askを実行...\n";
  let msg = perform Ask in (* ここで処理が一時停止し、ハンドラへ飛ぶ *)
  Printf.printf "4. ハンドラから値を受け取りました: %s\n" msg

(* メインの処理(ハンドラで囲む) *)
let () =
  Printf.printf "--- エフェクトハンドラのテスト開始 ---\n";

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
    match eff with
    | Ask -> Some (fun (k : (a, _) continuation) ->
        Printf.printf "2. ハンドラがAskエフェクトを捕捉\n";
        Printf.printf "3. 継続kをよす微出して処理を再開させる\n";
        continue k "Hello from Handler!") (*ここでperformの場所へもどる*)
    | _ -> None
  }