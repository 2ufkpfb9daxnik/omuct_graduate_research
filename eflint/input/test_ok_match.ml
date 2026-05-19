open Effect
open Effect.Deep

(* 天気を要求するエフェクト *)
type _ Effect.t += Ask_weather : string Effect.t

(* エフェクトを発生させる関数 *)
let perform_ask () =
  Printf.printf "エフェクトを発生\n";
  let result = perform Ask_weather in
  Printf.printf "ハンドラから受け取った結果: %s\n" result

let () =
  (* 分岐のための条件変数 *)
  let weather = "Sunny" in

  Printf.printf "--- match文のテスト開始 ---\n";
  
  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_weather -> Some (fun (k : (a, _) continuation) ->
          
          (* match文で3つのルートに分岐。どのルートを通っても継続は確実に1回だけ呼ばれる *)
          match weather with
          | "Sunny" -> 
              Printf.printf "晴れ\n";
              continue k "Go outside"  (* AST上の出現 1回目 *)
          | "Rainy" -> 
              Printf.printf "雨\n";
              continue k "Stay home"   (* AST上の出現 2回目 *)
          | _ -> 
              Printf.printf "その他\n";
              continue k "Sleep"       (* AST上の出現 3回目 *)

        )
      | _ -> None
  }