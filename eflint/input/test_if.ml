open Effect
open Effect.Deep

(* 文字列を要求するエフェクト *)
type _ Effect.t += Ask_day : string Effect.t

(* エフェクトを発生させる関数 *)
let perform_ask () =
  Printf.printf "エフェクトを発生\n";
  let result = perform Ask_day in
  Printf.printf "ハンドラから受け取った結果: %s\n" result

let () =
  (* 分岐のための条件変数 *)
  let today = "Sunday" in

  Printf.printf "テスト\n";
  
  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_day -> Some (fun (k : (a, _) continuation) ->
          
          (* 実行されるルートは必ずどちらか片方なので、継続は1回しか呼ばれない安全なコード *)
          if today = "Sunday" then begin
            Printf.printf "休日\n";
            continue k "Holiday"  (* AST上の出現 1回目 *)
          end else begin
            Printf.printf "平日\n";
            continue k "Workday"  (* AST上の出現 2回目 *)
          end

        )
      | _ -> None
  }