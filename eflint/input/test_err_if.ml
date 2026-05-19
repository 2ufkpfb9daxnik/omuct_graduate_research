open Effect
open Effect.Deep

type _ Effect.t += Ask_day : string Effect.t

let perform_ask () =
  let result = perform Ask_day in
  Printf.printf "結果: %s\n" result

let () =
  let today = "Sunday" in

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_day -> Some (fun (k : (a, _) continuation) ->
          
          if today = "Sunday" then begin
            Printf.printf "休日のルートに入りました。\n";
            continue k "Holiday";
            (* 【異常】ここで誤って2回目を呼び出してしまう！ *)
            continue k "Holiday_Again"
          end else begin
            Printf.printf "平日のルートに入りました。\n";
            continue k "Workday"
          end

        )
      | _ -> None
  }