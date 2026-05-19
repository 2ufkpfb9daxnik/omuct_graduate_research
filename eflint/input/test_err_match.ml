open Effect
open Effect.Deep

type _ Effect.t += Ask_weather : string Effect.t

let perform_ask () =
  let result = perform Ask_weather in
  Printf.printf "結果: %s\n" result

let () =
  let weather = "Rainy" in

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_weather -> Some (fun (k : (a, _) continuation) ->
          
          match weather with
          | "Sunny" -> 
              continue k "Go outside"
          | "Rainy" -> 
              (* 【異常】処理だけして、continue k を呼び忘れている！ *)
              Printf.printf "雨なので何もしません...\n"
          | _ -> 
            (* 【異常】処理だけして、continue k を2回呼び出している！ *)
              continue k "Sleep";
              continue k "Go outside"

        )
      | _ -> None
  }