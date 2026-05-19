open Effect
open Effect.Deep

type _ Effect.t += Ask_err_for : unit Effect.t

let perform_ask () = perform Ask_err_for

let () =
  Printf.printf "--- ERR For Loop Test ---\n";

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_err_for -> Some (fun (k : (a, _) continuation) ->
          
          (* 【異常】ループ内で呼ばれる *)
          (* 確実に3回呼ばれてクラッシュする *)
          for i = 1 to 3 do
            continue k ()
          done

        )
      | _ -> None
  }