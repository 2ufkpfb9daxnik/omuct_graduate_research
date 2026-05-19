open Effect
open Effect.Deep

type _ Effect.t += Ask_err_while : unit Effect.t

let perform_ask () = perform Ask_err_while

let () =
  Printf.printf "--- ERR While Loop Test ---\n";

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_err_while -> Some (fun (k : (a, _) continuation) ->
          
          let count = ref 0 in
          (* 【異常】ループ内で呼ばれる *)
          (* 条件次第で0回（未再開）、または複数回（二重再開）になる *)
          while !count < 2 do
            incr count;
            continue k ()
          done

        )
      | _ -> None
  }