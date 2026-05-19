open Effect
open Effect.Deep

type _ Effect.t += Ask_ok_for : unit Effect.t

let perform_ask () = perform Ask_ok_for

let () =
  Printf.printf "--- OK For Loop Test ---\n";

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_ok_for -> Some (fun (k : (a, _) continuation) ->
          
          (* ループの内部では継続 k には一切触らない *)
          for i = 1 to 3 do
            Printf.printf "for loop running: %d\n" i
          done;

          (* ループを完全に抜けた後で、確実に1回だけ再開する *)
          continue k ()

        )
      | _ -> None
  }