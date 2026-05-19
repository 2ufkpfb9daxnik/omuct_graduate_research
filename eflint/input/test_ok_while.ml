open Effect
open Effect.Deep

type _ Effect.t += Ask_ok_while : unit Effect.t

let perform_ask () = perform Ask_ok_while

let () =
  Printf.printf "--- OK While Loop Test ---\n";

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_ok_while -> Some (fun (k : (a, _) continuation) ->
          
          let count = ref 0 in
          (* ループの内部では継続 k には一切触らない *)
          while !count < 2 do
            Printf.printf "while loop running: %d\n" !count;
            incr count
          done;

          (* ループを完全に抜けた後で、確実に1回だけ再開する *)
          continue k ()

        )
      | _ -> None
  }