open Effect
open Effect.Deep

type _ Effect.t += Ask_or_ok : unit Effect.t
let perform_ask () = perform Ask_or_ok

let () =
  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_or_ok -> Some (fun (k : (a, _) continuation) ->
          
          (* 【安全】左辺は必ず実行される *)
          let _result = (continue k (); false) || true in
          ()

        )
      | _ -> None
  }