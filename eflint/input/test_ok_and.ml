open Effect
open Effect.Deep

type _ Effect.t += Ask_and_ok : unit Effect.t
let perform_ask () = perform Ask_and_ok

let () =
  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_and_ok -> Some (fun (k : (a, _) continuation) ->
          
          (* 【安全】左辺は必ず実行されるので、kは確実に1回消費される *)
          let _result = (continue k (); true) && false in
          ()

        )
      | _ -> None
  }