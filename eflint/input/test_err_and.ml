open Effect
open Effect.Deep

type _ Effect.t += Ask_and_err : unit Effect.t
let perform_ask () = perform Ask_and_err

let () =
  let some_condition = false in

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_and_err -> Some (fun (k : (a, _) continuation) ->
          
          (* 【異常】右辺は実行されない可能性があるため、未再開リスク(Unused)が生じる *)
          let _result = some_condition && (continue k (); true) in
          ()

        )
      | _ -> None
  }