open Effect
open Effect.Deep

type _ Effect.t += Ask_or_err : unit Effect.t
let perform_ask () = perform Ask_or_err

let () =
  let some_condition = true in

  try_with perform_ask ()
  { effc = fun (type a) (eff : a t) ->
      match eff with
      | Ask_or_err -> Some (fun (k : (a, _) continuation) ->
          
          (* 【異常】左辺がtrueだと右辺は実行されないため、未再開リスク(Unused)が生じる *)
          let _result = some_condition || (continue k (); false) in
          ()

        )
      | _ -> None
  }