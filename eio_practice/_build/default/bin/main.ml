open Eio.Std

let () =
  (* Eio_main.run が適切なバックエンド(io_uring, kqueue等)を自動選択して起動する *)
  Eio_main.run @@ fun env ->
  (* env#stdout を使って標準出力のストリームを取得 *)
  let stdout = Eio.Stdenv.stdout env in
  Eio.Flow.copy_string "Hello, Eio with Effect Handlers!\n" stdout