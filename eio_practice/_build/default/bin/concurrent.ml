open Eio.Std

let task1 clock =
  traceln "Task 1: 開始 (1秒待機)";
  Eio.Time.sleep clock 1.0;
  traceln "Task 1; 終了"

let task2 clock =
  traceln "Task 2: 開始(0.5秒待機)";
  Eio.Time.sleep clock 0.5;
  traceln "Task 2: 終了"

let () =
  Eio_main.run @@ fun env ->
  (*時間を扱うためのclockケイパビリティを取得*)
  let clock = Eio.Stdenv.clock env in

  traceln "--- Fiber.bothのテスト ---";
  (*2つのタスクを並行に実行し、両方が終わるのを待つ*)
  Fiber.both
    (fun () -> task1 clock)
    (fun () -> task2 clock);

  traceln "--- 全てのタスクが終了 ---"