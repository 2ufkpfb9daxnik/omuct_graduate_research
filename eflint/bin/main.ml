
let () =
  let filename = "test.ml" in
  let ic = open_in_bin filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Printf.printf "ファイル '%s' の内容:\n%s\n" filename content

