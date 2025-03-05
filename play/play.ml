let () =
  let result = Utils.Cmd.run "ls -l" in
  print_endline "Command output:";
  print_endline result
