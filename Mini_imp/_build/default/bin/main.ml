open Mini_imp_Lib

let () =
  let filename = Sys.argv.(1) in
  Mini_imp_Interpreter.run_program(filename)