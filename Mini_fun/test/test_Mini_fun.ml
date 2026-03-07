open Mini_fun

let main () =
  let t =
    let_ "f" (fun_ "z" (fun_ "y" (fun_ "x" (if_ (op minor (var "x") (int_ 0)) (app (var "y") (var "x")) (app (var "z") (var "x")))))) 
      (app ((app (app (var "f") (fun_ "x" (op plus (var "x") (int_ 1)))) ((fun_ "x" (op minus (var "x") (int_ 1)))))) (int_ (-1)))
  in
  match eval_t t SMap.empty with
  | IntV n -> print_int n
  | BoolV b -> print_endline (string_of_bool b)
  | ClosureV _ | RecClosureV _ -> print_endline "Function value"

let () = main ()