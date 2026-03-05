open Mini_fun

let main () =
  let t =
    letfun "fact" "n"
      (if_
         (op minor (var "n") (int_ 2))
         (int_ 1)
         (op times
             (var "n")
             (app (var "fact")
                  (op minus (var "n") (int_ 1)))))
      (app (var "fact") (int_ 5))
  in
  match eval_t t SMap.empty with
  | IntV n -> print_int n
  | BoolV b -> print_endline (string_of_bool b)
  | ClosureV _ | RecClosureV _ -> print_endline "Function value"

let () = main ()