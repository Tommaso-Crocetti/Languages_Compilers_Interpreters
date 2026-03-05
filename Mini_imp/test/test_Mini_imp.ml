open Mini_imp
let main () =
  let c =
    (
      (while_ (minor (var "y") (aval 10))
          (assign "z" (aval 1))
)
    ) in
  let program =
    make_program "test" "x" "y" c in
  print_int (execute program 0)
  ;
  let c = seq (assign "y" (aval 0)) (while_ (minor (var "y") (aval 10)) (assign "y" (plus (var "y") (aval 1)))) in
  let program = make_program "test2" "x" "y" c in
  print_int (execute program 0)
let () = main()