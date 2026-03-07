open Mini_imp_Lib.Mini_imp
let main () =
  let c =
    (
      (while_ (minor (var "y") (aval 10))
          (assign "z" (aval 1))
)
    ) in
  let program =
    make_program "x" "y" c in
  print_int (execute program 0)
  ;
  let c = seq (assign "y" (aval 0)) (while_ (minor (var "y") (aval 10)) (assign "y" (plus (var "y") (aval 1)))) in
  let program = make_program "x" "y" c in
  print_int (execute program 0)
let () = main()
