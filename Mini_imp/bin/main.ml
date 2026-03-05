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

let () = main()