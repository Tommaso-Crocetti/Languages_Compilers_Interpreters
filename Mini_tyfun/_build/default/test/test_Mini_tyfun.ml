open Mini_tyfun_Lib

let programs_dir = "tests"

let test_inputs = [0; 1; 2; 3; 5; 10; -1; -5]

let execute prog n =
  Mini_fun.extract_int
    (Mini_fun.compute
       (Mini_fun.app (Mini_tyfun.drop_types prog) (Mini_fun.int_ n)))

let expected_outputs = [
	("01_identity.mtfun", [0; 1; 2; 3; 5; 10; -1; -5]);
	("02_constant.mtfun", [42; 42; 42; 42; 42; 42; 42; 42]);
	("03_double.mtfun", [0; 2; 4; 6; 10; 20; -2; -10]);
	("04_square.mtfun", [0; 1; 4; 9; 25; 100; 1; 25]);
	("05_increment.mtfun", [1; 2; 3; 4; 6; 11; 0; -4]);
	("06_decrement.mtfun", [-1; 0; 1; 2; 4; 9; -2; -6]);
	("07_negate.mtfun", [0; -1; -2; -3; -5; -10; 1; 5]);
	("08_abs_value.mtfun", [0; 1; 2; 3; 5; 10; 1; 5]);
	("09_clamp_zero.mtfun", [0; 1; 2; 3; 5; 10; 0; 0]);
	("10_sign.mtfun", [0; 1; 1; 1; 1; 1; -1; -1]);
	("11_let_binding.mtfun", [1; 3; 5; 7; 11; 21; -1; -9]);
	("12_nested_let.mtfun", [-1; 1; 3; 5; 9; 19; -3; -11]);
	("13_bool_let.mtfun", [0; 1; 2; 3; 5; 10; 1; 5]);
	("14_quadratic.mtfun", [-2; -4; -4; -2; 8; 68; 2; 38]);
	("15_not_expr.mtfun", [0; 1; 4; 9; 25; 100; 0; 0]);
	("16_sum_naturals.mtfun", [0; 1; 3; 6; 15; 55; 0; 0]);
	("17_factorial.mtfun", [1; 1; 2; 6; 120; 3628800; 1; 1]);
	("18_fibonacci.mtfun", [0; 1; 1; 2; 5; 55; 0; 0]);
	("19_power_of_two.mtfun", [1; 2; 4; 8; 32; 1024; 1; 1]);
	("20_sum_squares.mtfun", [0; 1; 5; 14; 55; 385; 0; 0]);
	("21_sum_step2.mtfun", [0; 0; 2; 3; 8; 30; 0; 0]);
	("22_countdown.mtfun", [0; 1; 2; 3; 5; 10; 0; 0]);
	("23_curried_add.mtfun", [10; 11; 12; 13; 15; 20; 9; 5]);
	("24_curried_multiply.mtfun", [0; 3; 6; 9; 15; 30; -3; -15]);
	("25_min_hundred.mtfun", [0; 1; 2; 3; 5; 10; -1; -5]);
	("26_max_zero.mtfun", [0; 1; 2; 3; 5; 10; 0; 0]);
	("27_apply_twice.mtfun", [6; 7; 8; 9; 11; 16; 5; 1]);
	("28_higher_order_apply.mtfun", [0; 1; 4; 9; 25; 100; 1; 25]);
	("29_compose.mtfun", [2; 4; 6; 8; 12; 22; 0; -8]);
	("30_conditional_select.mtfun", [200; 200; 200; 200; 200; 200; 100; 100]);
	("31_apply_n_times.mtfun", [0; 1; 2; 3; 5; 10; 0; 0]);
	("32_multi_letfun.mtfun", [0; 0; 20; 182; 2970; 147840; 0; 0]);
	("33_ackermann.mtfun", [3; 5; 7; 9; 13; 23; 3; 3]);
	("34_deep_let.mtfun", [101; 121; 143; 167; 221; 391; 83; 31]);
	("35_gcd.mtfun", [0; 1; 1; 1; 1; 1; -1; -5]);
]

let parse_file path =
	let ic = open_in path in
	let lexbuf = Lexing.from_channel ic in
	let prog = Mini_tyfun_Parser.prg Mini_tyfun_Lexer.read lexbuf in
	close_in ic;
	prog

let is_mtfun_file name = Filename.check_suffix name ".mtfun"

let sorted_program_files dir =
	Sys.readdir dir
	|> Array.to_list
	|> List.filter is_mtfun_file
	|> List.sort String.compare

let expected_for_file fname = List.assoc_opt fname expected_outputs

let run_program_file dir fname =
	let path = Filename.concat dir fname in
	Printf.printf "=== %s ===\n%!" fname;
	let counts =
		try
		let prog = parse_file path in
		match Mini_tyfun.type_check prog with
		| None ->
			Printf.printf "  [TYPE ERROR] program is not well-typed\n%!";
			(1, 0)
		| Some _ ->
			(match expected_for_file fname with
			| None ->
				Printf.printf "  [NO EXPECTED] syntax/type check only\n%!";
				(1, 1)
			| Some expected ->
				if List.length expected <> List.length test_inputs then (
					Printf.printf "  [EXPECTED ERROR] expected vector length mismatch\n%!";
					(1, 0)
				) else (
					let all_ok = ref true in
					List.iter2
						(fun n exp ->
							try
								let result = execute prog n in
								Printf.printf "  f(%3d) = %d\n%!" n result;
								if result <> exp then (
									all_ok := false;
									Printf.printf "    [MISMATCH] expected %d, got %d\n%!" exp result
								)
							with e ->
								all_ok := false;
								Printf.printf "  f(%3d) raised: %s\n%!" n (Printexc.to_string e))
						test_inputs
						expected;
					if !all_ok then (1, 1) else (1, 0)
				))
		with e ->
			Printf.printf "  [PARSE ERROR] %s\n%!" (Printexc.to_string e);
			(0, 0)
	in
	print_newline ();
	counts

let () =
	if not (Sys.file_exists programs_dir && Sys.is_directory programs_dir) then
		failwith
			"Cannot locate test/tests. \
			 Ensure the tests directory exists inside Mini_tyfun/test.";

	let files = sorted_program_files programs_dir in
	Printf.printf "Found %d test programs in %s\n\n%!" (List.length files) programs_dir;
	let parsed_count = ref 0 in
	let typed_count = ref 0 in
	List.iter
		(fun f ->
			let p, t = run_program_file programs_dir f in
			parsed_count := !parsed_count + p;
			typed_count := !typed_count + t)
		files;
	Printf.printf
		"Summary: parsed %d/%d, correct %d/%d\n%!"
		!parsed_count
		(List.length files)
		!typed_count
		(List.length files);
	if !parsed_count <> List.length files || !typed_count <> List.length files then
		failwith "One or more Mini_tyfun tests failed"
