let partial_sum l =
 List.rev (
    List.fold_left
      (fun acc x ->
         match acc with
         | [] -> [x]
         | y :: _ -> (y + x) :: acc)
      []
      l
  ) 