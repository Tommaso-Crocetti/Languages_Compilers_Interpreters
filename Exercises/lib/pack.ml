let pack (l: 'a list) : 'a list list =
  let rec pack_r l res = 
    match l with
    | [] -> res
    | x::xs -> 
      match res with
      | [] -> pack_r xs [[x]]
      | y::ys -> 
        if x = List.hd y then pack_r xs ((x::y)::ys)
        else pack_r xs ([x]::res)
  in List.rev (pack_r l [])