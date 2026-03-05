type tree = Leaf of int | Node of tree * tree

let sum_tree (t: tree): int =
  let rec frontier (t: tree): int list =
    match t with
    | Leaf n -> [n]
    | Node (t1, t2) -> frontier t1 @ frontier t2
  in List.fold_left (+) 0 (frontier t)