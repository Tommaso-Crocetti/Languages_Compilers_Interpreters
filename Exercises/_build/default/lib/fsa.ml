type ('q, 's) fsa = {
  states: 'q list;
  alphabet: 's list;
  transition: ('q * 's * 'q) list;
  start: 'q;
  accept: 'q list;
}

let recognize (fsa: ('q, 's) fsa) (input: 's list) : bool =
  let rec step current_state input =
   match input with
   | [] -> List.mem current_state fsa.accept
   | symbol::rest -> 
    let transitions = List.filter (fun (q, s, q') -> q = current_state && s = symbol) fsa.transition
   in List.exists (fun (_, _, q') -> step q' rest) transitions
  in step fsa.start input