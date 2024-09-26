(*Take in a list, copy the list until you hit 0*)
let rec segregater list newlist =
  match list with
  | [] -> (newlist, [])
  | h :: t -> 
    if h = 0 then (newlist, t)
    else segregater t (newlist @ [h])

(*return a list of lists containing all the lists made in segregator*)
let rec divide list listoflists =
  match list with
  | [] -> listoflists
  | _ -> 
    let (newlist, oglist) = segregater list [] in
    divide oglist (listoflists @ [newlist])

(*filter function from textbook*)
let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let rec verify listoflists prevsign =
    match listoflists with 
    |[] -> true
    |h :: t -> 
      let currsign =
        if (filter ((>) 0) h) = h then 1 (*if all positive*)
        else if (filter ((<) 0) h) = h then -1 (*if all negative*)
        else 0 (*if mixed pos and neg*)
      in
      if currsign = 0 then false (*Can't be mixed*)
      else if currsign = prevsign then false
      else verify t currsign

let group lst =
  (*If the first element is 0 it's not valid*)
  if List.nth lst 0 = 0 then None
  else
  (*If the last element is 0 it's not valid*)
  if List.nth lst ((List.length lst) - 1) = 0 then None
  else
  let sublists = divide lst [] in
  if verify sublists 0 then Some sublists
  else None