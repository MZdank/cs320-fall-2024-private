let mk_unique_keys alist = 
  let rec newkey word num newlist = 
    match newlist with
    |[] -> [(word, num)]
    |(w, n) :: rest ->
      if w = word then
        (word, n + num) :: rest
      else
        (w, n) :: newkey word num rest
  in
  let rec help alist newlist =
    match alist with
    |[] -> newlist
    | (word, num) :: rest ->
      let newnewlist = newkey word num newlist in
      help rest newnewlist
  in
  help alist []