(*Tail recursively find sum of last n elements*)
let rec sum sumvar list n =
  (*base case*)
  if n = 0 then sumvar
  else
    match list with
    |[] -> sumvar
    |h :: t -> sum (sumvar + h) t (n - 1)

  (*make the list have at least k elements*)
let rec help biglist k length curlength = 
  if curlength > k then biglist
  else
    (*next element to add*)
    let sumvar = sum 0 biglist length in
    (*add to front of list so no timeout*)
    help (sumvar :: biglist) k length (curlength + 1)
(*    let newlist = biglist @ [sumvar] in    *)

let gen_fib list k =
  let length = List.length list in
  (*if list big enough*)
  if k < length then
    List.nth list k
  (*If not, build the list to be bigger than k*)
  else
    (*List is built backwards, reverse at end*)
    let result = help (List.rev list) k length length in
    List.nth (List.rev result) k