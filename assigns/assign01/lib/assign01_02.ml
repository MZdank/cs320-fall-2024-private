let sqrt x =
  let rec counter i =
    if i * i >= x then i
    else counter (i + 1)
  in
  counter (0)

let is_prime y =
  let rec counter i =
    if i = y then true
    else if y mod i = 0 then false
    else counter (i + 1)
  in
  if y <= 2 then false
  else counter (2)

let nth_prime n =
  let rec count c primetest =
    if n = 0 then 2
    else if c = n then primetest - 1
    else if is_prime primetest then count (c + 1) (primetest + 1)
    else count c (primetest + 1)
  in
  count 0 2