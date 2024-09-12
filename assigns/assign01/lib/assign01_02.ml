let sqrt x =
  let rec counter i =
    if i * i >= x then i
    else counter (i + 1)
  in
  counter (0)

let is_prime y =
  let counter i =
    if i = y then true
    else if y mod i = 0 then false
    else counter (i + 1)
  in
  if y < 2 then false
  else counter (2)

(*This is bullshit but hopefully less bullshit*)
let nth_prime n =
  let rec count c primetest =
    if c = n then return primetest
    else if is_prime (primetest) then c = c + 1
    else count(c, primetest + 1)
  in
  count (0, 0)

(*THIS IS BULLSHIT:
Let nth_prime n =
  count = 0
  primetest = 0
  if count = n then return current_prime
  if is_prime (primetest) then count++
  else primetest++
  *)