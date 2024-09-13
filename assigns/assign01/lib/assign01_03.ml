(*First three code blocks are from assign01_02.ml since I need the nth_prime function*)
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

(*Divide by prime numbers until you can't divide anymore*)  
let rec divide x prime count =
  if x mod prime <> 0 then (x, count)
  else 
  divide (x / prime) prime (count + 1)

(*Divide by each prime number until you can't anymore, then do next prime
number of times you can divide is the integer*)
let nth s i =
  let rec divide_help x y =
    if y = i then (*we've divided with every prime below i*)
      let (_, count) = divide (x) (nth_prime y) 0 in
      count (*don't care what x is anymore, return number of times we divided*)
    else (*if we're not on i*)
      let (new_x, _) = divide (x) (nth_prime y) 0 in
      (*divide by lower prime number until can't return what's left of s*)
      divide_help new_x (y + 1)  (*move onto next prime #, increment counter*)
  in 
  divide_help s 0
  