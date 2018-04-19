let rec gcd x y =
  if y = 0 then x else gcd y (x mod y);;

let is_prime n =
  if gcd n 30 == 1 then
    do_is_prime 

Printf.printf "%d" (gcd 55 200)
