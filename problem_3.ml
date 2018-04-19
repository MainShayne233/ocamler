let large_number : int = 600851475143

let rec gcd (num1: int) (num2: int) : int =
  if num2 == 0 then num1 else gcd num2 (num1 mod num2)

let clamped_sqrt (num: int) : int = num |> float_of_int |> sqrt |> truncate

let is_even (num: int) : bool = num mod 2 == 0

let rec prime_check (num: int) (checker: int) (max: int) : bool =
  if checker > max then true
  else if
    num mod checker == 0 || num mod (checker + 4) == 0
    || num mod (checker + 6) == 0 || num mod (checker + 10) == 0
    || num mod (checker + 12) == 0 || num mod (checker + 16) == 0
    || num mod (checker + 22) == 0 || num mod (checker + 22) == 0
  then false
  else prime_check num (checker + 30) max

let is_prime (num: int) : bool =
  if num <= 3 then num >= 2
  else if num == 5 then true
  else if gcd 30 num != 1 then false
  else prime_check num 7 (clamped_sqrt num)

let rec find_next_prime (odd_num: int) : int =
  if is_prime odd_num then odd_num else find_next_prime (odd_num + 2)

let next_prime (num: int) : int =
  if is_even num then find_next_prime (num + 1) else find_next_prime (num + 2)

let rec largest_prime_factor (num: int) ?prime:((prime: int) = 2) () : int =
  if num == 1 then prime
  else if num mod prime == 0 then largest_prime_factor (num / prime) ~prime ()
  else largest_prime_factor num ~prime:(next_prime prime) ()

let solve : int = largest_prime_factor large_number ()

;; print_int solve
