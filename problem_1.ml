(*
 * If we list all the natural numbers below 10 that are multiples of 3 or 5,
 * we get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *)

let is_factor_of_3_or_5 (number: int) : bool =
  number mod 3 == 0 || number mod 5 == 0

let rec solve ?index:((index: int) = 0) ?sum:((sum: int) = 0) () =
  match index with
  | 1000 -> sum
  | less_than_one_thousand ->
      if is_factor_of_3_or_5 index then
        solve ~index:(index + 1) ~sum:(sum + index) ()
      else solve ~index:(index + 1) ~sum ()

let result = solve ()

;; print_int result
