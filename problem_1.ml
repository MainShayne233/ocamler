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
