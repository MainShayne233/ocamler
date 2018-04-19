let rec solve ?num1:((num1: int) = 1) ?num2:((num2: int) = 2)
    ?sum:((sum: int) = 0) () =
  if num2 > 4000000 then sum
  else if num2 mod 2 == 0 then
    solve ~num1:num2 ~num2:(num1 + num2) ~sum:(sum + num2) ()
  else solve ~num1:num2 ~num2:(num1 + num2) ~sum ()

let result = solve ()

;; print_int result
