let is_factor_of_3_or_5 number =
  number mod 3 == 0 || number mod 5 == 0;;

let rec problem_1 = function
  (1000, sum) -> print_int sum
  | (index, sum) ->
      if is_factor_of_3_or_5 index then
        problem_1 (index + 1, sum + index)
      else
        problem_1 (index + 1, sum);;

problem_1 (0, 0);;
