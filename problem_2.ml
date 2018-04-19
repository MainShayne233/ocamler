let rec problem_2 num1 num2 sum =
  if num2 > 4000000 then
    print_int sum
  else if num2 mod 2 == 0 then
    problem_2 num2 (num1 + num2) (sum + num2)
  else
    problem_2 num2 (num1 + num2) sum;;

problem_2 1 2 0

