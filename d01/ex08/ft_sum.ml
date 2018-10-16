let ft_sum f low up =
  if low > up then nan else
    let rec sum_acc i acc =
      if i > up then acc
      else sum_acc (i + 1) (acc +. (f i))
    in
    sum_acc low 0.

let () =
  print_float(ft_sum (fun i -> float_of_int(i * i)) 1 10);
  print_newline ()