type ratio = {num: int; denom: int};;

let add_ratio r1 r2 =
  {num = r1.num * r2.denom + r2.num * r1.denom;
   denom = r1.denom * r2.denom};;

let result = add_ratio {num=9; denom=3} {num=6; denom=2};;

let integer_part r =
  match r with
    {num=num; denom=denom} -> num / denom;;

print_int(integer_part(result));;