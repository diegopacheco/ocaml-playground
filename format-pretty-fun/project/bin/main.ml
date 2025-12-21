open Format

let print_list pp_elem ppf lst =
  fprintf ppf "@[<hov 2>[";
  let rec aux = function
    | [] -> ()
    | [x] -> pp_elem ppf x
    | x :: xs -> fprintf ppf "%a;@ " pp_elem x; aux xs
  in aux lst;
  fprintf ppf "]@]"

let print_int ppf n = fprintf ppf "%d" n

let print_record ppf (name, age, city) =
  fprintf ppf "@[<v 2>{ name: %s;@ age: %d;@ city: %s }@]" name age city

let print_tree ppf tree =
  let rec aux ppf = function
    | `Leaf n -> fprintf ppf "Leaf(%d)" n
    | `Node (l, r) -> fprintf ppf "@[<hov 2>Node(@,%a,@ %a@,)@]" aux l aux r
  in aux ppf tree

let () =
  printf "List: %a\n" (print_list print_int) [1; 2; 3; 4; 5];
  printf "Record: %a\n" print_record ("Alice", 30, "NYC");
  printf "Tree: %a\n" print_tree (`Node (`Leaf 1, `Node (`Leaf 2, `Leaf 3)))
