module type Showable = sig
  type t
  val show : t -> string
end

module MakeList (S : Showable) = struct
  let print_list lst =
    List.iter (fun x -> print_endline (S.show x)) lst
end

module IntShow = struct
  type t = int
  let show x = string_of_int x
end

module StringShow = struct
  type t = string
  let show x = x
end

module IntList = MakeList(IntShow)
module StrList = MakeList(StringShow)

let () =
  IntList.print_list [1; 2; 3; 4; 5];
  StrList.print_list ["hello"; "world"; "from"; "functor"]
