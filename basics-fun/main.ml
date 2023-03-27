let x = 10;;
print_int(x);;
print_string("\n");;

let pi = 4.0 *. atan 1.0;;
print_float(pi);;
print_string("\n");;

let square x = x *. x;;
let result = square (sin pi) +. square (cos pi);;
print_float(result);;
print_string("\n");;