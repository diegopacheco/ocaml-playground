let get_x (p : < x : int; .. >) = p#x
let get_y (p : < y : int; .. >) = p#y

let distance (p1 : < x : int; y : int; .. >) (p2 : < x : int; y : int; .. >) =
  let dx = p1#x - p2#x in
  let dy = p1#y - p2#y in
  sqrt (float_of_int (dx * dx + dy * dy))

let make_point x y = object
  method x = x
  method y = y
end

let make_colored_point x y color = object
  method x = x
  method y = y
  method color = color
end

let () =
  let p1 = make_point 3 4 in
  let p2 = make_colored_point 6 8 "red" in
  Printf.printf "Point x: %d, y: %d\n" (get_x p1) (get_y p1);
  Printf.printf "Colored point x: %d, y: %d, color: %s\n" (get_x p2) (get_y p2) p2#color;
  Printf.printf "Distance: %f\n" (distance p1 p2)
