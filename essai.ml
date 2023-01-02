type pion = Color of int ;;
type t = pion list ;;
	 

let couleurs_possibles =[Color 1;Color 2;Color 3;Color 4;Color 5;Color 6];;

let rec combi n a = 
  if n = 0 then [[]]
  else 
    let res = combi (n-1) a in 
    List.concat (List.map (fun m -> List.map (fun s -> s@m)  a) res);;

(*combi 4 [[Color 1];[Color 2];[Color 3];[Color 4];[Color 5];[Color 6]];;*)
