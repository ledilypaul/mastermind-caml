(**Module de definition d'un code dans le jeu Mastermind *)
module Code :
sig
(** Le type d'un pion*)
 type pion = int

(**Le type d'un code *)
 type t = pion list

(**Nombre de pions par code*)
val nombre_pions : int

(**Liste des couleurs possible *)
val couleurs_possibles : t list
(**Compare deux codes*
@param code1 premier code a comparer*
@param code2 second  code a comparer*
@return 0 si les deux codes sont identiques,
  un entier positif si [code1] est strictement plus grand que [code2]
  un entier negatif si [code1] est strictement plus petit que [code2] *)
val compare : t -> t -> int

(**Conversion code vers chaine de caracteres (pour affichage)
*@param code code a convertir
*@return la representation en chaine de caracteres de [code] *)

val string_of_code : t -> string

(**Conversion chaine de caracteres vers code (pour saisie)
*@param string chaine de caractere saisie
*@return le code correspondant a la saisie si la conversion est possible
                            [None] si la conversion n'est pas possible*)
val code_of_string : string -> t option

(** Fais la liste des toutes les combi de 4 parmis 6 choix possible
 *@param int le nombre d'elements par combinaison
 *@param 'a list list la liste de ce dont vous voulez les combinaisons avec chaque element est presente comme une liste
 *@return la liste de toute les combinaisons
*)
val toutes_les_combis :int -> 'a list list -> 'a list list

(**La liste de tous les codes permis*)
val tous : t list

val toutes_reponses : (int * int) list

val reponse : t -> t-> int * int

 end = struct
 
type pion =int ;;

type t = pion list ;;

let nombre_pions = 4 ;;

let couleurs_possibles =[[1];[2];[3];[4];[5];[6]];;

let rec compare code1 code2 =
	match(code1,code2) with
		|(e::l1,f::l2) when e = f -> compare l1 l2
		|(e::l1,f::l2) -> if e > f then 1 else -1
		|(_,_) -> 0;;

let rec string_of_code code =
	match code with
		| [] -> ""
		| a::l1 -> string_of_int(a)^(string_of_code l1);;

let test_validitechaine chaine = if String.length(chaine) <> 4 then false else
	let rec test_cos chaine  =
		match chaine with
		| "" -> true
		| a -> if ((int_of_string((String.sub a 0 1)) <= 6))  then test_cos ( (String.sub a 1) ((String.length a)-1))  else false
	in test_cos chaine ;;

let code_of_string chaine = if test_validitechaine chaine = false then None else
	let rec cos chaine =
		match chaine with
		| ""  -> []
		| a  -> int_of_string(String.sub a 0 1)::(cos (String.sub a 1 ((String.length a)-1)))
		in Some (cos chaine) ;;

(* ajout de la fonctions qui fait la liste de toutes les combi*)
let rec toutes_les_combis n a =
	if n = 0 then [[]]
	else
		let res = toutes_les_combis (n-1) a in
			List.concat (List.map (fun m -> List.map (fun s -> s@m) a) res);;

let tous = toutes_les_combis 4 couleurs_possibles;;

let rec combi2 bp mp =
	match (bp + mp) with 
		|a when a <= 4 -> if a < 4 then (bp,mp)::(combi2 bp (mp+1))
							else (bp,mp)::(combi2 (bp+1) 0)
		|a -> [];;

let toutes_reponses = combi2 0 0;;

(* etablis une liste de boolean ou vrai correspond à bien placé*)
let rec lstVF prop secret = 
	match (prop,secret)with
		|([],[]) -> []
		|(a::l,b::m) when a = b -> true :: lstVF l m 
		|(a::l,b::m) -> false :: lstVF l m ;;

let rec nbrVrai lst = 
	match lst with 
		|[] -> 0 
		|a :: liste -> if a then 1 + nbrVrai liste else 0 + nbrVrai liste;; 

(*donne la liste des elements restants a comparés*)
let rec mpLst listVF combi = 
	match listVF with 
		|[] -> []
		|a ::lst when a -> mpLst lst (List.tl combi)
		|a :: lst -> (List.hd combi) :: mpLst lst (List.tl combi);;

let couleurs_possibles2 =[1;2;3;4;5;6];;

(* compte le nombre d'appartion d'un pion dans une liste*) 
let lstPion pion liste = List.fold_left (fun acc2 y -> if pion = y then acc2+1 else acc2) 0 liste;;

(* renvoie le nombre d'apparition de chaque pion dans la liste donnee avec (nbr d'apparition * pion)*)
let occurence combi= List.fold_left (fun acc x -> acc@[(lstPion x combi,x)]) [] couleurs_possibles2;;

let min a b = if a > b then b else a ;;

let nbrMp lstMpSecret lstMpCombi = 
	let occSec = occurence lstMpSecret in 
		let occComb = occurence lstMpCombi in
			let rec mp occSec occComb = 
				match (occSec,occComb)with 
					|[],[]-> 0
					|(a::lst1,b::lst2) -> if (fst a) = (fst b) then (fst a) + mp lst1 lst2 else (min (fst a) (fst b)) + mp lst1 lst2
			in mp occSec occComb;;

let reponse secret prop = let vflst = lstVF prop secret in (nbrVrai vflst , nbrMp (mpLst vflst secret) (mpLst vflst prop));;

end;;
