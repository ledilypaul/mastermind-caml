#use "code.ml"

module IA:

sig

(**Nombre d'algorithmes developpes*)
val nombre_methodes : int

(**Fonctions pour l'algorithme na�f *)

(**Choisit une combi al�atoirement parmis la liste des combis possibles
 *@param liste de toutes les combis
 *
 *@return une combi
*)
val choisir_code : Code.t list -> Code.t

(** Fonction qui permet � l'humain de saisir le score entre son code et la proposition de l'ordi
 *@param Code secret
 *@param proposition de l'ordi
 *
 *@return le nombre de pions bien et mal plac�s
 *)
val saisirScore : Code.t -> Code.t -> int * int




(*Fonctions pour Na�f *)


(**Fonction qui met a jour la liste des candidats possible en fonction du tour precedent
 * @param liste des candidats du tour precedent
 * @param proposition du tour precedent
 * @param score en propostion precedente et secret
 *
 * @return la liste des candidats possibles tq score entre chaque proposition et proiposition precendente = sc
*)
val majCandNaif: Code.t list -> Code.t -> int * int -> Code.t list

(**Fonction qui joue selon L'algorithme naif
 * @param la liste de toutes les combi autorisee
 * @param code secret
 * @param calcul automatique des scores si true
 * @param le nombre de coups � jouer
 *
 *@return jeu
*)
val naif: Code.t -> bool -> int -> unit


(*fonctions pour Knuth*)


(**Fonction qui retourne le nombre de chaque score que fait p parmis la liste
 * @param code de reference
 * @param liste des scores � parcourir
 * @param listes des codes � parcourir
 *
 *@return la liste des score precedes du nombres de combi qui ont ce score
*)
val totalScore : Code.t -> (int*int) list -> Code.t list -> (int * (int*int)) list

(**Fonction qui generalise totalScore a une liste de candidat
 * @param la liste des candidats
 * @param la liste de tous les scores
 * @param la liste de toutes les combinaisons
 *
 * @return la liste des listes de scores avec la combinaison
*)
val totalLstScore : Code.t list -> (int * int) list -> Code.t list -> ((int * (int * int)) list) list


(**Fonction qui recupere le max des scores
 * @param liste des scores
 *
 * @return le score max avec le score
*)
val maxScore : (int * (int * int)) list -> int

(**Fonction qui recup�re les max de chaque liste de score
 * @param la liste de toutes les listes de scores
 *
 * @return la liste des max
*)
val tabPoidsMax : (int * (int * int)) list list -> int list

(**Fonction qui renvoi le poids min
 * @param le tableau des poids
 *
 * @return le poids min
*)
val min : int list -> int

(**Fonction qui donne l'indice de la combi a jouer
 * @param le poids a trouver
 * @param la liste des poids
 *
 *@return le numero d'indice
*)
val numeroCombi: int -> int list -> int

(**Fonction qui donne la meilleure proposition � jouer
 * @param la liste des combinaison candidates
 * @param la liste de toutes les combi possibles
 *
 *@return la meilleure combinaison � jouer
*)
val meilleureProposition : Code.t list -> Code.t list -> Code.t

(**Fonction qui renouvelle la liste es candiats pour l'algo de Knuth
 * @param score precedent
 * @param combi precedente
 * @param liste des candidats precedents
 *
 * @return la liste des combi possibles
*)
val majcandidatKnuth : int * int -> Code.t -> Code.t list -> Code.t list

(**Fonction qui joue selon L'algorithme de Knuth
 * @param la liste de toutes les combi autorisee
 * @param code secret
 * @param calcul automatique des scores si true
 * @param le nombre de coups � jouer
 *
 *@return jeu
*)
val knuth : int -> Code.t -> bool -> unit


(*Fonctions generales*)

val nombre_methodes :int

(**Choisit un code a proposer
*	@param methode 0 pour l'algorithme naif,
*				   1 pour l'algorithme de KNUTH
*				   ... et ainsi de suite
*
*	@param essais la liste des codes deja proposes
*	@param la liste des codes possibles
*	@return le prochain code a essayer
*)
val choix : int -> Code.t list ->Code.t list -> Code.t


(** Filtre les codes possibles
* @param methode 0 pour l ' algorithme naif,
*		 1 pour l ' algorithme de KNUTH
*		 ... et ainsi de suite
* @param (code, rep) le code essaye et la reponse correspondante
* @param la liste de courante de codes possibles
* @return la nouvelle liste de codes possibles
*)
val filtre: int -> (Code.t * (int * int)) -> Code.t list -> Code.t list


(*Fonction qui lance l'IA a chaque partie
 * @param la methode choisie (0 : naif ;1 : knuth)
 * @param le nombre d'essais par parties
 * @param le code secret choisi par le joueur humain
 * @param calcul automatique ou non
 *
 *@return le jeu de lIA
*)
val jeuIA : int -> int -> Code.t -> bool -> unit

end = struct
open Code ;;
let valeur = List.length(Code.tous);;
let nombre_methodes = 2 ;;

let choisir_code lstComplete = List.nth lstComplete (Random.int 1296) ;;


let afficherCode code = print_string ("Combi : ");
	let rec affc combi =
		match combi with
			| a ::[] -> print_endline (string_of_int(a))
			| a :: liste -> print_string (string_of_int(a)) ; affc liste
	in affc code

let saisirScore combiSec prop = print_endline ("Code secret");
								afficherCode combiSec;
								print_endline ("Proposition IA");
								afficherCode prop;
								print_endline ("Combien de pions bien plac�s ?");
								let bp = read_int() in
								print_endline ("Combien de pions mal plac�s ?");
								let mp = read_int() in (bp,mp);;

let afficherScore sc combi = print_string("Le score est  : ");
				print_int (fst sc);
				print_string (" chiffre(s) bien place(s) et ");
				print_int(snd sc);
				print_endline (" chiffre(s) mal place(s) ");;

let rec majCandNaif lstCandidat prop sc =
	match lstCandidat with
		|[] -> []
		|a :: liste when (Code.reponse a prop) = sc -> a :: (majCandNaif liste prop sc)
		|a :: liste -> majCandNaif liste prop sc ;;


(*Knuth*)

(* Fonction qui determine combien de combi ont le score sc *)
let rec nbrScore p sc lstCode =
	match lstCode with
		|[] -> 0
		|a::lst when (Code.reponse a p) = sc -> 1 + nbrScore p sc lst
		|a::lst -> 0 + nbrScore p sc lst;;


(*fonction qui determine le nombre de combi pour tous les scores *)
let totalScore  p lstScore lstCode =
	List.fold_left (fun acc x -> acc@[((nbrScore p x lstCode),x)]) [] lstScore ;;

(*generalisation de la precedente *)
let totalLstScore lstCand lstScore lstComplete =
	List.fold_left (fun acc y -> acc@[(totalScore y lstScore lstComplete)]) [] lstCand;;

(* renvoie le numero du score max *)
	let maxScore tabScore = let m = List.hd tabScore in
		let rec max tab m =
			match tab with
				|[] -> fst m
				|(a,b)::lst -> if a > (fst m) then max lst (a,b) else max lst m
		in max tabScore m;;


(*recuperer poids max*)
let tabPoidsMax lstTotScore = List.map (fun x -> maxScore x) lstTotScore ;;

let min tabpoids =
	let m = List.hd tabpoids in
		let rec minimum tabpoids m =
			match tabpoids with
				|[] -> m
				|a :: lst -> if a < m then minimum lst a else minimum lst m
		in minimum tabpoids m;;

let rec numeroCombi poids tabpoids =
	match tabpoids with
		|[] -> failwith "mauvais tableau"
		|a :: lst when a = poids -> 0
		|a :: lst when a <> poids -> 1+ numeroCombi poids lst;;

let meilleureProposition lstCandidat lstComplete =
	let lstTotScore = totalLstScore lstCandidat Code.toutes_reponses lstComplete  in
		let tabpoids = tabPoidsMax lstTotScore in
			let poidsMin = min tabpoids in
				List.nth lstCandidat (numeroCombi poidsMin tabpoids);;

let rec majcandidatKnuth sc prop lstCandidat =
	match lstCandidat with
		|[] -> []
		|a :: liste when (Code.reponse a prop) = sc -> a :: (majcandidatKnuth sc prop liste)
		|a :: liste -> majcandidatKnuth sc prop liste;;

let rec afficherliste liste =
	match liste with
		|[] -> print_endline ("fin de la liste")
		|a :: lst -> afficherCode a ; afficherliste lst;;


(*Fonctions imposees*)

let choix numeroMethode lstCandidat lstComplete =
	match numeroMethode with
		|0 -> List.hd lstCandidat
		|1 -> meilleureProposition lstCandidat lstComplete
		|_ -> failwith ("erreur numero methode");;

let filtre numeroMethode (combi,sc) lstCandidat =
	match numeroMethode with
		|0 -> majCandNaif lstCandidat combi sc
		|1 -> majcandidatKnuth sc combi lstCandidat
		|_ -> failwith ("erreur numero methode");;



(*fonctions finales *)

let naif secret autom nbrTentatives =
	let lstCand = Code.tous in
		let p0 = choisir_code Code.tous in
			afficherCode p0;
			let sc0 = if autom then Code.reponse secret p0 else saisirScore secret p0 in
				afficherScore sc0 p0;
				let rec jeuN lstCandidats propPrec scPrec secret nbrCoup=
					match nbrCoup with
						|0 -> print_endline ("Fin de jeu de L'IA") ; print_string (" Le code secret �tait : "); afficherCode secret
						|a ->let nvlLstC = filtre 0 (propPrec,scPrec) lstCandidats in
								let pn = choix 0 nvlLstC Code.tous in
									afficherCode pn;
									let scn = if autom then Code.reponse secret pn else saisirScore secret pn in
										afficherScore scn pn;
										if scn = (4,0) then print_endline ("Gagne")
										else jeuN nvlLstC pn scn secret (nbrCoup -1)
				in jeuN lstCand p0 sc0 secret (nbrTentatives-1);;



let knuth nbrTentatives secret autom =
	let lstCand = Code.tous in
		let p0 = [2;2;1;1] in (* on donne directement le resultat de meilleurProposition lstComplete lstComplete car prend beaucoup de temps*)
			afficherCode p0;
			let sc0 = if autom then Code.reponse secret p0 else saisirScore secret p0 in
				afficherScore sc0 p0;
				let rec jeuK lstCandidats propPrec scPrec secret nbrCoup =
					match nbrCoup with
						|0 -> print_endline ("Fin de jeu de l'IA") ; print_string (" Le code secret �tait : "); afficherCode secret
						|a -> let nvlLstC = filtre 1 (propPrec,scPrec) lstCandidats in
									let pn = choix 1 nvlLstC Code.tous in
										afficherCode pn;
										let scn = if autom then Code.reponse secret pn else saisirScore secret pn in
										afficherScore scn pn;
										if scn = (4,0) then print_endline ("Gagne")
										else jeuK nvlLstC pn scn secret (nbrCoup -1)
				in jeuK lstCand p0 sc0 secret (nbrTentatives-1);;

let jeuIA numMethode nbrTentatives secret autom =
	match numMethode with
		|0 -> naif secret autom nbrTentatives
		|1 -> knuth nbrTentatives secret autom
		|_ -> failwith "Erreur sur le numero de la methode ";;


end;;
