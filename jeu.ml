#use "Code.ml";;
#use "IA.ml";;
module Jeu:

        sig
(** Fonction qui laisse choisr � l'utilisateur la m�thode voulue entre knuth et na�ve*)
val choisirMethode : unit -> int

(** Fonction qui affiche une combinaison
 * @param combi � afficher
*)
val afficherCode : Code.t -> unit

(** Fonction qui demande � l'utilisateur d'entrez une combinaison*)
val saisirCode : unit -> Code.t

(** Fonction qui permet � l'humain de saisir le score entre son code et la proposition de l'ordi
 *@param Code secret
 *@param proposition de l'ordi
 *
 *@return le nombre de pions bien et mal plac�s
 *)
val saisirScore : Code.t -> Code.t -> int * int

(** Fonction qui affiche le score d'une combinaison
 * @param le score
 * @param la combi propos�e
 *
 * @return affiche le score
*)
val afficherScore : int * int -> unit

(** Fonction principal qui sert � lancer le jeu
 * @param nom du joueur humain
 * @param nbr de tentatives par parties
 * @param le nbr de partie � jouer
 * @param deroulement automatique ou manuel du score
 *
 * @return le jeu
 *)
val mastermind : string -> int -> int -> bool -> unit



        end=struct
open Code;;
open IA;;

let choisirMethode ()= print_endline "Choissez la methode de jeu";
		     print_endline "0. IA Facile";
		     print_endline "1. IA Hard";
		     let choix = read_int() in choix;;

let saisirCode () = print_endline " Entrez une combinaison";
		 print_endline " 1er Chiffre (entre 1 et 6)";
		 let c1 = read_int() in
		 print_endline " 2eme Chiffre (entre 1 et 6)";
		 let c2 = read_int() in
		 print_endline " 3eme Chiffre (entre 1 et 6)";
		 let c3 = read_int() in
		 print_endline " 4eme Chiffre (entre 1 et 6)";
		 let c4 = read_int()
		 in [c1;c2;c3;c4];;

let rec afficherCode code = match code with
	| a ::[] -> print_endline (string_of_int(a))
	| a :: liste -> print_string (string_of_int(a)) ; afficherCode liste;;


let afficherScore sc = print_string("Le score est  : ");
				print_int (fst sc);
				print_string (" chiffre(s) bien place(s) et ");
				print_int(snd sc);
				print_endline (" chiffre(s) mal place(s) ");;

let saisirScore combiSec prop = print_endline ("Code secret");
								afficherCode combiSec;
								print_endline ("Proposition IA");
								afficherCode prop;
								print_endline ("Combien de pions bien plac�s ?");
								let bp = read_int() in
								print_endline ("Combien de pions mal plac�s ?");
								let mp = read_int() in (bp,mp);;

let rec jeuHumain nbrTentatives nomJoueur secret=
	match nbrTentatives with
		|0 -> print_endline ("Votre tour est termin�"); print_string (" Le code secret �tait : "); afficherCode secret
		|_-> print_string nomJoueur;
			 let combi = saisirCode() in
				let sc = Code.reponse secret combi in
					afficherScore sc;
			jeuHumain (nbrTentatives-1) nomJoueur secret;;

let rec partieOrdiPuisHumain nomJoueur nbrTentatives nbrParties autom =
	let numeroMethode = choisirMethode () in
		match nbrParties with
			|0 -> print_endline ("Fin de partie")
			|1 -> print_string nomJoueur;
				  print_endline (", c'est a vous de choisir un code secret");
				  let codeCache = saisirCode () in
				  IA.jeuIA numeroMethode nbrTentatives codeCache autom;

			|a when a mod 2 = 0 -> 	let codeCache = IA.choisir_code Code.tous in
									jeuHumain nbrTentatives nomJoueur codeCache;
									partieOrdiPuisHumain nomJoueur nbrTentatives (nbrParties-1) autom

			|a when a mod 2 <> 0 -> print_string nomJoueur;
									print_endline (", c'est a vous de choisir un code secret");
									let codeCache = saisirCode () in
									IA.jeuIA numeroMethode nbrTentatives codeCache autom;
									partieOrdiPuisHumain nomJoueur nbrTentatives (nbrParties-1) autom;;

let rec partieHumainPuisOrdi nomJoueur nbrTentatives nbrParties autom =
	let numeroMethode = choisirMethode () in
		match nbrParties with
			|0 -> print_endline ("Fin de partie")
			|1 -> let codeCache = IA.choisir_code Code.tous in
				  jeuHumain nbrTentatives nomJoueur codeCache;

			|a when a mod 2 = 0 -> print_string nomJoueur;
									print_endline (", c'est a vous de choisir un code secret");
									let codeCache = saisirCode () in
									IA.jeuIA numeroMethode nbrTentatives codeCache autom;
									partieHumainPuisOrdi nomJoueur nbrTentatives (nbrParties-1) autom

			|a when a mod 2 <> 0 -> let codeCache = IA.choisir_code Code.tous in
									jeuHumain nbrTentatives nomJoueur codeCache;
									partieHumainPuisOrdi nomJoueur nbrTentatives (nbrParties-1) autom;;

let randomBool () = ((Random.int (2)) mod 2 = 0) ;;

let mastermind nomJoueur nbrTentatives nbrParties autom =
	let debut = randomBool ()in (* si vrai alors ordi commence a proposer un code secret*)
		if debut then
			if nbrParties mod 2 <> 0 then partieOrdiPuisHumain nomJoueur nbrTentatives (nbrParties + 1) autom
			else partieOrdiPuisHumain nomJoueur nbrTentatives nbrParties autom
		else
			partieHumainPuisOrdi nomJoueur nbrTentatives nbrParties autom;;

end;;
