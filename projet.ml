
(**Module de definition d'un code dans le jeu Mastermind *)
module Code :
sig
(** Le type d'un pion*)
 type pion = Color of int ;;

(**Le type d'un code *)
 type t = pion list ;;

(**Nombre de pions par code*)
val nbr_pions : int ;;

(**Liste des couleurs possible *)
val couleurs_possibles : pion list ;;

(**Compare deux codes*
@param code1 premier code a comparer*
@param code2 second  code a comparer*
@return 0 si les deux codes sont identiques,
  un entier positif si [code1] est strictement plus grand que [code2]
  un entier negatif si [code1] est strictement plus petit que [code2] *)
val compare : t -> t -> int ;;

(**Conversion code vers chaine de caracteres (pour affichage)
*@param code code a convertir
*@return la representation en chaine de caracteres de [code] *)

val string_of_code : t -> string ;;

(**Conversion chaine de caracteres vers code (pour saisie)
*@param string chaine de caractere saisie
*@return le code correspondant a la saisie si la conversion est possible
                              [None] si la conversion n'est pas possible*)
val code_of_string : string -> t option ;;

(**La liste de tous les codes permis*)
val tous : t list ;;

(**La liste de toutes les reponses possibles*)
val toutes_reponses : (int * int) list ;;

(**Calcule la reponse d'un code par rapport au code cache
*@param code le code propose
*@param vrai_code le code cache
*@return un couple (nombre de pions bien places, nombre de pions mal places)
        [None] si la reponse ne peut etre calculee*)
val reponse : t -> t -> (int * int) list ;;

 end = struct
 type pion = Color of int ;;

 type t = pion list ;;

let nbr_pions = 4 ;;

let couleurs_possibles =[1;2;3;4;5;6];;

let propositionVeri = [Color 1;Color 2;Color 3;Color 4] ;;

let rec compare code1 code2 =
  match (code1,code2) with
    | ([],[]) -> 0
    | ( Color e::l1,Color f::l2) when e = f -> compare l1 l2
    | ( Color e::l1,Color f::l2) -> if e>f then 1 else -1 ;;

let string_of_code code1 =
  match code1 with
    | (Color a)::l1 -> a::(string_of_code l1)  ;;
    | [] ->
    |
end ;;

(** Algorithmes de recherche de code *)
module IA :
sig
	(** Nombre d ' algorithmes developpes *)
	val nombre_methodes : int
	
	(** Choisit un code a proposer
	* @param methode 0 pour l ' algorithme naif,
	*				 1 pour l ' algorithme de KNUTH
	*				 ... et ainsi de suite
	
	* @param essais la liste des codes deja proposes
	
	* @param possibles la liste des codes possibles
	* @return le prochain code a essayer
	*)
	val choix : int -> Code.t list -> Code.t list -> Code.t
	
	(** Filtre les codes possibles

	* @param methode 0 pour l ' algorithme naif,
	*				 1 pour l ' algorithme de KNUTH
	*				... et ainsi de suite
	* @param (code, rep) le code essaye et la reponse correspondante
	* @param la liste de courante de codes possibles
	* @param la nouvelle liste de codes possibles
	*)
	val filtre: int -> (Code.t * (int * int) option) -> Code.t list -> Code.t list
end ;;
