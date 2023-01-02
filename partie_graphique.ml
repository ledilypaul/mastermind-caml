#load "graphics.cma";;
#load "unix.cma";;


(* vérifie si b <= a <= c *)
let compris_entre a b c =
		if b <= a then
			if a <= c then true
			else false
		else false;;

let coul_nbr x y =
	match (x,y) with
		|(x,y) when (x>350) && (x<375) && (y>50) && (y<75) -> 1
		|(x,y) when (x>350) && (x<375) && (y>75) && (y<100) -> 2
		|(x,y) when (x>350) && (x<375) && (y>100) && (y<125) -> 3
		|(x,y) when (x>375) && (x<400) && (y>50) && (y<75) -> 4
		|(x,y) when (x>375) && (x<400) && (y>75) && (y<100) -> 5
		|(x,y) when (x>375) && (x<400) && (y>100) && (y<125) -> 6;;

let rec conv_prop() = print_endline("conv_prop");
 let p =Graphics.wait_next_event[Graphics.Button_down] in
	match p with
		|pt when pt.Graphics.mouse_x >350 && pt.Graphics.mouse_x < 400 && pt.Graphics.mouse_y > 50 && pt.Graphics.mouse_y < 125 -> (coul_nbr pt.Graphics.mouse_x pt.Graphics.mouse_y)::(conv_prop())
		|pt -> conv_prop();;

let des_carre x y =
 	Graphics.set_color Graphics.white;
	Graphics.draw_rect x y 25 25;;

let rec grille x y nbr taille =
Graphics.set_color Graphics.white;
Graphics.draw_rect x y 25 25;
	match taille with
		|0 -> Graphics.set_color Graphics.black;
			Graphics.draw_rect 70 300 25 25;
	 	|a when a <> 0 ->  	match nbr with
                				|3 -> grille 70 (y+25) 0 (taille -1)
                                                |_ -> grille (x+25) y (nbr+1) taille ;;


let ko_bleu num numTour =
        Graphics.moveto 70 50;
        Graphics.set_color Graphics.blue;
        Graphics.fill_circle (82+ (num*25)) (62+(25*numTour)) 11 ;;

let ko_rouge num numTour=
        Graphics.moveto 95 50;
        Graphics.set_color Graphics.red;
        Graphics.fill_circle (82+ (num*25)) (62+(25*numTour)) 11 ;;

let ko_vert num numTour=
        Graphics.moveto 120 50;
        Graphics.set_color Graphics.green;
        Graphics.fill_circle (82+ (num*25)) (62+(25*numTour)) 11 ;;

let ko_cyan num numTour=
        Graphics.moveto 145 50;
        Graphics.set_color Graphics.cyan;
        Graphics.fill_circle (82+ (num*25)) (62+(25*numTour)) 11 ;;

let ko_violet num numTour=
        Graphics.moveto 70 75;
        Graphics.set_color Graphics.magenta;
        Graphics.fill_circle (82+ (num*25)) (62+(25*numTour)) 11 ;;

let ko_jaune num numTour=
        Graphics.moveto 120 75;
        Graphics.set_color Graphics.yellow;
        Graphics.fill_circle (82+ (num*25)) (62+(25*numTour)) 11 ;;


let rec bouton_bleu num numTour=
	match num with
		| a when a <= 3-> let p = Graphics.wait_next_event[Graphics.Button_down] in
							match p with
							|pt when pt.Graphics.mouse_x > 350 && pt.Graphics.mouse_x < 375 && pt.Graphics.mouse_y > 50 && pt.Graphics.mouse_y < 75 -> ko_bleu num numTour; bouton_bleu (num + 1) numTour
							|pt when pt.Graphics.mouse_x > 350 && pt.Graphics.mouse_x < 375 && pt.Graphics.mouse_y > 75 && pt.Graphics.mouse_y < 100 -> ko_vert num numTour;bouton_bleu (num + 1) numTour
							|pt when pt.Graphics.mouse_x > 350 && pt.Graphics.mouse_x < 375 && pt.Graphics.mouse_y > 100 && pt.Graphics.mouse_y < 125 -> ko_jaune num numTour;bouton_bleu (num + 1) numTour
							|pt when pt.Graphics.mouse_x > 375 && pt.Graphics.mouse_x < 400 && pt.Graphics.mouse_y > 50 && pt.Graphics.mouse_y < 75 -> ko_rouge num numTour ;bouton_bleu (num + 1) numTour
							|pt when pt.Graphics.mouse_x > 375 && pt.Graphics.mouse_x < 400 && pt.Graphics.mouse_y > 75 && pt.Graphics.mouse_y < 100 -> ko_cyan  num numTour;bouton_bleu (num + 1) numTour
							|pt when pt.Graphics.mouse_x > 375 && pt.Graphics.mouse_x < 400 && pt.Graphics.mouse_y > 100 && pt.Graphics.mouse_y < 125 -> ko_violet num numTour;bouton_bleu (num + 1) numTour
		|_ -> bouton_bleu num numTour;;


let grille_color()=
Graphics.set_color Graphics.white;
Graphics.moveto 350 50;                 (* Grille couleurs*)
Graphics.lineto 400 50;
Graphics.moveto 350 75;
Graphics.lineto 400 75;
Graphics.moveto 350 100;
Graphics.lineto 400 100;
Graphics.moveto 350 125;
Graphics.lineto 400 125;
Graphics.moveto 350 50;
Graphics.lineto 350 125;

Graphics.moveto 400 50;
Graphics.lineto 400 125;
Graphics.set_color Graphics.blue;
Graphics.fill_circle 363 62 12;
Graphics.set_color Graphics.green;
Graphics.fill_circle 363 87 12;
Graphics.set_color Graphics.yellow;
Graphics.fill_circle 363 112 12;
Graphics.set_color Graphics.red;
Graphics.fill_circle 387 62 12;
Graphics.set_color Graphics.cyan;
Graphics.fill_circle 387 87 12;
Graphics.set_color Graphics.magenta;
Graphics.fill_circle 387 112 12;
Graphics.set_color Graphics.white;
Graphics.moveto 375 50;
Graphics.lineto 375 125;;

let carre_SubEra()=
Graphics.set_color Graphics.green;
Graphics.fill_rect 70 25 47 20;
Graphics.set_color Graphics.blue;
Graphics.fill_rect 125 25 45 20;
Graphics.set_color Graphics.white;
Graphics.moveto 70 27;
Graphics.draw_string "Submit";
Graphics.moveto 125 27;
Graphics.draw_string "Erase";;


		let graph_game() =
			(* paramètres de la fenetre*)
			Graphics.open_graph " 500x500";
			Graphics.set_window_title "Game";
			Graphics.clear_graph();
			Graphics.set_color Graphics.black;
			Graphics.fill_rect 0 0 500 500;

			grille 70 50 0 10;
			grille_color();
			carre_SubEra();
			bouton_bleu 0 5;;



			let rec bouton_game() =
	let p =Graphics.wait_next_event[Graphics.Button_down] in
	match p with
		|pt when pt.Graphics.mouse_x > 100 && pt.Graphics.mouse_x < 160 && pt.Graphics.mouse_y > 350 && pt.Graphics.mouse_y < 390 -> graph_game()
		|pt when pt.Graphics.mouse_x > 300 && pt.Graphics.mouse_x < 360 && pt.Graphics.mouse_y > 350 && pt.Graphics.mouse_y < 390 -> Graphics.close_graph()
		|pt -> bouton_game();;




let menu_jouer ()=
	(* paramètres de la fenetre*)
	Graphics.open_graph " 500x500";
	Graphics.set_window_title "Jeux";
	Graphics.clear_graph();
	(* Intro*)
	Graphics.moveto 170 420;
	Graphics.draw_string "Jouer une partie";

	(* Boutons Jouer/Quitter*)
	Graphics.set_color Graphics.blue;
	Graphics.fill_rect 110 350 75 40;
	Graphics.moveto 110 370;
	Graphics.set_color Graphics.black;
	Graphics.draw_string "Partie";
	Graphics.set_color Graphics.red;
	Graphics.fill_rect 310 350 75 40;
	Graphics.moveto 310 370;
	Graphics.set_color Graphics.black;
	Graphics.draw_string "IA VS Man";
	bouton_game();;
	
let rec bouton_accueil() =
		let p =Graphics.wait_next_event[Graphics.Button_down] in print_endline("pass");
		match p with
			|pt when pt.Graphics.mouse_x > 100 && pt.Graphics.mouse_x < 160 && pt.Graphics.mouse_y > 350 && pt.Graphics.mouse_y < 390 -> menu_jouer()
			|pt when pt.Graphics.mouse_x > 300 && pt.Graphics.mouse_x < 360 && pt.Graphics.mouse_y > 350 && pt.Graphics.mouse_y < 390 -> Graphics.close_graph()
			|pt -> bouton_accueil() ;;


let menu ()=
	(* paramètres de la fenetre*)
	Graphics.open_graph " 500x500";
	Graphics.set_window_title "Mastermind";

	(* Intro*)
	Graphics.moveto 140 420;
	Graphics.draw_string "Bienvenue Dans Le Mastermind";

	(* Boutons Jouer/Quitter*)
	Graphics.set_color Graphics.green;
	Graphics.fill_rect 100 350 60 40;
	Graphics.moveto 110 370;
	Graphics.set_color Graphics.black;
	Graphics.draw_string "Jouer";
	Graphics.set_color Graphics.red;
	Graphics.fill_rect 300 350 60 40;
	Graphics.moveto 310 370;
	Graphics.set_color Graphics.black;
	Graphics.draw_string "Sortir";
	bouton_accueil();;


menu();;

(**#use "partie_graphique.ml" ;;*)
