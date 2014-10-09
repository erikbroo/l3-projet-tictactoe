with Ada.Text_Io;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Strings.Unbounded;
with Joueurs, Plateaux;

use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;
use Ada.Text_Io;
use Joueurs, Plateaux;

procedure Morpion is
   
   -- type 
   type T_FinDePartie is (Victoire, Egalite, Non);
   
   -- Permet la génération d'un nombre aléatoire
   subtype Intervalle is Integer range 1..100 ;
   package Aleatoire is new Ada.Numerics.Discrete_Random( Intervalle ) ;
   use Aleatoire;
   
   G : Aleatoire.Generator;
   
   -- ** Saisie ** 
   -- => Saisie des coordonnées du joueur 
   -- => Modifie les variables X, Y ( caractères correspondant respectivement aux subtypes Colonne et Ligne)
   procedure Saisie (X,Y : in out Character) is 
      COORDONNEES_INCORRECTES : exception;
      Valide : Boolean := False;
      S : Unbounded_String;
   begin      
      
      while (not Valide) loop
	 Put("Saisissez les coordonnées (C1 par exemple) : ");
	 S := Get_Line;
	 if ( Length(S) >= 2) then 
	    
	    --tester si la saisie est correcte et declencher une exception sinon
	TRY : begin
	   if (Element(S,1) in Colonne and Element(S,2) in Ligne) then
	      X := Element(S,2);
	      Y := Element(S,1);
	      valide := True;
	   else raise COORDONNEES_INCORRECTES ;
	   end if;
	   
	exception
	   when COORDONNEES_INCORRECTES => put_line("Coordonnées incorrectes") ;
	end TRY;
	
	 end if;
      end Loop;
      
   end Saisie;
   
  
   -- ** Joue **
   -- => Demande au joueur la position de sa case choisie
   -- => Vérifie que la case n'est pas occupée
   -- => Si oui, l'ajoute dans le plateau 
   procedure Joue (P : in out Plateau; J : Joueur) is 
      -- OK boolean vérifiant si la case n'est pas déjà occupée par un joueur
      OK : Boolean := False;
      X,Y : Character := ' ';
   begin
      while (not OK and not Plateau_Plein(P) ) loop
	 if ( not Get_IA(J) ) then 
	    Saisie(X,Y);
	 -- else 
	    -- Renvoie les coordonnées de la case de l'IA 
	 end if;
	 
	 if Case_Vide(P ,X,Y) then  
	    Set_Case(P, X, Y, Get_Symbole(J) );
	    OK := True;
	 else
	    Put_Line("Cette case est déjà prise...");
	 end if;
	 
      end loop;
   end Joue;  
   
   -- ** Nul **
   -- => Teste si le plateau est rempli (ce qui revient à dire qu'il y a une égalité)
   function Nul (P : Plateau) return T_FinDePartie is 
   begin
      if Plateau_Plein(P) then 
	 return Egalite;
      else 
	 return Non;
      end if;
   end Nul;
   
   -- ** Gagne **
   -- => Vérifie si un joueur a gagné ou non 
   function Gagne (P : Plateau) return T_FinDePartie is  
   begin
      
      -- Lignes
      for X in Ligne loop
	 if (Get_Case(P, X,'A') = Get_Case(P, X,'B') 
	       and Get_Case(P, X,'B') = Get_Case(P, X,'C') 
	       and not Case_Vide(P, X,'A')) then 
	    FinDePartie := Victoire;
	 end if;
      end loop;
      
      -- Colonnes
      for Y in Colonne loop
	 if (Get_Case(P, '1',Y) = Get_Case(P, '2',Y) 
	       and Get_Case(P, '2',Y) = Get_Case(P, '3',Y) 
	       and not Case_Vide(P, '1',Y)) 
	 then 
	    FinDePartie := Victoire;
	 end if;
      end loop;
      
      -- Diagonales
       if (Get_Case(P, '1','A') = Get_Case(P, '2','B') 
	     and Get_Case(P, '2','B') = Get_Case(P, '3','C')
	     and not Case_Vide(P, '1','A'))
       then 
	 FinDePartie := Victoire;
      end if;
      
      if (Get_Case(P, '1','C') = Get_Case(P, '2','B') 
	    and Get_Case(P, '2','B') = Get_Case(P, '3','A') 
	    and not Case_Vide(P, '1','C'))
      then 
	 FinDePartie := Victoire;
      end if;
      return FinDePartie;
   end Gagne;
   
      -- Variables
   PlateauJeu : Plateau;	
   FinDePartie : T_FinDePartie  := Non;
   I : Integer := 1; -- Numéro du joueur courant
   MaxJoueurs : constant Integer := 2;
   Ch : Unbounded_String;
   J1, J2, JoueurCourant : Joueur;
begin
   -- Initialise le générateur de nombres aléatoires 
   Reset(G);
   
   Put_Line("Bienvenue dans le jeu du (Super) Morpion !...");
   Put_Line("Faites [Entrée] pour commencer !");
   Ch := Get_Line;
   
   J1 := Initialisation_Joueur(Joueur1, False);
   J2 := Initialisation_Joueur(Joueur2, False);
   
   --Initialisation
   PlateauJeu := Initialisation_Plateau;
   I := Random(G);
   
   while (FinDePartie = Non) loop 
      Put(ASCII.ESC & "[2J"); -- Commande clear 
      case (I mod MaxJoueurs) is
	 when 1 => JoueurCourant := J1;
       	 when 0 => JoueurCourant := J2;
	 when Others => Put_Line("Erreur");
      end case;
      Put_Line("C'est au tour du joueur " & Get_Nom(JoueurCourant) & " (Symbole '" & Get_Symbole(JoueurCourant) & "')" );
      Affiche_Plateau(PlateauJeu);
      Joue(PlateauJeu,JoueurCourant);

      FinDePartie := Nul(PlateauJeu);
      FinDePartie := Gagne(PlateauJeu);
      I := I + 1;
   end loop;
   
   Put(ASCII.ESC & "[2J"); -- Commande clear 
   Affiche_Plateau(PlateauJeu);
   
   case FinDePartie is 
      when Victoire => Put_line("Victoire du joueur " & Get_NOm(JoueurCourant));
      when Egalite  => Put_Line("Match nul !!");
      when Non => Put_Line ("Wtf o_O ? Tu as planté mon programme !"); 
   end case;
   
end Morpion;
