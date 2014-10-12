with Ada.Text_Io;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Strings.Unbounded;
with Joueurs, Plateaux, Ia;

use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;
use Ada.Text_Io;
use Joueurs, Plateaux, Ia;

procedure Morpion is

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
   procedure Joue (P : in out Plateau; J,J2 : Joueur; Profondeur : Integer) is 
      -- OK boolean vérifiant si la case n'est pas déjà occupée par un joueur
      OK : Boolean := False;
      X,Y : Character := ' ';
      Coor : Couple_C;
   begin
      while (not OK and not Plateau_Plein(P) ) loop
	 X := ' ';
	 Y := ' ';
	 if ( not Get_IA(J) ) then 
	    Saisie(X,Y);
	 else 
	    Coor := CalcIA(P,Profondeur,J,J2);
	    X := Coor(0);
	    Y := Coor(1);
	 end if;
	 
	 if Case_Vide(P,X,Y) then  
	    Set_Case(P, X, Y, Get_Symbole(J) );
	    OK := True;
	 else
	    Put_Line("Cette case est déjà prise...");
	 end if;
	 
      end loop;
   end Joue;  
   
   -- ** Nul **
   -- => Teste si le plateau est rempli (ce qui revient à dire qu'il y a une égalité)
   --  function Nul (P : Plateau) return T_FinDePartie is 
   --  begin
   --     if Plateau_Plein(P) then 
   --  	 return Egalite;
   --     else 
   --  	 return Non;
   --     end if;
   --  end Nul;
   
   -- fonction menu 
   procedure Menu (J1, J2 : out Joueur; Choix : out Integer ) is
      CHOIX_INCORRECTE :exception;
      Reessayer : Unbounded_String;
   begin
      Choix := 0;
      While (Choix /= 1 AND Choix /= 2 AND Choix /= 3) loop
	 
     TRY:begin
	
	Put_Line("Pour jouer contre l'ordinateur tapez 1");
	Put_Line("Pour jouer contre un autre joueur tapez 2");
	Put_Line("Pour quitter taper 3");
	Choix:=Integer'Value(Get_Line);        
	
	case Choix is
	   when 1 =>
	      J1 := Initialisation_Joueur(Joueur1,False);
	      J2 := Initialisation_Joueur(Joueur2,True);
	   when 2 =>
	      J1 := Initialisation_Joueur(Joueur1,False);
	      J2 := Initialisation_Joueur(Joueur2,False);
	   when 3 => Put_Line("Au revoir !");exit;
	   when others => raise CHOIX_INCORRECTE;
	end case;
	
     exception
	when CHOIX_INCORRECTE => Put_Line ("Votre choix est incorrecte. Cliquez sur la touche [Entree] pour reessayer");Skip_Line;New_Line;
	Reessayer := Get_Line;
	when others => New_Line; Put_Line ("ERREUR ! ");New_Line;
	Put_Line("Cliquez sur la touche [Entree] pour reessayer");
	Reessayer := Get_Line;
     end TRY;
      end loop;
      
   end Menu;
   
   
   
   -- Variables
   PlateauJeu : Plateau;	
   FinDePartie : T_FinDePartie  := Non;
   I : Integer := 1; -- Numéro du joueur courant
   MaxJoueurs : constant Integer := 2;
   Ch : Unbounded_String;
   J1, J2, JoueurCourant,JoueurEnAttente : Joueur;
   
   Choix : Integer;
   
begin
   -- Initialise le générateur de nombres aléatoires 
   Reset(G);
   
   Put_Line("Bienvenue dans le jeu du (Super) Morpion !...");
   Put_Line("Faites [Entrée] pour commencer !");
   Ch := Get_Line;
   
   Menu(J1,J2,Choix);
   
   
   
   if (Choix /= 3) then 
   --Initialisation
   PlateauJeu := Initialisation_Plateau;
   I := Random(G);
   
   while (FinDePartie = Non) loop 
      Put(ASCII.ESC & "[2J"); -- Commande clear 
      case (I mod MaxJoueurs) is
	 when 1 => 
	    JoueurCourant := J1;
	    JoueurEnAttente := J2;
       	 when 0 => 
	    JoueurCourant := J2;
	    JoueurEnAttente := J1;
	 when Others => Put_Line("Erreur");
      end case;
      if(not Get_IA(JoueurCourant)) then
	 Put_Line("C'est au tour du joueur " & Get_Nom(JoueurCourant) & " (Symbole '" & Get_Symbole(JoueurCourant) & "')" );
      end if;
      Affiche_Plateau(PlateauJeu);
      Joue(PlateauJeu,JoueurCourant,JoueurEnAttente,9);

      -- FinDePartie := Nul(PlateauJeu);
      FinDePartie := Gagne(PlateauJeu,JoueurCourant);
      I := I + 1;
   end loop;
   
   Put(ASCII.ESC & "[2J"); -- Commande clear 
   Affiche_Plateau(PlateauJeu);
   
   case FinDePartie is 
      when Victoire => Put_line("Victoire du joueur " & Get_NOm(JoueurCourant));
      when Defaite => Put_Line("La defaite n'est pas envisagable !");
      when Egalite  => Put_Line("Match nul !!");
      when Non => Put_Line ("Wtf o_O ? Tu as planté mon programme !"); 
   end case;
   end if;
   
end Morpion;
