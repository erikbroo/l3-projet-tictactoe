with Ada.Text_Io;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Strings.Unbounded;
with Joueurs, Plateaux;

use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;
use Ada.Text_Io;
use Joueurs, Plateaux;

package body Ia is
   subtype Intervalle is Integer range 1..100 ;
   package Aleatoire is new Ada.Numerics.Discrete_Random(Intervalle);
   use Aleatoire;
   
   -- coeur de l'IA cette fonction renvoie la case la plus appropriée à jouer
   function CalcIA(Pl : Plateau; Profondeur : Integer; J,J2 : Joueur) return Couple_C is
      P : Plateau := Pl;
      MaxCourant : Integer := -10000;
      Tmp : Integer; 
      Coor : Couple_C;
      G : Aleatoire.Generator;
   begin   
      for X in Ligne loop
	 for Y in Colonne loop
	    if Case_Vide(P,X,Y) then 
	       Set_Case(P,X,Y,Get_Symbole(J));
	       Tmp := CalcMin(P,Profondeur-1,J,J2);
	       if Tmp > MaxCourant then
		  Maxcourant := Tmp;
		  Coor := (X,Y);
	       end if; 
	       Set_Case(P,X,Y,' ');
	    end if;
	 end loop;
      end loop;
      return Coor;
   end CalcIA;
   
   -- Fonctionne de pair avec la fonction CalcMin elle simule les cases jouées par l'IA et cherche à maximiser le score (c'est-à-dire à chercher le meilleur coup possible pour l'IA)
   function CalcMax(Pl : in  Plateau; Profondeur : Integer; J,J2 : Joueur) return Integer is
       P : Plateau := Pl;
      Max : Integer := -10000;
      Tmp : Integer;
   begin
      if (not (Profondeur /= 0 and Gagne(P,J) = Non)) then
	 return Evaluation(P,Profondeur,J);
      else
	 for X in Ligne loop
	    for Y in Colonne loop
	       if Case_Vide(P,X,Y) then
		  Set_Case(P,X,Y,Get_Symbole(J));
		  Tmp := CalcMin(P,Profondeur-1,J,J2);
		  if Tmp > Max then 
		     Max := Tmp;
		  end if;
		  -- On annule le coup
		  Set_Case(P,X,Y,' ');
	       end if;
	    end loop;
	 end loop;
	 return Max;
      end if;
   end CalcMax;
   
   -- Fonctionne de pair avec la fonction CalcMax elle simule les cases jouées par le joueur et cherche à minimiser le score (c'est-à-dire à chercher le meilleur coup possible pour le joueur ou le pire coup pour l'IA)
   function CalcMin(Pl : Plateau;Profondeur : Integer; J,J2 : Joueur) return Integer is
      P : Plateau := Pl;
      Min : Integer := 10000;
      Tmp : Integer;
   begin
      if (not (Profondeur /= 0 and Gagne(P,J) = Non)) then
	 return Evaluation(P,Profondeur,J);
      else	 
	 for X in Ligne loop
	    for Y in Colonne loop
	       if Case_Vide(P,X,Y) then
		  Set_Case(P,X,Y,Get_Symbole(J2));
		  Tmp := CalcMax(P,Profondeur-1,J,J2);
		  if Tmp < Min then 
		     Min := Tmp;
		  end if;		    
		  Set_Case(P,X,Y,' '); -- On annule le coup
	       end if;
	    end loop;
	 end loop;
	 return Min;
      end if; 
   End CalcMin;
   
   -- Regarde le nombre de pions et on renvoie une valeur en fonction de l'état de la ligne
   function calcScore(Cntpion,Cntjoueur : Integer) return Integer is
   begin
      case Cntpion is
	 when 1 =>  return 10*Cntjoueur;
	 when 2 =>  return 40*Cntjoueur;
	 when others => return 0;
      end case;
   end CalcScore;
   
   -- Evalue une ligne et revoie le résultat sous forme de couple (permet facilement de lui attribuer un score par la suite)
   function TestLigne(C : C_Tableau; J : Joueur) return Couple_I is
      Pion : Couple_I;
   begin
      Pion(1) := 0;
      Pion(2) := 0;
      for X in 0..2 loop
	 if C(X) /= ' ' then
	    Pion(1) := Pion(1) + 1 ;
	    if C(X) = Get_Symbole(J) then
	       Pion(2) := Pion(2) +1;
	    else
	      Pion(2) := Pion(2) -1;
	    end if;
	 end if;
      end loop;
      return Pion;
   end TestLigne;
   
   -- Evalue la valeur des cases à un Etat n du plateau (c'est-à-dire le score du plateau si celui-ci est positif/nul/négatif respectivement indique une victoire/égalité/défaite)
   function Evaluation(Pl :Plateau;Profondeur : Integer; J : Joueur) return Integer is
      P : Plateau := Pl;
      N : T_FinDePartie;
      Somme : Integer := 0;
      Pions : Couple_I;
      Alignement : C_Tableau;
   begin
      N := Gagne(P,J);

      if Profondeur = 0 or N /= Non  then
	  case N is
	     when Victoire => 
		return +1000 - Get_NbreCasesRemplies(P);
	     when Defaite =>
		return -1000 + Get_NbreCasesRemplies(P);
	     when Others =>
		return 0;
	  end case;
      else
	 -- test de la diagonale descendante
	 Alignement := (Get_Case(P,'1','A'),Get_Case(P,'2','B'),Get_Case(P,'3','C'));
	 Pions := TestLigne(Alignement,J);
	 Somme:=Somme+CalcScore(Pions(1),Pions(2));
	 -- Test de la diagonale montante
	 Alignement := (Get_Case(P,'1','C'),Get_Case(P,'2','B'),Get_Case(P,'3','A'));
	 Pions := TestLigne(Alignement,J);
	 Somme:=Somme+CalcScore(Pions(1),Pions(2));
	 -- Test des lignes
	 for X in Ligne loop
	    Alignement := (Get_Case(P,X,'A'),Get_Case(P,X,'B'),Get_Case(P,X,'C'));
	    Pions := TestLigne(Alignement,J);
	    Somme := Somme + CalcScore(Pions(1),Pions(2));
	 end loop;
	 -- Test des colonnes
	 for Y in Colonne loop
	    Alignement := (Get_Case(P,'1',Y),Get_Case(P,'2',Y),Get_Case(P,'3',Y));
	    Pions :=  TestLigne(Alignement,J);
	    Somme := Somme + CalcScore(Pions(1),Pions(2));
	 end loop;
	 return Somme;
     end if;
   End Evaluation;
   
end Ia;
