with Ada.Text_Io;
with Plateaux;
use Plateaux;
use Ada.Text_Io;
package body Joueurs is
   
      function Initialisation_Joueur (NJ : T_Joueur; IA : Boolean) return Joueur is
      J : Joueur;
      begin
	 -- Si le joueur n'est pas une IA alors demande de rentrer un nom 
	 if ( not IA ) then  
	    -- Saisie du nom du joueur 
	    Put_Line("Entrez le nom du " & T_Joueur'Image(NJ) & " :" );
	    J.Nom := Get_Line;
	 else 
	    J.Nom := To_Unbounded_String("IA");
	 end if;
	 
	 case NJ is
	    -- Définit le symbole, le numéro du joueur 1 
	    when Joueur1 => 
	       J.Symbole := 'O';
	       J.NumJoueur := Joueur1;
	       -- Définit le symbole, le numéro du joueur 2
	    when Joueur2 =>
	       J.Symbole := 'X';
	       J.NumJoueur := Joueur2;
	 end case;
	 J.IA := IA;
	 return J;
   end Initialisation_Joueur;
   
   -- Getters
   function Get_NumJoueur  (J :Joueur) return T_Joueur is 
   begin
      return J.NumJoueur;
   end Get_NumJoueur;
   
   function Get_Nom  (J :Joueur) return Unbounded_String is
   begin
      return J.Nom;
   end Get_Nom;

   function Get_Symbole (J :Joueur) return Character is 
   begin 
      return J.Symbole;
   end Get_Symbole;
   
   function Get_IA  (J :Joueur) return Boolean is
   begin
      return J.IA;
   end Get_IA;

   -- Setters
   procedure Set_NumJoueur (J : out Joueur; TJ : in T_Joueur) is
   begin
      J.NumJoueur := TJ;
   end Set_NumJoueur;

   procedure Set_Nom (J : out Joueur; N : in String) is 
   begin
      J.Nom := To_Unbounded_String(N);
   end Set_Nom;
   
    procedure Set_Nom (J : out Joueur; N : in Unbounded_String) is 
   begin
      J.Nom := N;
   end Set_Nom;

   procedure Set_Symbole (J : out Joueur; S : in Character) is 
   begin
      J.Symbole := S;
   end Set_Symbole;

   procedure Set_IA (J : out Joueur; B : in Boolean) is
   begin
      J.IA := B;
   end Set_IA;
      -- ** Gagne **
   -- => Vérifie si un joueur a gagné ou non 
   function Resultat (P : Plateau; J : Joueur) return T_FinDePartie is 
   begin
      -- Lignes
      for X in Ligne loop
   	 if (Get_Case(P, X,'A') = Get_Case(P, X,'B') 
   	       and Get_Case(P, X,'B') = Get_Case(P, X,'C') 
   	       and not Case_Vide(P, X,'A')) then 
   	    if Get_Case(P,X,'A') = Get_Symbole(J) then return Victoire;
   	    else return Defaite;
   	    end if;
   	 end if;
      end loop;
      
      -- Colonnes
      for Y in Colonne loop
   	 if (Get_Case(P, '1',Y) = Get_Case(P, '2',Y) 
   	       and Get_Case(P, '2',Y) = Get_Case(P, '3',Y) 
   	       and not Case_Vide(P, '1',Y)) 
   	 then 	 
   	    if Get_Case(P,'1',Y) = Get_Symbole(J) then return Victoire;
   	    else return Defaite;
   	    end if;
   	 end if;
      end loop;
      
      -- Diagonales
      if (Get_Case(P, '1','A') = Get_Case(P, '2','B') 
   	    and Get_Case(P, '2','B') = Get_Case(P, '3','C')
   	    and not Case_Vide(P, '1','A'))
      then 
   	 if Get_Case(P,'1','A') = Get_Symbole(J) then return Victoire;
   	 else return Defaite;
   	 end if;
      end if;
      
      if (Get_Case(P, '1','C') = Get_Case(P, '2','B') 
   	    and Get_Case(P, '2','B') = Get_Case(P, '3','A') 
   	    and not Case_Vide(P, '1','C'))
      then 
   	 if Get_Case(P,'1','C') = Get_Symbole(J) then return Victoire;
   	 else return Defaite;
   	 end if;
      end if;
      
      if Plateau_Plein(P) then
   	 return Egalite;
      end if;
      return Non;
   end Resultat;
   
end Joueurs;
