with Ada.Text_Io;
with Ada.Numerics.Discrete_Random;
use Ada.Text_Io;


procedure Morpion is
   
   -- Permet la génération d'un nombre aléatoire
   subtype Intervalle is Integer range 1..100 ;
   package Aleatoire is new Ada.Numerics.Discrete_Random( Intervalle ) ;
   use Aleatoire;
   
   -- Constantes 
   NbreColonnes : constant Natural := 3;
   
   -- Types de variables
   type Joueur is (Joueur1, Joueur2);
   type Tab is array(1..NbreColonnes * NbreColonnes) of Character;
   type T_FinDePartie is (Victoire, Egalite, Non);
   
   -- Variables
   Plateau : Tab;
   FinDePartie : T_FinDePartie  := Non; 
   J : Joueur;
   NbreCasesRempli : Integer := 0;
   I : Integer := 1; -- Numéro du joueur courant
   MaxJoueurs : constant Integer := 2;
   
   G : Aleatoire.Generator; 
   -- S : String := " ";
   
   
   -- ************ Procédures **************
   
   -- ** Affiche **
   -- => Affiche un plateau 
   procedure Affiche ( T : Tab) is 
      NumCase : Natural;
   begin 
      
      Put_Line(" A     B     C");
      for Y in 0..NbreColonnes-1 loop	 
	for X in 1..NbreColonnes loop
	   
	   NumCase := X + Y * NbreColonnes; 
	   Put(" " & T(NumCase) & " ");

	   if X < NbreColonnes then 
	      Put(" | ");    
	   else 
	      Put("  " & Natural'Image(Y+1));
	   end if;
	   
	end loop;
	New_Line;
	
      end loop;
      New_Line;
      Put_Line("Joueur 1 => O");
      Put_Line("Joueur 2 => X");
   end Affiche;
   
   
   -- ** Joue **
   -- => Demande au joueur la position de sa case choisie
   -- => Vérifie que la case n'est pas occupée
   -- => Si oui, l'ajoute dans le plateau 
   -- => Si toutes les cases du plateau sont occupées = Match nul et quitte la partie (et le programme) 
  procedure Joue (T : in out Tab; J : Joueur; FinDePartie : in out T_FinDePartie) is 
     B : Boolean := True;
     S : String := "  ";
     X,Y, NumCase : Integer;
  begin
     while (B and NbreCasesRempli < T'Length ) loop
	Put("Coordonnées (C1 par exemple) : ");
	S := Get_Line;
	
	-- 'A' => 1, 'B' => 2, 'C' => 3
	X := Character'Pos(S(1))- 64;
	-- 48 correspond au code décimal du caractère '0'
	Y := Character'Pos(S(2))- 48;
	
	Numcase := X + (Y-1) * NbreColonnes;
	
	  if T(NumCase) = ' ' then 
	   case J is 
	      when Joueur1 => T(NumCase) := 'O';
	      when Joueur2 => T(Numcase) := 'X';
	   end case;
	   B := False;
	   NbreCasesRempli := NbreCasesRempli + 1; 
	  else
	     Put_Line("Cette case est déjà prise...");
	  end if;
	  
	  if NbreCasesRempli >= T'Length then 
	     FinDePartie := Egalite;
	     -- Put_Line ("Match Nul !");
	  end if;
	  
     end loop;
  end Joue;     
     
  -- ** Gagne **
  -- => Vérifie si un joueur a gagné ou non 
  function Gagne (T : Tab) return T_FinDePartie is
  NumCase : Integer;   
  begin
     
     -- Lignes
     for Y in 0..NbreColonnes-1 loop
	Numcase := Y * NbreColonnes + 1; 
	if (T(Numcase) = T(NumCase + 1) and (T(Numcase + 1) = T(NumCase + 2)) and T(NumCase) /= ' ' ) then 
	   FinDePartie := Victoire;
	end if;
     end loop;
     
     -- Colonnes
     for X in 1..NbreColonnes loop
	if (T(X) = T(X + NbreColonnes) and T(X + NbreColonnes) = T(X + 2*NbreColonnes) and T(X) /= ' ')
	then 
	   FinDePartie := Victoire;
	end if;
     end loop;
     
     -- Diagonales
     if (T(1) = T(5) and T(5) = T(9) and T(1) /= ' ') then 
       FinDePartie := Victoire;
     end if;
     
     if (T(3) = T(5) and T(5) = T(7) and T(3) /= ' ') then
       FinDePartie := Victoire;
     end if;
     return FinDePartie;
  end Gagne;
  
  
begin
   -- Initialise le générateur de nombres aléatoires 
   Reset(G);
   
   Put_Line("Bienvenue dans le jeu du (Super) Morpion !...");
   
   -- Put("Cliquez sur [Entrée] pour commencer");
   -- S := Get_line;
   
   --Initialisation 
   Plateau :=  (1..Plateau'Length => ' ');
   I := Random(G);
   
   while (FinDePartie = Non) loop 
      Put(ASCII.ESC & "[2J"); -- Commande clear 
      case (I mod MaxJoueurs) is
	 when 1 => J := Joueur1;
       	 when 0 => J := Joueur2;
	 when Others => Put_Line("Erreur");
      end case;
      Put_Line("C'est au tour du " & Joueur'Image(J));
      Affiche(Plateau);
      Joue(Plateau,J, FinDePartie);

      I := I + 1;
      FinDePartie := Gagne(Plateau);
   end loop;
   
   Put(ASCII.ESC & "[2J"); -- Commande clear 
   Affiche(Plateau);
   
   case FinDePartie is 
      when Victoire => Put_line("Victoire du " & Joueur'Image(J));
      when Egalite  => Put_Line("Match nul !!");
      when Non => Put_Line ("Wtf o_O ? Tu as planté mon programme !"); 
   end case;
   
end Morpion;
