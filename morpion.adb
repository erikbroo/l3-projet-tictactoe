with Ada.Text_Io;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;
use Ada.Text_Io;


procedure Morpion is
   
   -- Permet la génération d'un nombre aléatoire
   subtype Intervalle is Integer range 1..100 ;
   package Aleatoire is new Ada.Numerics.Discrete_Random( Intervalle ) ;
   use Aleatoire;
   
   -- Types de variables
   type T_Joueur is (Joueur1, Joueur2);
   
   subtype Ligne is Character range '1'..'3';
   subtype Colonne is Character range 'A'..'C';
   
   type T_Tableau is array(Ligne, Colonne) of Character;
   type T_FinDePartie is (Victoire, Egalite, Non);
   
   -- Variables
   Plateau : T_Tableau;
   FinDePartie : T_FinDePartie  := Non;
   
   J : T_Joueur;
   
   NbreCasesRempli : Integer := 0;
   I : Integer := 1; -- Numéro du joueur courant
   MaxJoueurs : constant Integer := 2;
   
   G : Aleatoire.Generator; 
   
   -- ** Affiche **
   -- => Affiche un plateau 
   procedure Affiche ( T : T_Tableau) is 
      I : Natural := 1;
   begin 
      
      Put_Line("  A    B     C");
	 
	for X in Ligne loop
	   for Y in Colonne loop   
	   Put(" " & T(X,Y) & " ");

	   if Y /= 'C' then 
	      Put(" | ");    
	   else 
	      Put(" " & Natural'Image(I));
	      I := I + 1;
	   end if;
	 
	end loop;
	
	New_Line;
	Put_Line(" ___  ___  ___ ");
	New_Line;
	
      end loop;
      New_Line;
      Put_Line("Joueur 1 => O");
      Put_Line("Joueur 2 => X");
   end Affiche;
   
   
   -- ** Saisie ** 
   -- => Saisie des coordonnées du joueur 
   -- => Modifie les variables X, Y ( caractères correspondant respectivement aux subtypes Colonne et Ligne)
   procedure Saisie (X,Y : in out Character) is 
      Valide : Boolean := False;
      S : Unbounded_String;
   begin      
      	while (not Valide) loop
	   Put("Coordonnées (C1 par exemple) : ");
	   S := Get_Line;
	   if ( Length(S) >= 2) then 
	     if (Element(S,1) in Colonne and Element(S,2) in Ligne) then
		X := Element(S,2);
		Y := Element(S,1);
		valide := True;
	     end if;
	   end if;
	end Loop;
   end Saisie;
   
   
   -- ** Joue **
   -- => Demande au joueur la position de sa case choisie
   -- => Vérifie que la case n'est pas occupée
   -- => Si oui, l'ajoute dans le plateau 
  procedure Joue (T : in out T_Tableau; J : T_Joueur) is 
     OK : Boolean := False;
     X,Y : Character := ' ';
  begin
     while (not OK and NbreCasesRempli <  (Ligne'width * Colonne'width) ) loop
	Saisie(X,Y);
	
	if T(X,Y) = ' ' then 
	   case J is 
	      when Joueur1 => T(X,Y) := 'O';
	      when Joueur2 => T(X,Y) := 'X';
	   end case;
	   OK := True;
	   NbreCasesRempli := NbreCasesRempli + 1; 
	else
	   Put_Line("Cette case est déjà prise...");
	end if;
	
     end loop;
  end Joue;  
  
  -- ** Nul **
  -- => Teste si le plateau est rempli (ce qui revient à dire qu'il y a une égalité)
  function Nul return T_FinDePartie is 
  begin
     if NbreCasesRempli >= (Ligne'width * Colonne'width) then 
	return Egalite;
     else 
	return Non;
     end if;
  end Nul;
     
  -- ** Gagne **
  -- => Vérifie si un joueur a gagné ou non 
  function Gagne (T : T_Tableau) return T_FinDePartie is  
  begin
     
     -- Lignes
     for X in Ligne loop
	if (T(X,'A') = T(X,'B') and T(X,'B') = T(X,'C') and T(X,'A') /= ' ' ) then 
	   FinDePartie := Victoire;
	end if;
     end loop;
     
     -- Colonnes
     for Y in Colonne loop
	if (T('1',Y) = T('2',Y) and T('2',Y) = T('3',Y) and T('1',Y) /= ' ')
	then 
	   FinDePartie := Victoire;
	end if;
     end loop;
     
     -- Diagonales
     if (T('1','A') = T('2','B') and  T('2','B') = T('3','C') and T('1','A') /= ' ') then 
       FinDePartie := Victoire;
     end if;
     
     if (T('1','C') = T('2','B') and T('2','B') = T('3','A') and T('1','C') /= ' ') then
       FinDePartie := Victoire;
     end if;
     return FinDePartie;
  end Gagne;
  
  
  -- **Initialisation**
  -- => Initialise un plateau vide (toutes les cases du tableau ont le caractère ' ')
  function Initialisation return T_Tableau is 
     T : T_Tableau;
  begin 
       for Y in Colonne loop	 
	  for X in Ligne loop
	     T(X,Y) := ' ';
	end loop;
       end loop;
       
       return T;
       
  end Initialisation;
    
  Ch : Unbounded_String;
  J1, J2 : Unbounded_String;
begin
   -- Initialise le générateur de nombres aléatoires 
   Reset(G);
   
   Put_Line("Bienvenue dans le jeu du (Super) Morpion !...");
   Put_Line("Faites [Entrée] pour commencer !");
   Ch := Get_Line;
    
   --Initialisation
   Plateau := Initialisation;
   I := Random(G);
   
   while (FinDePartie = Non) loop 
      Put(ASCII.ESC & "[2J"); -- Commande clear 
      case (I mod MaxJoueurs) is
	 when 1 => J := Joueur1;
       	 when 0 => J := Joueur2;
	 when Others => Put_Line("Erreur");
      end case;
      Put_Line("C'est au tour du " & T_Joueur'Image(J));
      Affiche(Plateau);
      Joue(Plateau,J);

      FinDePartie := Nul;
      FinDePartie := Gagne(Plateau);
      I := I + 1;
   end loop;
   
   Put(ASCII.ESC & "[2J"); -- Commande clear 
   Affiche(Plateau);
   
   case FinDePartie is 
      when Victoire => Put_line("Victoire du " & T_Joueur'Image(J));
      when Egalite  => Put_Line("Match nul !!");
      when Non => Put_Line ("Wtf o_O ? Tu as planté mon programme !"); 
   end case;
   
end Morpion;
