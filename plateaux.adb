package body Plateaux is
   
   -- **Initialisation**
   -- => Initialise un plateau vide (toutes les cases du tableau ont le caractère ' ')
   function Initialisation_Plateau return Plateau is 
      P : Plateau;
   begin 
      for Y in Colonne loop	 
	 for X in Ligne loop
	    P.Tableau(X,Y) := ' ';
	 end loop;
      end loop;
      P.NbreCasesRemplies := 0;
      return P;
      
   end Initialisation_Plateau;
   
    -- ** Affiche **
   -- => Affiche un plateau 
   procedure Affiche_Plateau ( P : Plateau) is 
      I : Natural := 1;
   begin 
      
      Put_Line("  A    B     C");
	 
	for X in Ligne loop
	   for Y in Colonne loop   
	   Put(" " & P.Tableau(X,Y) & " ");

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
   end Affiche_Plateau;
   
   procedure Set_Case (P : in out Plateau; X : in Ligne; Y : in Colonne; C : in Character) is
      COORDONNEES_INVALIDES : exception;
   begin
  Try : 
      begin 
      P.Tableau(X,Y) := C;
      P.NbreCasesRemplies :=  P.NbreCasesRemplies + 1;
      exception 
	    when COORDONNEES_INVALIDES => Put_Line("Coordonnées invalides dans la fonction Update_Plateau");
      end Try;
      
   end Set_Case;
   
   function Plateau_Vide (P : Plateau) return Boolean is
   begin
      return P.NbreCasesRemplies = 0;
   end Plateau_Vide;
   
   function Case_Vide (P: Plateau; X : Ligne; Y : Colonne) return Boolean is 
   begin 
      return P.Tableau(X,Y) = ' ';
   end Case_Vide; 
   
   function Plateau_Plein (P: Plateau) return Boolean is
   begin 
      return P.NbreCasesRemplies >= (Ligne'width * Colonne'width);
   end Plateau_Plein;
   
   function Get_NbreCasesRemplies (P : Plateau) return Integer is 
   begin 
      return P.NbreCasesRemplies;
   end Get_NbreCasesRemplies;
   
   function Get_Case (P : Plateau; X : Ligne; Y : Colonne) return Character is 
   begin
      return P.Tableau(X,Y);
   end Get_Case;
   
   procedure Set_NbreCasesRemplies (P : out Plateau; I : in Integer) is 
   begin
      P.NbreCasesRemplies := I;
   end Set_NbreCasesRemplies;
   
end Plateaux;
