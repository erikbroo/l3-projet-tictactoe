with Ada.Text_Io;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;
use Ada.Text_Io;

package Plateaux is
   type Plateau is private;
   
   subtype Ligne is Character range '1'..'3';
   subtype Colonne is Character range 'A'..'C';
   type T_Tableau is array(Ligne, Colonne) of Character;
   
   -- Constructeur Plateau
   function Initialisation_Plateau return Plateau;
   
   procedure Affiche_Plateau (P : Plateau);
   
   -- getters
   function Get_NbreCasesRemplies (P : Plateau) return Integer;
   -- Récupère la valeur d'une case du plateau
   function Get_Case (P : Plateau; X : Ligne; Y : Colonne) return Character;
   
   
   -- setters
   
   -- Modifie la valeur d'une case du plateau
   procedure Set_Case (P : in out Plateau; X : in Ligne; Y : in Colonne; C : in Character);
   procedure Set_NbreCasesRemplies (P : out Plateau; I : in Integer);
   
   function Plateau_Vide (P : Plateau) return Boolean;
   function Case_Vide (P: Plateau; X : Ligne; Y : Colonne) return Boolean;
   function Plateau_Plein (P: Plateau) return Boolean;
     
private
   type Plateau is record
     Tableau : T_Tableau;
     NbreCasesRemplies : Integer;
   end record;
   
end Plateaux;
