with Ada.Text_Io;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Strings.Unbounded;
with Joueurs, Plateaux;

use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;
use Ada.Text_Io;
use Joueurs, Plateaux;

package ia is
   -- Type inhérents à l'IA
   type C_Tableau is array(0..2) of Character;
   type Couple_C is array(0..1) of Character ;
   type Couple_I is array(1..2) of Integer;
   
   -- Fonction
   function CalcIA(Pl : Plateau; Profondeur : Integer; J,J2 : Joueur) return Couple_C;
   function CalcMax(Pl : Plateau; Profondeur : Integer; J,J2 : Joueur) return Integer;
   function CalcMin(Pl : Plateau; Profondeur : Integer; J,J2 : Joueur) return Integer;
   function CalcScore(Cntpion,Cntjoueur : Integer) return Integer;
   function TestLigne(C : C_Tableau; J : Joueur) return Couple_I;
   function Evaluation(Pl : Plateau; Profondeur : Integer; J : Joueur) return Integer;
   
end ia;
  
