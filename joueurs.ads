with Ada.Text_Io;
with Ada.Strings.Unbounded.Text_Io;
with Ada.Strings.Unbounded;
with Plateaux;

use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded.Text_Io;
use Ada.Text_Io;
use Plateaux;

Package Joueurs is
   type Joueur is private;
   type T_Joueur is (Joueur1, Joueur2);
   
   -- Constructeur de joueur
   function Initialisation_Joueur (NJ : T_Joueur; IA : Boolean) return Joueur;
   
   -- Getters
   function Get_NumJoueur (J :Joueur) return T_Joueur;
   function Get_Nom (J :Joueur) return Unbounded_String;
   function Get_Symbole (J :Joueur) return Character;
   function Get_IA (J :Joueur) return Boolean;
   
   -- Setters
   procedure Set_NumJoueur (J : out Joueur; TJ : in T_Joueur);
   procedure Set_Nom (J : out Joueur; N : in String);
   procedure Set_Nom (J : out Joueur; N : in Unbounded_String);
   procedure Set_Symbole (J : out Joueur; S : in Character);
   procedure Set_IA (J : out Joueur; B : in Boolean);
   
   -- function
   function Gagne (P : Plateau; J : Joueur) return T_FinDePartie;
   
private
   
   type Joueur is record
      NumJoueur : T_Joueur;
      Nom : Unbounded_String;
      Symbole : Character;
      IA : Boolean;
   end record;
end Joueurs;
