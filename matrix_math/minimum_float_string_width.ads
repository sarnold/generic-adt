with Ada.Text_IO;

generic
   with package fio is new Ada.Text_IO.Float_IO(<>);
function Minimum_Float_String_Width(
   Item : in fio.Num;
   Aft  : in Ada.Text_IO.Field := fio.Default_Aft;
   Exp  : in Ada.Text_IO.Field := fio.Default_Exp) return natural;

