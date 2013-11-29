with Ada.Strings.Fixed;

function Minimum_Float_String_Width(
   Item : in fio.Num;
   Aft  : in Ada.Text_IO.Field := fio.Default_Aft;
   Exp  : in Ada.Text_IO.Field := fio.Default_Exp) return natural is

   Str : string(1 .. 2 * fio.Num'digits);

function Length(Str : in string) return natural is
begin -- Length
   return Str'length;
end Length;

begin -- Minimum_Float_String_Width
   fio.Put(Str, Item, Aft, Exp);
   return Length(Ada.Strings.Fixed.Trim(Str, Ada.Strings.Both));
end Minimum_Float_String_Width;

