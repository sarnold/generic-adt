with Ada.Text_IO;                   use  Ada.Text_IO;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;

with Ada.Numerics.Discrete_Random;

procedure Make_Dat is

   subtype Medium_Positive is Positive range 1..500000;
   package Random_Numbers is new Ada.Numerics.Discrete_Random(Medium_Positive);
   Gen : Random_Numbers.Generator;
   N   : Medium_Positive;
   New_File : File_Type;

begin -- Make_Dat
   Create(New_File, Out_File, "test.dat");
   Random_Numbers.Reset(Gen);
  for I in 1..1000 loop
     N := Random_Numbers.Random(Gen);
     Put(New_File, N, Width => 1);
     New_Line(New_File);
  end loop;
  Close(New_File);
end Make_Dat;







