-----------------------------------------------------------------------------
--  Allan Hancock College
--  CS152, Spring 2000
--  Assignment Merge Sort
--  Stephen L Arnold
--  Procedure Merge_Sort
-----------------------------------------------------------------------------
--  Description: This program implements a merge sort using the Priority
--  Queue and Dynamic List Manager packages.  It performs a partition (or
--  "chunk") phase, followed by a merge phase, to sort a large disk file.
--
--  You can enter an input filename on the command line, or the program
--  will prompt you for the name.
-----------------------------------------------------------------------------

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Ada.Exceptions;                use Ada.Exceptions;

with Binary_Search_Tree;
with Priority_Queue_Manager;

Procedure Merge_Sort is

   subtype DOS8_3 is String(1..12);

   Input_File   : File_Type;
   Output_File  : File_Type;
   Filename     : String(1..512);
   Last         : Natural;
   Input1       : Integer;
   Input2       : Integer;
   Temp_File1   : File_Type;
   Temp_File2   : File_Type;
   Temp_File3   : File_Type;
   Temp_Name1   : DOS8_3;
   Temp_Name2   : DOS8_3;
   Temp_Name3   : DOS8_3;
   Temp_Count   : Natural  := 0;
   Temp_Size    : Positive := 100;

   function Identity(Element : in Integer) return Integer is
   begin --Identity
      return Element;
   end Identity;

   function P(Filename : in DOS8_3) return Natural is
   begin --P
      return Natural'Value(Filename(Filename'First..Index(Filename, ".")-1));
   end P;

   package BT is new Binary_Search_Tree(Integer, Integer, Identity, "=", "<");  use BT;
   package Q is new Priority_Queue_Manager(DOS8_3, Natural, P, ">");    use Q;

   Tree         : aliased BST;
   Name_Queue   : Priority_Queue_Type;

   procedure Put(I : in Integer; Cont : out Boolean) is
   begin -- Put
      Put(Temp_File1,I,Width => 1);
      New_Line(Temp_File1);
      Cont := True;
   end Put;

   procedure Write_Tree is new Traverse(Put);

begin --Merge_Sort

   Put_Line("You'll need free disk space equal to a little more than twice ");
   Put_Line("the size of your input file.");
   -- open or prompt for input file
   begin
      if Argument_Count = 1 then
         open (Input_File, In_File, Argument(1));
      else
         loop
            Put("Please enter the input file name: ");
            Get_Line(Filename, Last);
            exit when Last in Filename'range;
         end loop;
         Put_Line("Next time use the command line.");
         Open(Input_File, In_File, Filename(Filename'First..Last));
      end if;
   exception
      when Name_Error | Use_error =>
         Put_Line("Invalid filename -- please try again.");
   end;

   -- Read the input file and write it back out in sorted chunks
   while not End_Of_File(Input_File) loop
      Get(Input_File, Input1);
      Skip_Line(Input_File);
      begin
         Insert(Input1, Tree);
      exception
         when Key_Error =>
            Skip_Line(Input_File);
      end;
      if BT.Count(Tree) = Temp_Size then -- dump tree
         Temp_Name1 := Tail(Trim(Natural'image(Temp_Count), Ada.Strings.Both), 8, '0') & ".tmp";
         Create(Temp_File1, Out_File, Temp_Name1);
         Write_Tree(Tree'Access, In_Order);
         Enqueue(Temp_Name1, Name_Queue);
         Close(Temp_File1);
         Clear(Tree);
         Temp_Count := Natural'Succ(Temp_Count);
      end if;
   end loop;
   -- Check for partial tree (leftovers; is there a better way to do this?)
   if not Empty(Tree) then
      Temp_Name1 := Tail(Trim(Natural'image(Temp_Count), Ada.Strings.Both), 8, '0') & ".tmp";
      Create(Temp_File1, Out_File, Temp_Name1);
      Write_Tree(Tree'Access, In_Order);
      Enqueue(Temp_Name1, Name_Queue);
      Close(Temp_File1);
      Clear(Tree);
      Temp_Count := Natural'Succ(Temp_Count);
   end if;

   -- Merge the files back together into one big sorted file
   while Q.Count(Name_Queue) > 1 loop
      Dequeue(Temp_Name1, Name_Queue);
      Dequeue(Temp_Name2, Name_Queue);
      Open(Temp_File1, In_File, Temp_Name1);
      Open(Temp_File2, In_File, Temp_Name2);
      Temp_Name3 := Tail(Trim(Natural'image(Temp_Count), Ada.Strings.Both), 8, '0') & ".tmp";
      Create(Temp_File3, Out_File, Temp_Name3);

      Get(Temp_File1, Input1);
      Get(Temp_File2, Input2);
      while not (End_Of_File(Temp_File1) and End_Of_File(Temp_File2)) loop
         if Input1 < Input2 then
            Put(Temp_File3, Input1, Width => 1);
            Skip_Line(Temp_File1);
            Get(Temp_File1, Input1);
         else
            Put(Temp_File3, Input2, Width => 1);
            Skip_Line(Temp_File2);
            Get(Temp_File2, Input2);
         end if;
         New_Line(Temp_File3);
      end loop;
      if End_Of_File(Temp_File1) then
         Put(Temp_File3, Input2, Width => 1);
         while not End_Of_File(Temp_File2) loop
            Get(Temp_File2, Input2);
            Skip_Line(Temp_File2);
            Put(Temp_File3, Input2, Width => 1);
            New_Line(Temp_File3);
         end loop;
      else
         Put(Temp_File3, Input1, Width => 1);
         while not End_Of_File(Temp_File1) loop
            Get(Temp_File1, Input1);
            Skip_Line(Temp_File1);
            Put(Temp_File3, Input1, Width => 1);
            New_Line(Temp_File3);
         end loop;
      end if;
      Enqueue(Temp_Name3, Name_Queue);
      Temp_Count := Natural'Succ(Temp_Count);
      Delete(Temp_File1);
      Delete(Temp_File2);
   end loop;

   -- copy final result to output file
   Create(Output_File, Out_File, "output.dat");
   while not End_Of_File(Temp_File3) loop
      Get(Temp_File3, Input1);
      Skip_Line(Temp_File3);
      Put(Output_File, Input1, Width => 1);
      Skip_Line(Output_File);
   end loop;
   Delete(Temp_File3);
   Close(Output_File);
end Merge_Sort;








