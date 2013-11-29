-----------------------------------------------------------------------------
--  Allan Hancock College
--  CS152, Spring 2000
--  Assignment Merge Sort
--  Stephen L Arnold
--  Procedure Merge_Sort
-----------------------------------------------------------------------------
--  Description: This program implements a merge sort using the Priority
--  Stacks/Queues and Dynamic List Manager packages.  It performs a partition
--  (or "chunk") phase, followed by a merge phase, to sort a large disk file.
--
--  You can enter an input filename on the command line, or the program
--  will prompt you for the name.  Output is written to the filename
--  'output.dat' so make sure your input file has a different name.
--  Also, the input file should have one integer value per line.  Duplicate
--  processing is not guaranteed to be correct (some will be left out).
-----------------------------------------------------------------------------

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Ada.Exceptions;                use Ada.Exceptions;

with Binary_Search_Tree;
with Priority_Queue_Manager;

Procedure Merge_Sort is

   type Data_Record is
      record
         Data : Integer;
         Used : Boolean := False;
      end record;

   subtype DOS8_3 is String(1..12);

   Input_File   : File_Type;
   Output_File  : File_Type;
   Filename     : String(1..512);
   Last         : Natural;
   Input        : Integer;
   Input1       : Data_Record;
   Input2       : Data_Record;
   Done1        : Boolean := false;
   Done2        : Boolean := false;
   Temp_File1   : File_Type;
   Temp_File2   : File_Type;
   Temp_File3   : File_Type;
   Temp_Name1   : DOS8_3;
   Temp_Name2   : DOS8_3;
   Temp_Name3   : DOS8_3;
   Temp_Count   : Natural  := 0;
   Temp_Size    : Positive := 100;

   -- Priority functions for Queue and Tree packages.

   function Identity(Element : in Integer) return Integer is
   begin --Identity
      return Element;
   end Identity;

   function P(Filename : in DOS8_3) return Natural is
   begin --P
      return Natural'Value(Filename(Filename'First..Index(Filename, ".")-1));
   end P;

   function P2(Element : in Data_Record) return Integer is
   begin --P2
      return Element.Data;
   end P2;

   package BT is new Binary_Search_Tree(Integer, Integer, Identity, "=", "<");  use BT;
   Tree        : aliased BST;

   package Q  is new Priority_Queue_Manager(DOS8_3, Natural, P, ">");           use Q;
   Name_Queue  : Q.Priority_Queue_Type;

   package DQ is new Priority_Queue_Manager(Data_Record, Integer, P2, ">");     use DQ;
   DataQ1      : DQ.Priority_Queue_Type;
   DataQ2      : DQ.Priority_Queue_Type;

   -----------------------------------------------------------------------------------
   procedure Load(File_Handle      : in     File_Type;
                            DataQ  : in out DQ.Priority_Queue_Type;
                            Done   : out    Boolean) is

      -- reads from open file given by file_type and loads given queue with at
      -- most 10 data_records.  Done flag indicates when file has been read.

      Input : Data_Record;

   begin --Load
      Done := False;
      for I in 1..10 loop
         if End_Of_File(File_Handle) then
            Done := True;
         else
            Get(File_Handle, Input.Data);
            DQ.Enqueue(Input, DataQ);
            Skip_Line(File_Handle);
         end if;
         exit when End_Of_File(File_Handle);
      end loop;
      if End_Of_File(File_Handle) then
         Done := True;
      end if;
   end Load;

   ---------------------------------------------------------------------------------
   procedure Put(I : in Integer; Cont : out Boolean) is
   begin -- Put
      Put(Temp_File1,I,Width => 1);
      New_Line(Temp_File1);
      Cont := True;
   end Put;

   procedure Write_Tree is new Traverse(Put);

---------------------------------------------------------------------------------
begin --Merge_Sort

   Put_Line("You'll need free disk space equal to a little more than twice ");
   Put_Line("the size of your input file.  Duplicate values are skipped.");
   Put_Line("Sorted data is written to file OUTPUT.DAT");
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
      Get(Input_File, Input);
      Skip_Line(Input_File);
      begin
         Insert(Input, Tree);
      exception
         when Key_Error =>
            Skip_Line(Input_File);
            Put_Line("Duplicate value: " & Integer'Image(Input));
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
   -- Start by grabbing 2 files off the Name_Queue and creating the output file
   loop
      Dequeue(Temp_Name1, Name_Queue);
      Dequeue(Temp_Name2, Name_Queue);
      Temp_Name3 := Tail(Trim(Natural'image(Temp_Count), Ada.Strings.Both), 8, '0') & ".tmp";
      Open(Temp_File1, In_File, Temp_Name1);
      Open(Temp_File2, In_File, Temp_Name2);
      Create(Temp_File3, Out_File, Temp_Name3);
      -- Load the intial data into the Data_Queues
      Load(Temp_File1, DataQ1, Done1);
      Load(Temp_File2, DataQ2, Done2);
      loop    -- Loop until both files are done
         if (DQ.Empty(DataQ1) and not Done1) then
            Load(Temp_File1, DataQ1, Done1);
         end if;
         if (DQ.Empty(DataQ2) and not Done2) then
            Load(Temp_File2, DataQ2, Done2);
         end if;
         loop   -- Loop through Data_Queues
            if not (DQ.Empty(DataQ1) and DQ.Empty(DataQ2)) then
               Input1 := DQ.Front(DataQ1);
               Input2 := DQ.Front(DataQ2);
               if Input1.Data < Input2.Data then
                  DQ.Dequeue(Input1, DataQ1);
                  Put(Temp_File3, Input1.Data, 1);
               else
                  DQ.Dequeue(Input2, DataQ2);
                  Put(Temp_File3, Input2.Data, 1);
               end if;
               New_Line(Temp_File3);
            end if;
            exit when DQ.Empty(DataQ1) or DQ.Empty(DataQ2);
         end loop;  -- Pick up remnants
            if (Done1) then
               while not DQ.Empty(DataQ2) loop
                  DQ.Dequeue(Input2, DataQ2);
                  Put(Temp_File3, Input2.Data, 1);
                  New_Line(Temp_File3);
               end loop;
            end if;
            if (Done2) then
               while not DQ.Empty(DataQ1) loop
                  DQ.Dequeue(Input1, DataQ1);
                  Put(Temp_File3, Input1.Data, 1);
                  New_Line(Temp_File3);
               end loop;
            end if;
         exit when Done1 and then Done2;
      end loop;
      Enqueue(Temp_Name3, Name_Queue);
      Temp_Count := Natural'Succ(Temp_Count);
      Delete(Temp_File1);
      Delete(Temp_File2);
      Close(Temp_File3);
      exit when Q.Count(Name_Queue) = 1;
   end loop;
   -- copy final result to output file
   Dequeue(Temp_Name1, Name_Queue);
   Open(Temp_File1, In_File, Temp_Name1);
   Create(Output_File, Out_File, "output.dat");
   while not End_Of_File(Temp_File1) loop
      Get(Temp_File1, Input);
      Skip_Line(Temp_File1);
      Put(Output_File, Input, Width => 1);
      New_Line(Output_File);
   end loop;
   Delete(Temp_File1);
   Close(Output_File);
   exception
      when Name_Error =>
         Put_Line("Specified filename does not exist");
end Merge_Sort;








