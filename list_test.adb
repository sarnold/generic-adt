----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment List Package (Array Implementation)
-- Stephen L Arnold
-- package body List_Test
----------------------------------------------------------------------------
-- Description: This package tests the list_manager package.
-- 
-- It tests all procedures and function, as well as each exception.  The
-- test variables are lists of integers, but other types should also work.
-- Exceptions raised in procedure process are propagted.
--
-- Exception tests performed:
--     Exceptions:        state_error      cursor_error       overflow
--
--     proc/fun:           Insert                              Insert
--                                          Replace
--                         Remove           Remove
--                         Clear
--                         Traverse
--                        [process]
--                         Move             Move
--                         Move             Move (3)
--                                          Current_Item
--     
----------------------------------------------------------------------------
   with Ada.Text_IO ;                        use Ada.Text_IO ;
   with Ada.Integer_Text_IO ;                use Ada.Integer_Text_IO ;
   with Ada.Exceptions ;                     use Ada.Exceptions ;
   with Ada.Numerics.Discrete_Random ;
--   with Ada.Command_Line ;                 use Ada.Command_Line ;
--   with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
--   with Ada.Strings.Maps.Constants;        use Ada.Strings.Maps.Constants;

   with List_Manager ;
--   with Minimum_Float_String_Width ;

   procedure List_Test is
      package L is new List_Manager(Integer, 10) ;    use L ;
      package Boolean_IO is new Ada.Text_IO.Enumeration_IO (enum => Boolean) ;
      subtype Small_Positive is positive range 1..20 ;
      package Random_Numbers is new Ada.Numerics.Discrete_Random(Small_Positive) ;
      Gen : Random_Numbers.Generator ;
      n   : Small_Positive ;
      
      procedure Put(I : in out Integer; Cont : out Boolean) is
      begin -- Put
	 Put_Line(Integer'Image(I)) ;
	 Cont := True ;
      end Put ;
      
      procedure Write_List is new L.Traverse(Put) ;
      
      A : L.List_Type ;
      B : L.List_Type ;
      C : L.List_Type ;
      M : Integer ;
      T : Boolean := True ;

   begin -- List_Test
      
      Put_Line("Creating empty lists...") ;
      Put_Line("Count(A) = " & Natural'Image(L.Count(A))) ;
      Put_Line("Count(B) = " & Natural'Image(L.Count(B))) ;
      Put_Line("Count(C) = " & Natural'Image(L.Count(C))) ;
      New_Line ;
      
      Put_Line("Checking empty status...") ;
      Put("Empty(A) = ") ; Boolean_IO.Put(L.Empty(A)) ; New_Line ;
      Put("Empty(B) = ") ; Boolean_IO.Put(L.Empty(B)) ; New_Line ;
      Put("Empty(C) = ") ; Boolean_IO.Put(L.Empty(C)) ; New_Line ;
      New_Line ;
      
      Put_Line("Filling lists with 8 random integers...") ;
      Random_Numbers.Reset(Gen) ;
      for I in 1..8 loop
	 n := Random_Numbers.Random(Gen) ;
	 L.Insert(Item => n ,List => A) ;
	 n := Random_Numbers.Random(Gen) ;
	 L.Insert(Item => n ,List => B) ;
	 n := Random_Numbers.Random(Gen) ;
	 L.Insert(Item => n ,List => C) ;
      end loop ;
      New_Line ;
      
      Put_Line("Dumping list contents...") ;
      Put_Line("Contents of A:") ;
      L.Move(A, L.At_Start) ;
      Write_List(A, L.Forward) ;
      Put_Line("Contents of B:") ;
      L.Move(B, L.At_Start) ;
      Write_List(B, L.Forward) ;
      Put_Line("Contents of C:") ;
      L.Move(C, L.At_Start) ;
      Write_List(C, L.Forward) ;
      New_Line ;
      
      Put_Line("Testing procedure Insert...") ;
      Put_Line("Inserting the value 1 at the head of A.") ;
      L.Insert(1, A, L.At_Start) ;
      L.Move(A, L.At_Start) ;
      Write_List(A, L.Forward) ;
      Put_Line("Inserting the value 1 at the tail of A.") ;
      L.Insert(1, A, L.At_End) ;
      L.Move(A, L.At_Start) ;
      Write_List(A, L.Forward) ;
      New_Line ;
      
      Put_Line("Count(A) = " & Natural'Image(L.Count(A))) ;
      Put_Line("Count(B) = " & Natural'Image(L.Count(B))) ;
      Put_Line("Count(C) = " & Natural'Image(L.Count(C))) ;
      New_Line ;
      
      Put_Line("Inserting the value 10 before the tail of B.") ;
      Move(B, L.At_End) ;
      Insert(10, B, L.Before) ;
      Move(B, L.At_Start) ;
      Write_List(B, L.Forward) ;
      Put_Line("Inserting the value 10 after the head of B.") ;
      Move(B, L.At_Start) ;
      Insert(10, B, L.After) ;
      Move(B, L.At_Start) ;
      Write_List(B, L.Forward) ;
      New_Line ;
      
      Put_Line("Testing procedure Remove...") ;
      Put_Line("Removing the value 1 at the head of A.") ;
      Move(A, L.At_Start) ;
      Remove(A, N) ;
      Put_Line("Item is  " & Positive'Image(N)) ;
      Write_List(A, L.Forward) ;
      Move(A, L.At_Start) ;
      Put_Line("Move to head of A.") ;
      Put_Line("Getting new current item of A...") ;
      Put_Line("Item is  " & Positive'Image(Current_Item(A))) ;
      Put_Line("Removing the value 1 at the tail of A.") ;
      Move(A, L.At_End) ;
      Remove(A, M) ;
      Put_Line("Item is  " & Positive'Image(M)) ;
      Move(A, L.At_Start) ;
      Write_List(A, L.Forward) ;
      Put_Line("Getting new current item of A...") ;
      Put_Line("Item is  " & Positive'Image(Current_Item(A))) ;
      New_Line ;
      
      Put_Line("Replacing second to last item of A with the value 20...") ;
      Move(A, L.Backward) ;
      Replace(20, N, A) ;
      Put_Line("Old item was  " & Positive'Image(N)) ;
      Put_Line("New item is  " & Positive'Image(Current_Item(A))) ;
      New_Line ;
            
      Put_Line("Count(A) = " & Natural'Image(L.Count(A))) ;
      Put_Line("Count(B) = " & Natural'Image(L.Count(B))) ;
      Put_Line("Count(C) = " & Natural'Image(L.Count(C))) ;
      New_Line ;
      
      Put("Is A = B?  ") ; Boolean_IO.Put(A = B) ; New_Line ;
      Put("Is A = A?  ") ; Boolean_IO.Put(A = A) ; New_Line ;
      Put("Is A = C?  ") ; Boolean_IO.Put(A = C) ; New_Line ;
      New_Line ;
      
      Put_Line("Clearing C.") ;
      Clear(C) ;
      Put_Line("Count(C) = " & Natural'Image(L.Count(C))) ;
      Put("Is cursor B at the end?  ") ; Boolean_IO.Put(At_End (B)) ; New_Line ;
      Put("Is cursor C at the end?  ") ; Boolean_IO.Put(At_End (C)) ; New_Line ;
      Put("Is cursor C at the start?  ") ; Boolean_IO.Put(At_Start (C)) ; New_Line ;
      Move(A, L.At_Start) ;
      Put("Is cursor A at the start?  ") ; Boolean_IO.Put(At_Start (A)) ; New_Line ;
      New_Line ;
      
      Put_Line("Beginning exception tests.") ;
      begin
	 Put_Line("Getting current item of C...") ;
	 Put_Line("Current item of C = " & Natural'Image(L.Current_Item(C))) ;
         Put_Line("This test failed.") ;
	 
         exception
            when Error : Cursor_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Current_Item test passed.") ;
      end ;
      New_Line ;

      begin
	 Put_Line("Replacing current item of C...") ;
	 Replace(20, N, C) ;
	 Put_Line("Replaced item of C = ") ;
         Put_Line("This test failed.") ;
	 
         exception
            when Error : Cursor_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Replace test passed.") ;
      end ;
      New_Line ;

      begin
	 Put_Line("Removing current item of C.") ;
	 Remove(C, N) ;
         Put_Line("This test failed.") ;
	 
         exception
            when Error : Cursor_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Remove test passed.") ;
      end ;
      New_Line ;

      begin
	 Put_Line("Moving cursor of C to head.") ;
	 Move(C, At_Start) ;
         Put_Line("This test failed.") ;
	 
         exception
            when Error : Cursor_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Move test passed.") ;
      end ;
      New_Line ;

      begin
	 Put_Line("Moving cursor of C forward.") ;
	 Move(C, Forward) ;
         Put_Line("This test failed.") ;
	 
         exception
            when Error : Cursor_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Move test passed.") ;
      end ;
      New_Line ;

      begin
	 Put_Line("Moving cursor of A forward from end.") ;
	 Move(A, At_End) ;
	 Move(A, Forward) ;
         Put_Line("This test failed.") ;
	 
         exception
            when Error : Cursor_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Move test passed.") ;
      end ;
      New_Line ;

      begin
	 Put_Line("Moving cursor of A backward from start.") ;
	 Move(A, At_Start) ;
	 Move(A, Backward) ;
         Put_Line("This test failed.") ;
	 
         exception
            when Error : Cursor_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Move test passed.") ;
      end ;
      New_Line ;

      Put_Line("Filling C to 11 elements (Max_Size = 10)...") ;
      Random_Numbers.Reset(Gen) ;
      begin
	 for I in 1..11 loop
	    n := Random_Numbers.Random(Gen) ;
	    L.Insert(Item => N, List => C) ;
	 end loop ;
         Put_Line("This test failed.") ;
         exception
            when Error : Overflow =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Overflow test passed.") ;
      end ;
      New_Line ;
      
      
      exception

         when Error : others =>

            Put_Line("Something is terribly wrong. There was as unhandled exception: ") ;
            Put_Line(Exception_Name(Error)) ;
            Put_Line(Exception_Message(Error)) ;

    end List_Test ;
