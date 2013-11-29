----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment List Package (Dynamic Memory Allocation Implementation)
-- Stephen L Arnold
-- procedure body List_Test
----------------------------------------------------------------------------
-- Description: This procedure tests the list_manager package.
--
-- It tests all procedures and functions, as well as most exceptions.  The
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
   with Ada.Unchecked_Deallocation ;

   with Dynamic_List_Manager ;

   procedure D_List_Test is
      package L is new Dynamic_List_Manager(Integer) ;    use L ;
      package Boolean_IO is new Ada.Text_IO.Enumeration_IO (enum => Boolean) ;
      subtype Small_Positive is positive range 1..20 ;
      package Random_Numbers is new Ada.Numerics.Discrete_Random(Small_Positive) ;
      Gen : Random_Numbers.Generator ;
      n   : Small_Positive ;

      A, B, C, D : aliased List_Type ;
      M : Integer := 0 ;
      ns : array(1..8) of Integer := (8, 4, 3, 11, 13, 1, 10, 17) ;

      procedure Process(I : in out Integer; Cont : out Boolean) is
      begin -- Process
         begin
            Cont := True ;
            L.Insert(1, D) ;
            Put_Line("Failed insert during traversal test.") ;
         exception
            when Error : State_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Insert during traversal test passed.") ;
         end ;
         New_Line ;
         begin
            L.Replace(1, M, D) ;
            Put_Line("Failed replace during traversal test.") ;
         exception
            when Error : State_Error =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Replace during traversal test passed.") ;
         end ;
      end Process ;

      procedure Tr is new Traverse(Process) ;

      procedure Put(I : in out Integer; Cont : out Boolean) is
      begin -- Put
         Put_Line(Integer'Image(I)) ;
         Cont := True ;
      end Put ;

      procedure Write_List is new Traverse(Put) ;

   begin -- D_List_Test

      L.Insert(Item => 1 ,List => D) ;
      Put_Line("Beginning traversal exception tests...") ;
      Tr(D'access, Forward) ;
      New_Line ;

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

      Put_Line("Adding known values to A and removing them.") ;
      for I in 1..8 loop
         L.Insert(ns(I), A) ;
      end loop ;

      for I in 1..8 loop
         L.Remove(m, A) ;
         Put_Line("Item removed: " & Natural'Image(m)) ;
      end loop ;
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
      Move(A, At_Start) ;
      Write_List(A'access, Forward) ;
      Put_Line("Contents of B:") ;
      Move(B, At_Start) ;
      Write_List(B'access, Forward) ;
      Put_Line("Contents of C:") ;
      Move(C, At_Start) ;
      Write_List(C'access, Forward) ;
      New_Line ;

      Put_Line("Testing procedure Insert...") ;
      Put_Line("Inserting the value 1 at the head of A.") ;
      Insert(1, A, L.At_Start) ;
      Move(A, L.At_Start) ;
      Write_List(A'access, Forward) ;
      Put_Line("Inserting the value 1 at the tail of A.") ;
      Insert(1, A, L.At_End) ;
      Move(A, L.At_Start) ;
      Write_List(A'access, Forward) ;
      New_Line ;

      Put_Line("Count(A) = " & Natural'Image(L.Count(A))) ;
      Put_Line("Count(B) = " & Natural'Image(L.Count(B))) ;
      Put_Line("Count(C) = " & Natural'Image(L.Count(C))) ;
      New_Line ;

      Put_Line("Inserting the value 10 before the tail of B.") ;
      Move(B, L.At_End) ;
      Insert(10, B, L.Before) ;
      Move(B, L.At_Start) ;
      Write_List(B'access, Forward) ;
      Put_Line("Inserting the value 10 after the head of B.") ;
      Move(B, L.At_Start) ;
      Insert(10, B, L.After) ;
      Move(B, L.Backward) ;
      Write_List(B'access, Forward) ;
      New_Line ;

      Put_Line("Testing procedure Remove...") ;
      Put_Line("Removing the value 1 at the head of A.") ;
      Move(A, L.At_Start) ;
      Remove(N, A) ;
      Put_Line("Item is  " & Positive'Image(N)) ;
      Write_List(A'access, Forward) ;
      Move(A, L.At_Start) ;
      Put_Line("Move to head of A.") ;
      Put_Line("Getting new current item of A...") ;
      Put_Line("Item is  " & Positive'Image(Current_Item(A))) ;
      Put_Line("Removing the value 1 at the tail of A.") ;
      Move(A, L.At_End) ;
      Remove(M, A) ;
      Put_Line("Item is  " & Positive'Image(M)) ;
      Move(A, L.At_Start) ;
      Write_List(A'access, Forward) ;
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

      Put_Line("Beginning exception tests...") ;
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
         Remove(N, C) ;
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
      New_Line ;
      Put_Line("All tests passed!") ;
      New_Line ;
      New_Line ;

      exception

         when Error : others =>

            Put_Line("Something is terribly wrong. There was as unanticipated exception: ") ;
            Put_Line(Exception_Name(Error)) ;
            Put_Line(Exception_Message(Error)) ;

    end D_List_Test ;
