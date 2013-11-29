
----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment Stack & Queue Packages (Dynamic_List_Manager Implementation)
-- Stephen L Arnold
-- procedure body Stack_Test
----------------------------------------------------------------------------
-- Description: This procedure tests the Stack package.
--
-- It tests all procedures and functions, as well as most exceptions.  The
-- test variables are integers, but other types should also work.
--
--
-- Exception tests performed:
--     Exceptions:         underflow        overflow
--
--     proc/fun:             Pop              Push (not sure how to do this)
--                           Top
--
----------------------------------------------------------------------------
   with Ada.Text_IO ;                        use Ada.Text_IO ;
   with Ada.Integer_Text_IO ;                use Ada.Integer_Text_IO ;
   with Ada.Exceptions ;                     use Ada.Exceptions ;
   with Ada.Numerics.Discrete_Random ;
   --with Ada.Unchecked_Deallocation ;

   with Stack_Manager ;

   procedure Stack_Test is
      package S is new Stack_Manager(Integer) ;    use S ;
      package Boolean_IO is new Ada.Text_IO.Enumeration_IO (enum => Boolean) ;
      subtype Small_Positive is positive range 1..20 ;
      package Random_Numbers is new Ada.Numerics.Discrete_Random(Small_Positive) ;
      Gen : Random_Numbers.Generator ;
      n   : Small_Positive ;

      A, B, C, D : Stack_Type ;
      M : Integer := 0 ;

   begin -- Stack_Test

      Put_Line("Testing Count and Empty.") ;
      Put_Line("Creating empty stacks...") ;
      Put_Line("Count(A) = " & Natural'Image(S.Count(A))) ;
      Put_Line("Count(B) = " & Natural'Image(S.Count(B))) ;
      Put_Line("Count(C) = " & Natural'Image(S.Count(C))) ;
      New_Line ;

      Put_Line("Checking empty status...") ;
      Put("Empty(A) = ") ; Boolean_IO.Put(S.Empty(A)) ; New_Line ;
      Put("Empty(B) = ") ; Boolean_IO.Put(S.Empty(B)) ; New_Line ;
      Put("Empty(C) = ") ; Boolean_IO.Put(S.Empty(C)) ; New_Line ;
      New_Line ;

      Put_Line("Testing Push, Pop, and Top.") ;
      Put_Line("Filling stacks with 8 random integers...") ;
      New_Line ;
      Random_Numbers.Reset(Gen) ;
      Put("Checking contents of A.") ;
      New_Line ;
      for I in 1..8 loop
         n := Random_Numbers.Random(Gen) ;
         S.Push(n, A) ;
         Put("The number pushed was: " & Natural'Image(n)) ;
         New_Line ;
         Put("The actual number was: " & Natural'Image(S.Top(A))) ;
         New_Line ;
         n := Random_Numbers.Random(Gen) ;
         S.Push(n, B) ;
         n := Random_Numbers.Random(Gen) ;
         S.Push(n, C) ;
      end loop ;
      New_Line ;

      Put_Line("Popping the top two values of A.") ;
      S.Pop(m, A) ;
      Put_Line("The first number popped was: " & Natural'Image(m)) ;
      S.Pop(m, A) ;
      Put_Line("The second number popped was: " & Natural'Image(m)) ;
      New_Line ;

      Put_Line("Count(A) = " & Natural'Image(S.Count(A))) ;
      Put_Line("Count(B) = " & Natural'Image(S.Count(B))) ;
      Put_Line("Count(C) = " & Natural'Image(S.Count(C))) ;
      New_Line ;

      Put("Is A = B?  ") ; Boolean_IO.Put(A = B) ; New_Line ;
      Put("Is A = A?  ") ; Boolean_IO.Put(A = A) ; New_Line ;
      Put("Is B = C?  ") ; Boolean_IO.Put(B = C) ; New_Line ;
      New_Line ;

      Put_Line("Clearing C.") ;
      S.Clear(C) ;
      Put_Line("Count(C) = " & Natural'Image(S.Count(C))) ;
      New_Line ;

      Put_Line("Beginning exception tests...") ;
      begin
         Put_Line("Getting current item of C...") ;
         Put_Line("Top of C = " & Natural'Image(S.Top(C))) ;
         Put_Line("This test failed.") ;

         exception
            when Error : Underflow =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Function Top test passed.") ;
      end ;
      New_Line ;

      begin
         Put_Line("Popping current item of C.") ;
         S.Pop(m, C) ;
         Put_Line("This test failed.") ;

         exception
            when Error : Underflow =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Pop test passed.") ;
      end ;
      New_Line ;



   exception

      when Error : others =>

         Put_Line("Something is terribly wrong. There was as unanticipated exception: ") ;
         Put_Line(Exception_Name(Error)) ;
         Put_Line(Exception_Message(Error)) ;

    end Stack_Test ;
