-----------------------------------------------------------------------------
--  Allan Hancock College
--  CS152, Spring 2000
--  Assignment Matrix Math
--  Stephen L Arnold
--  Procedure Matrix_Test
-----------------------------------------------------------------------------
-- This test procedure tests the matrix math package with several matrices
-- and the following operations: addition of two matrices, subtraction of
-- two matrices, multiplication of two matrices, and multiplying a scalar
-- and a matrix.  It also tests exception handling.
--
-- Test 1: Tests the addition of both similar and dis-similar matrices.
--         Prints results, checks answers against known results, and
--         prints pass/fail results and exception messages.
--
-- Test 2: Tests the subtraction of both similar and dis-similar matrices.
--         Prints results, checks answers against known results, and
--         prints pass/fail results and exception messages.
--
-- Test 3: Tests the multiplication of both similar and dis-similar matrices.
--         Prints results, checks answers against known results, and
--         prints pass/fail results and exception messages.
--
-- Test 4: Tests the multiplication of a scalar and a matrix.
--         Prints results, checks answers against known results, and
--         prints pass/fail results.
--
-- Test varaibles: A, B, C are matrices (of type matrix),
--                 and l, m, n are scalars (of type float).
-----------------------------------------------------------------------------

   with Matrix_Math ;               use Matrix_Math ;
   with Ada.Text_IO ;               use Ada.Text_IO ;
   with Ada.Float_Text_IO ;         use Ada.Float_Text_IO ;
   with Ada.Exceptions ;            use Ada.Exceptions ;

   procedure Matrix_Test is

            A : Matrix(11..13, 10..12) := ((8.0,     1.0,     6.0),
                                           (3.0,     5.0,     7.0),
                                           (4.0,     9.0,     2.0)) ;

            B : Matrix(1..3, 1..3)  :=  ((9.0,     3.0,     8.0),
                                         (5.0,     7.0,     9.0),
                                         (6.0,    15.0,     4.0)) ;

       AplusB : Matrix(1..3, 1..3)  :=  ((17.0,    4.0,    14.0),
                                         (8.0,    12.0,    16.0),
                                         (10.0,   24.0,     6.0)) ;

      BminusA : Matrix(1..3, 1..3)  :=  ((1.0,    2.0,     2.0),
                                         (2.0,    2.0,     2.0),
                                         (2.0,    6.0,     2.0)) ;

            C : Matrix(1..4, 1..3)  :=  ((4.0,    8.0,     1.0),
                                         (7.0,    6.0,     3.0),
                                         (8.0,    9.0,     5.0),
                                         (2.0,    4.0,     1.0)) ;

      CtimesA : Matrix(1..4, 1..3)  :=  ((60.0,    53.0,     82.0),
                                         (86.0,    64.0,     90.0),
                                         (111.0,   98.0,    121.0),
                                         (32.0,    31.0,     42.0)) ;

      mtimesA : Matrix(1..3, 1..3)  :=  ((32.0,     4.0,     24.0),
                                         (12.0,    20.0,     28.0),
                                         (16.0,    36.0,      8.0)) ;

            l : Float := 2.0 ;
            m : Float := 4.0 ;
            n : Float := -1.0 ;


        negB  : Matrix(1..3, 1..3)  :=  (others => (others => 0.0)) ;
        temp  : Matrix(1..3, 1..3)  :=  (others => (others => 0.0)) ;

   -----------------------------------------------------------------------
      procedure Put_Array (Float_array : in Matrix) is
      begin
         for I in Float_Array'first(1)..Float_Array'last(1) loop
            for J in Float_Array'first(2)..Float_Array'last(2) loop
               Put(Float_Array(I, J), 0, 0, 0) ;
               Put(" ") ;
            end loop ;
            New_line ;
         end loop ;
      end Put_Array ;

   -----------------------------------------------------------------------

   begin

      Put_Line("The test variables are: ") ;
      Put_Line("A = ") ;
      Put_Array(A) ;
      New_Line ;
      Put_Line("B = ") ;
      Put_Array(B) ;
      New_Line ;
      Put_Line("C = ") ;
      Put_Array(C) ;
      New_Line ;
      Put("l = ") ;
      Put(l, 0, 0, 0) ;
      New_Line ;
      New_Line ;
      Put("m = ") ;
      Put(m, 0, 0, 0) ;
      New_Line ;
      New_Line ;
      Put("n = ") ;
      Put(n, 0, 0, 0) ;
      New_Line ;
      New_Line ;

   ----------------------------------- begin test 1 ----------------------
      Put_Line("Test 1:") ;
      Put_line("Is A + B correct?") ;
      if (A + B) = AplusB then
         Put_Line("Yes. Test 1.a passed") ;
      else
         Put_line("No. Test 1.a failed") ;
      end if ;

      New_Line ;

      Put_line("Is B + -B = 0?") ;
      negB := n * B ;
      if (B + negB) = temp then
         Put_Line("Yes. Test 1.b passed") ;
      else
         Put_line("No. Test 1.b failed") ;
      end if ;

      New_Line ;

      Put_line("Is A + B = B + A?") ;
      if (A + B) = (B + A) then
         Put_Line("Yes. Test 1.c passed") ;
      else
         Put_line("No. Test 1.c failed") ;
      end if ;

      New_Line ;

      Put_line("Is A + A = 2 * A?") ;
      if (A + A) = (l * A) then
         Put_Line("Yes. Test 1.d passed") ;
      else
         Put_line("No. Test 1.d failed") ;
      end if ;

      New_Line ;

      Put_line("A + C should raise an error.") ;
      begin
         temp := A + C ;
         Put_Line("If you see this, test 1.e failed.") ;
         exception
            when Error : BAD_DIMENSION =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("If you see this, test 1.e passed.") ;
      end ;

      New_Line ;

   ----------------------------------- end test 1 ------------------------

   ----------------------------------- begin test 2 ----------------------
      Put_Line("Test 2:") ;
      Put_line("Is B - A correct?") ;
      if (B - A) = BminusA then
         Put_Line("Yes. Test 2.a passed") ;
      else
         Put_line("No. Test 2.a failed") ;
      end if ;

      New_Line ;

      Put_line("Is A - B = -(B - A)?") ;
      temp := n * BminusA ;
      if (A - B) = temp then
         Put_Line("Yes. Test 2.b passed") ;
      else
         Put_line("No. Test 2.b failed") ;
      end if ;

      New_Line ;

      Put_line("Is A - A = 0?") ;
      temp := (others => (others => 0.0)) ;
      if (A - A) = temp then
         Put_Line("Yes. Test 2.c passed") ;
      else
         Put_line("No. Test 2.c failed") ;
      end if ;

      New_Line ;

      Put_line("A - C should raise an error.") ;
      Put_Line("The exception was: ") ;
      begin
         temp := A - C ;
         Put_Line("If you see this, test 2.d failed.") ;
         exception
            when Error : BAD_DIMENSION =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("If you see this, test 2.d passed.") ;
      end ;

      New_Line ;

   ----------------------------------- end test 2 ------------------------

   ----------------------------------- begin test 3 ----------------------
      Put_Line("Test 3") ;
      Put_line("Is C * A correct?") ;
      if (C * A) = CtimesA then
         Put_Line("Yes. Test 3.a passed") ;
      else
         Put_line("No. Test 3.a failed") ;
      end if ;

      New_Line ;

      Put_line("A * C should raise an error.") ;
      Put_Line("The exception was: ") ;
      begin
         temp := A * C ;
         Put_Line("If you see this, test 3.b failed.") ;
         exception
            when Error : BAD_DIMENSION =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("If you see this, test 3.b passed.") ;
      end ;

      New_Line ;

   ----------------------------------- end test 3 ------------------------

   ----------------------------------- begin test 4 ----------------------
      Put_Line("Test 4") ;
      Put_line("Is m * A correct?") ;
      if (m * A) = mtimesA then
         Put_Line("Yes. Test 4.a passed") ;
      else
         Put_line("No. Test 4.a failed") ;
      end if ;

      New_Line ;

      Put_line("Does m * A = A * m?") ;
      if (m * A) = (A * m) then
         Put_Line("Yes. Test 4.b passed") ;
      else
         Put_line("No. Test 4.b failed") ;
      end if ;

      New_Line ;

   ----------------------------------- end test 4 ------------------------

      exception

         when Error : others =>

            Put_Line("Something is terribly wrong. There was as unknown exception: ") ;
            Put_Line(Exception_Name(Error)) ;
            Put_Line(Exception_Message(Error)) ;

   end Matrix_Test;
