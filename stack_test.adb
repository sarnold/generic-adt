
----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment Stack Package (Dynamic_List_Manager Implementation)
-- Stephen L Arnold
-- procedure body Satck_Test
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

      A, B, C, D : aliased Stack_Type ;
      M : Integer := 0 ;

   begin -- Stack_Test



      exception

         when Error : others =>

            Put_Line("Something is terribly wrong. There was as unanticipated exception: ") ;
            Put_Line(Exception_Name(Error)) ;
            Put_Line(Exception_Message(Error)) ;

    end Stack_Test ;
