
----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment Stack & Queue Packages (Dynamic_List_Manager Implementation)
-- Stephen L Arnold
-- procedure body Q_Test
----------------------------------------------------------------------------
-- Description: This procedure tests the Queue package.
--
-- It tests all procedures and functions, as well as most exceptions.  The
-- test variables are integers, but other types should also work.
--
--
-- Exception tests performed:
--     Exceptions:         underflow        overflow
--
--     proc/fun:            Dequeue          Enqueue (not sure how to do this)
--                          Front
--
----------------------------------------------------------------------------
   with Ada.Text_IO ;                        use Ada.Text_IO ;
   with Ada.Integer_Text_IO ;                use Ada.Integer_Text_IO ;
   with Ada.Exceptions ;                     use Ada.Exceptions ;
   with Ada.Numerics.Discrete_Random ;

   with Priority_Queue_Manager ;

   procedure Q_Test is

      function Identity(Element : in Integer) return Integer is
         begin --Identity
            return Element ;
         end Identity ;

      package Q is new Priority_Queue_Manager(Integer, Integer, Identity, "<") ;    use Q ;
      package Boolean_IO is new Ada.Text_IO.Enumeration_IO (enum => Boolean) ;
      subtype Small_Positive is positive range 1..20 ;
      package Random_Numbers is new Ada.Numerics.Discrete_Random(Small_Positive) ;
      Gen : Random_Numbers.Generator ;
      n   : Small_Positive ;

      A, B, C, D : Priority_Queue_Type ;
      M : Integer := 0 ;
      ns : array(1..8) of Integer := (8, 4, 3, 11, 13, 1, 10, 17) ;

   begin -- Queue_Test

      Put_Line("Testing Count and Empty.") ;
      Put_Line("Creating empty queues...") ;
      Put_Line("Count(A) = " & Natural'Image(Q.Count(A))) ;
      Put_Line("Count(B) = " & Natural'Image(Q.Count(B))) ;
      Put_Line("Count(C) = " & Natural'Image(Q.Count(C))) ;
      New_Line ;

      Put_Line("Checking empty status...") ;
      Put("Empty(A) = ") ; Boolean_IO.Put(Q.Empty(A)) ; New_Line ;
      Put("Empty(B) = ") ; Boolean_IO.Put(Q.Empty(B)) ; New_Line ;
      Put("Empty(C) = ") ; Boolean_IO.Put(Q.Empty(C)) ; New_Line ;
      Put("Empty(D) = ") ; Boolean_IO.Put(Q.Empty(D)) ; New_Line ;
      New_Line ;

      Put_Line("Adding known values to A and dequeueing them.") ;
      for I in 1..8 loop
         Q.Enqueue(ns(I), A) ;
      end loop ;

      for I in 1..8 loop
         Q.Dequeue(m, A) ;
         Put_Line("Dequeued item: " & Natural'Image(m)) ;
      end loop ;

      New_Line ;
      Put_Line("Testing Enqueue, Dequeue, and Front.") ;
      Put_Line("Filling other queues with 8 random integers...") ;
      New_Line ;
      Random_Numbers.Reset(Gen) ;
      Put("Checking contents of B.") ;
      New_Line ;
      for I in 1..8 loop
         n := Random_Numbers.Random(Gen) ;
         Q.Enqueue(n, B) ;
         Put("The number queued was: " & Natural'Image(n)) ;
         New_Line ;
         Put("The front of the queue is: " & Natural'Image(Q.Front(B))) ;
         New_Line ;
         n := Random_Numbers.Random(Gen) ;
         Q.Enqueue(n, C) ;
         n := Random_Numbers.Random(Gen) ;
         Q.Enqueue(n, D) ;
      end loop ;
      New_Line ;

      Put_Line("Dequeueing two values of B.") ;
      Q.Dequeue(m, B) ;
      Put_Line("The first number dequeued was: " & Natural'Image(m)) ;
      Q.Dequeue(m, B) ;
      Put_Line("The second number dequeued was: " & Natural'Image(m)) ;
      New_Line ;

      Put_Line("Count(A) = " & Natural'Image(Q.Count(A))) ;
      Put_Line("Count(B) = " & Natural'Image(Q.Count(B))) ;
      Put_Line("Count(C) = " & Natural'Image(Q.Count(C))) ;
      Put_Line("Count(D) = " & Natural'Image(Q.Count(D))) ;
      New_Line ;

      Put("Is B = C?  ") ; Boolean_IO.Put(C = B) ; New_Line ;
      Put("Is B = B?  ") ; Boolean_IO.Put(B = B) ; New_Line ;
      Put("Is C = D?  ") ; Boolean_IO.Put(D = C) ; New_Line ;
      New_Line ;

      Put_Line("Clearing D.") ;
      Q.Clear(D) ;
      Put_Line("Count(D) = " & Natural'Image(Q.Count(D))) ;
      New_Line ;

      Put_Line("Beginning exception tests...") ;
      begin
         Put_Line("Getting item from front of D...") ;
         Put_Line("Front of D = " & Natural'Image(Q.Front(D))) ;
         Put_Line("This test failed.") ;

         exception
            when Error : Underflow =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Front test passed.") ;
      end ;
      New_Line ;

      begin
         Put_Line("Dequeueing item of D.") ;
         Q.Dequeue(m, D) ;
         Put_Line("This test failed.") ;

         exception
            when Error : Underflow =>
               Put("The exception was: ") ;
               Put_Line(Exception_Name(Error)) ;
               Put("The exception message is: ") ;
               Put_Line(Exception_Message(Error)) ;
               Put_Line("Dequeue test passed.") ;
      end ;
      New_Line ;

   exception

      when Error : others =>

         Put_Line("Something is terribly wrong. There was as unanticipated exception: ") ;
         Put_Line(Exception_Name(Error)) ;
         Put_Line(Exception_Message(Error)) ;

    end Q_Test ;
