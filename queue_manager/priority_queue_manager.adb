----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment Stack and Queue Packages
-- Stephen L Arnold
-- generic package specification Priority_Queue_Manager
----------------------------------------------------------------------------
-- Description: This package provides services to save items in a prioritized
-- queue.  The items to be saved in the queue are defined via the generic formal
-- type parameter Element_Type.  Conceptually, each item of type Element_Type
-- contains all the necessary information to determine the priority of an item.
-- The priority information is extracted from a given item using the imported
-- function priority.  Two priorities are compared using the imported function
-- less than ("<").
--
-- The prioritized queues grow dynamically and thus their maximum size
-- is limited only by available memory.  The abstraction of a
-- prioritized queue is defined by the exported data type
-- Prioritized_Queue_Type.
----------------------------------------------------------------------------

with Ada.Exceptions ;

package body Priority_Queue_Manager is

   -------------------------------------------------------------------------
   procedure Enqueue(Item : in Element_Type; Q : in out Priority_Queue_Type) is

   -- Adds items to Queue Q.  The position of the item within the queue
   -- is determined by the item's relative priority to the items already
   -- in the queue.

   -- Exceptions:
   --   Overflow    Item could not be added to Q.

   begin -- Enqueue
      if LM.Empty(Q.List) then
         LM.Insert(Item, Q.List) ;
      else
         LM.Move(Q.List, LM.At_Start) ;
         while Priority(Item) < Priority(LM.Current_Item(Q.List)) loop
            exit when LM.At_End(Q.List) ;
            LM.Move(Q.List, LM.Forward) ;
         end loop ;
         if Priority(Item) < Priority(LM.Current_Item(Q.List)) then
            LM.Insert(Item, Q.List, LM.After) ;
         else
            LM.Insert(Item, Q.List, LM.Before) ;
         end if ;
      end if ;
      LM.Move(Q.List, LM.At_Start) ;
      exception
         when LM.Overflow =>
            LM.Move(Q.List, LM.At_Start) ;
            Ada.Exceptions.Raise_Exception (Overflow'identity,
                        "Error using Enqueue.  Not eneough free memory.") ;
   end Enqueue ;


   -------------------------------------------------------------------------
   procedure Dequeue(Item : out Element_Type; Q : in out Priority_Queue_Type) is

   -- Removes items from the front of the queue.

   -- Exceptions:
   --   Underflow    Queue is empty.

   begin --Dequeue
      if LM.Empty(Q.List) then
         Ada.Exceptions.Raise_Exception (Underflow'identity,
                     "Error using Dequeue.  Queue is empty.") ;
      end if ;
      LM.Move(Q.List, LM.At_Start) ;
      LM.Remove(Item, Q.List) ;
   end Dequeue ;


   -------------------------------------------------------------------------
   function Front(Q : in Priority_Queue_Type) return Element_Type is

   -- Returns a copy of the item at the front of the queue.

   -- Exceptions:
   --   Underflow    Stack is empty.

   begin --Front
      if LM.Empty(Q.List) then
         Ada.Exceptions.Raise_Exception (Underflow'identity,
                     "Error using Front.  Queue is empty.") ;
      end if ;

      return LM.Current_Item(Q.List) ;
   end Front ;


   -------------------------------------------------------------------------
   function Empty(Q  : in Priority_Queue_Type) return Boolean is

   -- Returns true if the Queue is empty and false otherwise.

   -- Exceptions:
   --    None.

   begin -- Empty
      return LM.Empty(Q.List) ;
   end Empty ;


   -------------------------------------------------------------------------
   function Count(Q  : in Priority_Queue_Type) return Natural is

   -- Returns the number of items in queue Q.  If the queue is empty,
   -- zero is returned.

   -- Exceptions:
   --    None.

   begin  -- Count
      return LM.Count(Q.List) ;
   end Count ;


   -------------------------------------------------------------------------
   procedure Clear(Q   : in out Priority_Queue_Type) is

   -- Removes all items from queue Q.  If the queue is empty, the procedure
   -- does nothing.

   -- Exceptions:
   --    None.

   begin  -- Clear
      LM.Clear(Q.List) ;
   end Clear ;


   -------------------------------------------------------------------------
   function "="(Left, Right  : in Priority_Queue_Type) return Boolean is

   -- Returns true if both queues have the same number of elements in the
   -- same order, and items at the same relative positions are equal.
   -- Returns false otherwise.

   -- Exceptions:
   --    None.

   begin -- "="
      return LM."="(Left.List, Right.List) ;
   end "=" ;


end Priority_Queue_Manager ;


