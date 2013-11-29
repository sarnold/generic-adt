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

with Dynamic_List_Manager ;

generic
   type Element_Type is private ;
   type Priority_Type is limited private ;
   with function Priority(Item : in Element_Type) return Priority_Type ;
   with function "<"(Left, Right : in Priority_Type) return Boolean ;

package Priority_Queue_Manager is

   type Priority_Queue_Type is limited private ;

   Overflow     :  exception;  -- Rasied when queue space runs out.
   Underflow    :  exception;  -- Rasied when retrieving from empty queue.


   -------------------------------------------------------------------------
   procedure Enqueue(Item : in Element_Type; Q : in out Priority_Queue_Type) ;

   -- Adds items to Queue Q.  The position of the item within the queue
   -- is determined by the item's relative priority to the items already
   -- in the queue.

   -- Exceptions:
   --   Overflow    Item could not be added to Q.



   -------------------------------------------------------------------------
   procedure Dequeue(Item : out Element_Type; Q : in out Priority_Queue_Type) ;

   -- Removes items from the front of the queue.

   -- Exceptions:
   --   Underflow    Queue is empty.



   -------------------------------------------------------------------------
   function Front(Q : in Priority_Queue_Type) return Element_Type ;

   -- Returns a copy of the item at the front of the queue Q.

   -- Exceptions:
   --   Underflow    Stack is empty.



   -------------------------------------------------------------------------
   function Empty(Q  : in Priority_Queue_Type) return Boolean ;

   -- Returns true if the Queue is empty and false otherwise.

   -- Exceptions:
   --    None.



   -------------------------------------------------------------------------
   function Count(Q  : in Priority_Queue_Type) return Natural ;

   -- Returns the number of items in queue Q.  If the queue is empty,
   -- zero is returned.

   -- Exceptions:
   --    None.



   -------------------------------------------------------------------------
   procedure Clear(Q   : in out Priority_Queue_Type) ;

   -- Removes all items from queue Q.  If the queue is empty, the procedure
   -- does nothing.

   -- Exceptions:
   --    None.



   -------------------------------------------------------------------------
   function "="(Left, Right  : in Priority_Queue_Type) return Boolean ;

   -- Returns true if both queues have the same number of elements in the
   -- same order, and items at the same relative positions are equal.
   -- Returns false otherwise.

   -- Exceptions:
   --    None.

private

   package LM is new Dynamic_List_Manager(Element_Type) ;

   type Priority_Queue_Type is
      record
         List : aliased LM.List_Type ;
      end record ;

end Priority_Queue_Manager ;


