----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment Stack and Queue Packages
-- Stephen L Arnold
-- generic package body Stack_Manager
----------------------------------------------------------------------------
-- Description: This package provides services to save items in a stack.
-- The items to be saved in the stack are defined via the generic formal
-- type parameter Element_Type.  The stack grows dynamically and thus its
-- maximum size is limited only by available memory.  The abstraction
-- of a stack is defined by the exported data type Stack_Type.  This
-- package uses the generic package Dynamic_List_Manager to implement
-- the stack objects.
----------------------------------------------------------------------------

with Dynamic_List_Manager ;

generic
   type Element_Type is private ;

package Stack_Manager is

   type Stack_Type is limited private ;

   Overflow     :  exception;  -- Rasied when stack space runs out.
   Underflow    :  exception;  -- Rasied when retrieving from empty stack.


   -------------------------------------------------------------------------
   procedure Push(Item : in Element_Type; S : in out Stack_Type) ;

   -- Adds items to Stack S.

   -- Exceptions:
   --   Overflow    Item could not be added to S.



   -------------------------------------------------------------------------
   procedure Clear(S   : in out Stack_Type) ;

   -- Removes all items from stack S.  If the stack is empty, the procedure
   -- does nothing.

   -- Exceptions:
   --    None.



   -------------------------------------------------------------------------
   function Empty(S  : in Stack_Type) return Boolean ;

   -- Returns true if the Stack is empty and false otherwise.

   -- Exceptions:
   --    None.



   -------------------------------------------------------------------------
   function Count(S  : in Stack_Type) return Natural ;

   -- Returns the number of items in stack S.  If the stack is empty,
   -- zero is returned.

   -- Exceptions:
   --    None.



   -------------------------------------------------------------------------
   function "="(Left, Right  : in Stack_Type) return Boolean ;

   -- Returns true if both stacks have the same number of elements in the
   -- same order, and items at the same relative positions are equal.
   -- Returns false otherwise.

   -- Exceptions:
   --    None.

private

   package LM is new Dynamic_List_Manager(Element_Type) ;

   type Stack_Type is
      record
         List : LM.List_Type ;
      end record ;

end Stack_Manager ;

