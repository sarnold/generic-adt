----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment Stack and Queue Packages
-- Stephen L Arnold
-- generic package specification Stack_Manager
----------------------------------------------------------------------------
-- Description: This package provides services to save items in a stack.
-- The items to be saved in the stack are defined via the generic formal
-- type parameter Element_Type.  The stack grows dynamically and thus its
-- maximum size is limited only by available memory.  The abstraction
-- of a stack is defined by the exported data type Stack_Type.
----------------------------------------------------------------------------

with Ada.Exceptions ;

package body Stack_Manager is

   -------------------------------------------------------------------------
   procedure Push(Item : in Element_Type; S : in out Stack_Type) is

   -- Adds items to Stack S.

   -- Exceptions:
   --   Overflow    Item could not be added to S.

   begin -- Push
      LM.Insert(Item, S.List, LM.At_Start) ;
      exception
         when LM.Overflow =>
            Ada.Exceptions.Raise_Exception (Overflow'identity,
                        "Error using Push.  Not eneough free memory.") ;
   end Push ;


   -------------------------------------------------------------------------
   procedure Pop(Item : out Element_Type; S : in out Stack_Type) is

   -- Removes items from Stack S.

   -- Exceptions:
   --   Underflow    Stack is empty.

   begin --Pop
      if LM.Empty(S.List) then
         Ada.Exceptions.Raise_Exception (Underflow'identity,
                     "Error using Pop.  Stack is empty.") ;
      end if ;
      LM.Move(S.List, LM.At_Start) ;
      LM.Remove(Item, S.List) ;
   end Pop ;


   -------------------------------------------------------------------------
   function Top(S : in Stack_Type) return Element_Type is

   -- Returns a copy of the item at the top of the Stack S.

   -- Exceptions:
   --   Underflow    Stack is empty.

   begin --Top
      if LM.Empty(S.List) then
         Ada.Exceptions.Raise_Exception (Underflow'identity,
                     "Error using Top.  Stack is empty.") ;
      end if ;

      return LM.Current_Item(S.List) ;
   end Top ;


   -------------------------------------------------------------------------
   function Empty(S  : in Stack_Type) return Boolean is

   -- Returns true if the Stack is empty and false otherwise.

   -- Exceptions:
   --    None.

   begin -- Empty
      return LM.Empty(S.List) ;
   end Empty ;


   -------------------------------------------------------------------------
   function Count(S  : in Stack_Type) return Natural is

   -- Returns the number of items in stack S.  If the stack is empty,
   -- zero is returned.

   -- Exceptions:
   --    None.

   begin  -- Count
      return LM.Count(S.List) ;
   end Count ;


   -------------------------------------------------------------------------
   procedure Clear(S   : in out Stack_Type) is

   -- Removes all items from stack S.  If the stack is empty, the procedure
   -- does nothing.

   -- Exceptions:
   --    None.

   begin  -- Clear
      LM.Clear(S.List) ;
   end Clear ;


   -------------------------------------------------------------------------
   function "="(Left, Right  : in Stack_Type) return Boolean is

   -- Returns true if both stacks have the same number of elements in the
   -- same order, and items at the same relative positions are equal.
   -- Returns false otherwise.

   -- Exceptions:
   --    None.

   begin -- "="
      return LM."="(Left.List, Right.List) ;
   end "=" ;

end Stack_Manager ;



