-------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment List Package (Array Implementation)
-- Stephen L Arnold
-- generic package body List_Manager
-------------------------------------------------------------
-- Description: This package provides services to save items in a list.
-- The items to be saved in the list are defined via the generic type
-- parameter Element_Type.  The maximum size of the list is defined by
-- the generic constant object Max_Size.  Max_Size is an optional parameter
-- in the instantiation and defaults to the value shown below.  The
-- abstraction of a list is defined by the exported data type List_Type.
--
-- This version of the package body implements List_Type as a record,
-- with an array to hold the user data.
-------------------------------------------------------------

with Ada.Exceptions;
package body List_Manager is
   
   -- exceptions:
   --     Overflow             Rasied when list space runs out.
   --     Cursor_Error     Rasied for invalid cursor operations.
   --     State_Error       Raised on invalid state change.
   
   ----------------------------------------------------------
   procedure Insert(
		    Item   : in     Element_Type;
		    List   : in out List_Type;
		    Place  : in     Position_Reference := At_End);
   
   -- Inserts Item into List at the Place specified.  After the insertion, the
   -- cursor is positioned at the newly inserted Item.  If the List is
   -- initially empty, then the Place parameter is ignored.
   
   -- Exceptions:
   --    Overflow      Item could not be inserted into List.
   --    State_Error   List is in a traversal.
   
   ----------------------------------------------------------
   procedure Replace(
		     New_Item   : in     Element_Type;
		     Old_Item   : in out Element_Type;
		     List       : in out List_Type);
   
   -- Replaces An Item in The List.  The Old_Item is returned and replaced with
   -- the New_Item. The item to be replaced is the one at the current cursor
   -- position.
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   
   ----------------------------------------------------------
   procedure Remove(
		    Item   : in out Element_Type;
		    List   : in out List_Type);
   
   -- Removes the Item in List at the current cursor position and returns
   -- Item.  The cursor is placed at the item immediately preceding the item
   -- being removed.  If the item to be removed was at the head of the list,
   -- the cursor is placed at the new head of the list.
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   --    State_Error      List is in a traversal.
   
   ----------------------------------------------------------
   procedure Clear(List   : in out List_Type) is
   
   -- Removes all items from the list.  If the list is empty, the procedure
   -- does nothing.
   
   begin  -- Clear
      if Empty(List) then
	 return ;
      elsif Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
               "Error using Clear.  List is in a traversal.") ;
      else
	 List.Count := 0 ;
      end if ;
   end Clear ;
   
   ----------------------------------------------------------
   function Empty(List   : in List_Type) return Boolean is
   
   -- Returns true if the List is empty and false otherwise.
   
   begin
      return List.Count = 0 ;
   end Empty ;
   
   ----------------------------------------------------------
   function Count(List   : in List_Type) return Natural is
   
   -- Returns the number of items in the List.  If List is empty, zero is
   -- returned.
   
   begin  -- Clear
      if Empty(List) then
	 return 0 ;
      else
	 return List.Count ;
      end if ;
   end Count ;
   ----------------------------------------------------------
   procedure Move(
		  List  : in out List_Type;
		  Place : in     EndPoint);
   
   -- Moves the cursor to the given place in List.  This allows the cursor to
   -- jump to a given endpoint of the List.
   
   -- Exceptions:
   --     Cursor_Error     List is empty.
   --     State_Error      List is in a traversal.
   
   ----------------------------------------------------------
   procedure Move(
		  List   : in out List_Type;
		  Course : in     Direction);
   
   -- Moves the cursor one item in List.  It only mves one item,
   -- in the direction provided by Course.
   
   -- Exceptions:
   --     Cursor_Error     Cursor is at beginning of List and Course is Backward.
   --     Cursor_Error     Cursor is at end of List and Course is Forward.
   --     Cursor_Error     List is empty.
   --     State_Error      List is in a traversal.
   
   
   
   ----------------------------------------------------------
   function At_Start(List   : in List_Type) return boolean;
   
   -- Returns true if the cursor for List is positioned at the start of List.
   -- Returns false otherwise, even if List is empty.
   
   -- Exceptions:
   --    None.
   
  ----------------------------------------------------------
   function At_End(List   : in List_Type) return boolean;
   
   -- Returns true if the cursor for List is positioned at the end of List.
   -- Returns false otherwise, even if List is empty.
   
   -- Exceptions:
   --    None.
   
  ----------------------------------------------------------
   function Current_Item(List   : in List_Type) return Element_Type;
   
   -- Returns the item in List at the current cursor position.
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   
   ----------------------------------------------------------
   function "="(Left, Right  : in List_Type) return boolean;
   
   -- Returns true if both lists have the same number of elements in the
   -- same order, and items at the same relative positions are equal.
   -- Returns false otherwise.
   
   -- Exceptions:
   --    None.
   
   ----------------------------------------------------------
   procedure Traverse(List : in out List_Type; Course : in Direction);
   
   -- Passive list iterator.  Iteration begins at the cursor position and
   -- advances in the direction indicated by Course until an endpoint of
   -- List is reached.  Upon return, the cursor is positioned at an endpoint
   -- of the list, unless the traversal was terminated early.  Early termination
   -- of the traversal can be accomplished by setting the Continue parameter
   -- of the generic formal procedure to false.  The traversal will cease, and
   -- the cursor is left positioned at whatever item was last processed.
   -- Unless an early termination of the traversal is desired, the Continue
   -- parameter should be set to true.  During a traversal, the state of List
   -- is not allowed to be changed.  A state change is defined to occur when
   -- items are either inserted or removed from the list, or when an attempt
   -- is made to adjust the cursor position for the List.  Replacing existing
   -- items is not considered a state change.  If List is empty, this procedure
   -- does nothing.
   
   -- Exceptions:
   --     State_Error     List is already in a traversal.
   --     State_Error     Attempt to change List state in procedure Process.
   --     Any exceptions raised within procedure Process are propagated.
   

end List_Manager;

