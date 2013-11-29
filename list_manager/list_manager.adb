----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment List Package (Array Implementation)
-- Stephen L Arnold
-- generic package body List_Manager
----------------------------------------------------------------------------
-- Description: This package provides services to save items in a list.
-- The items to be saved in the list are defined via the generic type
-- parameter Element_Type.  The maximum size of the list is defined by
-- the generic constant object Max_Size.  Max_Size is an optional parameter
-- in the instantiation and defaults to the value shown below.  The
-- abstraction of a list is defined by the exported data type List_Type.
--
-- This version of the package body implements List_Type as a record,
-- with an array to hold the user data.
----------------------------------------------------------------------------

with Ada.Exceptions;
package body List_Manager is
      
   -------------------------------------------------------------------------
   procedure Insert(
		    Item   : in     Element_Type;
		    List   : in out List_Type;
		    Place  : in     Position_Reference := At_End) is
   
   -- Inserts Item into List at the Place specified.  After the insertion, the
   -- cursor is positioned at the newly inserted Item.  If the List is
   -- initially empty, then the Place parameter is ignored.
   
   -- Exceptions:
   --    Overflow      Item could not be inserted into List.
   --    State_Error   List is in a traversal.
   
   begin -- Insert
      if List.Count = Max_Size then
	 Ada.Exceptions.Raise_Exception (Overflow'identity,
			 "Error using Insert.  List is already full.") ;
      elsif List.Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			 "Error using Insert.  List is already in a traversal.") ;
      else
	 if List.Count = 0 then
	    List.List_Data(1) := Item ;
	    List.Head := 1 ;
	 elsif Place = At_End then
	    List.List_Data(Positive'Succ(List.Count)) := Item ;
	 elsif Place = Before then
	    List.List_Data(Positive'Succ(List.Cursor)..Positive'Succ(List.Tail)) :=
	      List.List_Data(List.Cursor..List.Tail) ;
	    List.List_Data(List.Cursor) := Item ;
	 elsif Place = After then
	    Move(List, Forward) ;
	    List.List_Data(Positive'Succ(List.Cursor)..Positive'Succ(List.Tail)) :=
	      List.List_Data(List.Cursor..List.Tail) ;
	    List.List_Data(List.Cursor) := Item ;
	 else
	    List.List_Data(Positive'Succ(List.Head)..Positive'Succ(List.Tail)) :=
	      List.List_Data(List.Head..List.Tail) ;
	    List.List_Data(List.Head) := Item ;
	 end if ;
	 List.Count := List.Count + 1 ;
	 List.Tail := List.Count ;
      end if ;
   end Insert ;
	    
   -------------------------------------------------------------------------
   procedure Replace(
		     New_Item   : in     Element_Type;
		     Old_Item   : in out Element_Type;
		     List       : in out List_Type) is
   
   -- Replaces an Item in The List.  The Old_Item is returned and replaced with
   -- the New_Item. The item to be replaced is the one at the current cursor
   -- position.
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   
   begin -- Replace
      
      if Empty(List) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
			 "Error using Replace.  List is empty.") ;
      else
	 Old_Item := List.List_Data(List.Cursor) ;
	 List.List_Data(List.Cursor) := New_Item ;
      end if ;
   end Replace;
   
   -------------------------------------------------------------------------
   procedure Remove(
		    List   : in out List_Type;
		    Item   :    out Element_Type) is
   
   -- Removes the Item in List at the current cursor position and returns
   -- Item.  The cursor is placed at the item immediately preceding the item
   -- being removed.  If the item to be removed was at the head of the list,
   -- the cursor is placed at the new head of the list.
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   --    State_Error      List is in a traversal.
   
   begin -- Remove
       if Empty(List) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
			 "Error using Remove.  List is empty.") ;
       elsif List.Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			 "Error using Remove.  List is in a traversal.") ;
       elsif List.Count = 1 then
	  Item := List.List_Data(List.Head) ;
	  Clear(List) ;
       else
	  if List.Cursor = List.Tail then
	     Item := List.List_Data(List.Cursor) ;
	  else
	     Item := List.List_Data(List.Cursor) ;
	     List.List_Data(List.Cursor..Positive'Pred(List.Tail)) :=
	       List.List_Data(Positive'Succ(List.Cursor)..List.Tail) ;
	  end if ;
	  if List.Cursor /= List.Head then
	     Move(List, Backward) ;
	  end if ;
       	  List.Count := Positive'Pred(List.Count) ;
	  List.Tail := List.Count ;
       end if ;
   end Remove;
   
   -------------------------------------------------------------------------
   procedure Clear(List   : in out List_Type) is
   
   -- Resets list attributes to zero (removes all items from the list). 
   -- If the list is empty, the procedure does nothing.
   
   -- Exceptions:
   --    State_Error       List is in a traversal.
   
   begin  -- Clear
      if not Empty(List) then
	 if List.Traversing then
	    Ada.Exceptions.Raise_Exception (State_Error'identity,
			    "Error using Clear.  List is in a traversal.") ;
	 else
	    List.Count := 0 ;
	    List.Head := 0 ;
	    List.Tail := 0 ;
	    List.Cursor := 0 ;
	    List.Traversing := False ;
	 end if ;
      end if ;
   end Clear ;
   
   -------------------------------------------------------------------------
   function Empty(List   : in List_Type) return Boolean is
   
   -- Returns true if the List is empty and false otherwise.
   
   -- Exceptions:
   --    None.
   
   begin -- Empty
      return List.Count = 0 ;
   end Empty ;
   
   -------------------------------------------------------------------------
   function Count(List   : in List_Type) return Natural is
   
   -- Returns the number of items in the List.  If List is empty, zero is
   -- returned.
   
   -- Exceptions:
   --    None.
   
   begin  -- Count
      return List.Count ;
   end Count ;
   
   -------------------------------------------------------------------------
   procedure Move(
		  List  : in out List_Type;
		  Place : in     EndPoint) is
   
   -- Moves the cursor to the given place in List.  This allows the cursor to
   -- jump to a given endpoint of the List.
   
   -- Exceptions:
   --     Cursor_Error     List is empty.
   --     State_Error      List is in a traversal.
   
   begin -- Move
      if Empty(List) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
               "Error using Move.  List is empty.") ;
      elsif List.Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
               "Error using Move.  List is in a traversal.") ;
      elsif Place = At_End  then
	 List.Cursor := List.Tail ;
      else
	 List.Cursor := List.Head ;
      end if ;
   end Move ;
   
   -------------------------------------------------------------------------
   procedure Move(
		  List   : in out List_Type;
		  Course : in     Direction) is
   
   -- Moves the cursor one item in List.  It only mves one item,
   -- in the direction provided by Course.
   
   -- Exceptions:
   --     Cursor_Error     Cursor is at beginning of List and Course is Backward.
   --     Cursor_Error     Cursor is at end of List and Course is Forward.
   --     Cursor_Error     List is empty.
   --     State_Error      List is in a traversal.
   
   begin -- Move
      if Empty(List) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
               "Error using Move.  List is empty.") ;
      elsif List.Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
               "Error using Move.  List is in a traversal.") ;
      elsif ((Course = Forward) and then
	 (List.Cursor = List.Tail)) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
               "Error using Move.  Can't move beyond end of List.") ;
      elsif ((Course = Backward) and then
	 (List.Cursor = List.Head)) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
               "Error using Move.  Can't move beyond end of List.") ;
      elsif Course = Forward then
	 List.Cursor := Positive'Succ(List.Cursor) ;
      else
	 List.Cursor := Positive'Pred(List.Cursor) ;
      end if ;
   end Move ;
   
   -------------------------------------------------------------------------
   function At_Start(List   : in List_Type) return Boolean is
   
   -- Returns true if the cursor for List is positioned at the start of List.
   -- Returns false otherwise, even if List is empty.
   
   -- Exceptions:
   --    None.
   
   begin
      if Empty(List) then
	 return False ;
      else
         return List.Cursor = List.Head ;
      end if ;
   end At_Start ;
   
  --------------------------------------------------------------------------
   function At_End(List   : in List_Type) return Boolean is
   
   -- Returns true if the cursor for List is positioned at the end of List.
   -- Returns false otherwise, even if List is empty.
   
   -- Exceptions:
   --    None.
   
   begin
      if Empty(List) then
	 return False ;
      else
         return List.Cursor = List.Tail ;
      end if ;
   end At_End ;
   
  --------------------------------------------------------------------------
   function Current_Item(List   : in List_Type) return Element_Type is
   
   -- Returns the item in List at the current cursor position.
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   
   begin -- Current_Item
      if Empty(List) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
		"Error retrieving list element.  List is empty.") ;
      else
	 return List.List_Data(List.Cursor) ;
      end if ;
   end Current_Item ;

   -------------------------------------------------------------------------
   function "="(Left, Right  : in List_Type) return Boolean is
   
   -- Returns true if both lists have the same number of elements in the
   -- same order, and items at the same relative positions are equal.
   -- Returns false otherwise.
      
   -- Exceptions:
   --    None.
   
      Loop_Count : Natural := 0 ;
      Result : Boolean ;
   
   begin -- "="
      if Left.Count /= Right.Count then
	 Result := False ;
      elsif Empty(Left) and Empty(Right) then  -- empty lists are equal.
	 Result := True ;
      else 
	 for I in 1..Left.Count loop
	    exit when Left.List_Data(I) /= Right.List_Data(I) ;
	    Loop_Count := Positive'Succ(Loop_Count) ;
	 end loop ;
	 if Loop_Count /= Left.Count then  -- early exit => lists are not equal.
	   Result := False ;
	 else
	    Result := True ;
	 end if ;
      end if ;
      return Result ;
   end "=" ;
   
   -------------------------------------------------------------------------
   procedure Traverse(List : in out List_Type; Course : in Direction) is
   
   -- Passive list iterator.  Iteration begins at the cursor position and
   -- advances in the direction indicated by Course until an endpoint of List
   -- is reached.  Upon return, the cursor is positioned at an endpoint of
   -- the list, unless the traversal was terminated early.  Early termination
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
   
      Continue : Boolean := True ;
      
   begin -- Traverse
      if not Empty(List) then
	  if List.Traversing then
	    Ada.Exceptions.Raise_Exception (State_Error'identity,
			    "Error using Traverse.  List is already in a traversal.") ;
	  else
	     List.Traversing := True ;
	     loop
		Process(List.List_Data(List.Cursor), Continue) ;
			
		exit when((Course = Forward) and (List.Cursor = List.Tail)) or
		((Course = Backward) and (List.Cursor = List.Head)) or not Continue ;
			
		if Course = Forward then
		   List.Cursor := Positive'Succ(List.Cursor);
		else
		   List.Cursor := Positive'Pred(List.Cursor);
		end if ;
	     end loop ;
	     List.Traversing := False ;
	  end if ;
      end if ;
   end Traverse ;
end List_Manager;
