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

with Ada.Exceptions ;
with Ada.Unchecked_Deallocation ;
package body Dynamic_List_Manager is
   
   procedure Free is new Ada.Unchecked_Deallocation(List_Element, List_Element_Ptr) ;
   
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
      
      New_Place : Position_Reference := Place ;
      New_Item  : List_Element_Ptr ;
   
   begin -- Insert
      
      if List.Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			"Error using Insert.  List is already in a traversal.") ;
      end if ;
      begin
	 New_Item := new List_Element'(null, Item, null) ;
      exception
	 when Storage_Error =>
	    Ada.Exceptions.Raise_Exception (Overflow'identity,
			"Error using Insert.  Not eneough free memory.") ;
      end ;
      
      if List.Count = 0 then
	 List := (Count => 1, Traversing => False, others => New_Item) ;
      else
         if (Place = Before) and (List.Cursor = List.Head) then
	    New_Place := At_Start ;
	 elsif (Place = After) and (List.Cursor = List.Tail) then
	    New_Place := At_End ;
	 end if ;
	 case New_Place is
	    when At_Start =>
	       New_Item.Next := List.Head ;
	       List.Head.Prev := New_Item ;
	       List.Head := New_Item ;
	    when At_End =>
	       New_Item.Prev := List.Tail ;
	       List.Tail.Next := New_Item ;
	       List.Tail := New_Item ;
	    when Before =>
	       New_Item.all := (List.Cursor.Prev, New_Item.Data, List.Cursor) ;
	       List.Cursor.Prev.Next := New_Item ;
	       List.Cursor.Prev := New_Item ;
	    when After =>
   	       New_Item.all := (List.Cursor, New_Item.Data, List.Cursor.Next) ;
   	       List.Cursor.Next.Prev := New_Item ;
	       List.Cursor.Next := New_Item ;
	 end case ;
	 List.Cursor := New_Item ;
	 List.Count := Positive'Succ(List.Count) ;
      end if ;
   end Insert ;
	    
   -------------------------------------------------------------------------
   procedure Replace(
		     New_Item   : in     Element_Type;
		     Old_Item   :    out Element_Type;
		     List       : in out List_Type) is
   
   -- Replaces an Item in The List.  The Old_Item is returned and replaced with
   -- the New_Item. The item to be replaced is the one at the current cursor
   -- position.
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   --    State_Error      List is in a traversal.
   
   begin -- Replace
      
      if List.Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			"Error using Replace.  List is already in a traversal.") ;
      elsif Empty(List) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
			 "Error using Replace.  List is empty.") ;
      else
	 Old_Item := List.Cursor.Data ;
	 List.Cursor.Data := New_Item ;
      end if ;
   end Replace;
   
   -------------------------------------------------------------------------
   procedure Remove(
		    Item   :    out Element_Type;
		    List   : in out List_Type) is
   
   -- Removes the Item in List at the current cursor position and returns
   -- Item.  The cursor is placed at the item immediately preceding the item
   -- being removed.  If the item to be removed was at the head of the list,
   -- the cursor is placed at the new head of the list.
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   --    State_Error      List is in a traversal.
      
      Orphan : List_Element_Ptr := List.Cursor ;
      
   begin -- Remove
       if List.Count = 0 then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
			"Error using Remove.  List is empty.") ;
       elsif List.Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			"Error using Remove.  List is in a traversal.") ;
       end if ;
       
       Item := List.Cursor.Data ;
       
       if List.Count = 1 then
	  Clear(List) ;
       elsif List.Cursor = List.Head then
   	  List.Head := List.Head.Next ;
	  List.Head.Prev := null ;
	  List.Cursor := List.Head ;
	  List.Count := Positive'Pred(List.Count) ;
       elsif List.Cursor = List.Tail then
   	  List.Tail := List.Tail.Prev ;
	  List.Tail.Next := null ;
	  List.Cursor := List.Tail ;
	  List.Count := Positive'Pred(List.Count) ;
       else
	  List.Cursor.Prev.Next := List.Cursor.Next ;
	  List.Cursor.Next.Prev := List.Cursor.Prev ;
	  List.Cursor := List.Cursor.Prev ;
	  List.Count := Positive'Pred(List.Count) ;
       end if ;
       Free(Orphan) ;
   end Remove;
   
   -------------------------------------------------------------------------
   procedure Clear(List   : in out List_Type) is
   
   -- Resets list attributes to zero (removes all items from the list). 
   -- If the list is empty, the procedure does nothing.
   
   -- Exceptions:
   --    State_Error       List is in a traversal.
   
      Orphan : List_Element_Ptr := List.Cursor ;
      
   begin  -- Clear
      if List.Traversing then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			"Error using Clear.  List is in a traversal.") ;
      else
	 while (List.Head /= null) loop
	    Orphan := List.Head ;
	    List.Head := List.Head.Next ;
	    Free(Orphan) ;
	 end loop ;
	 List := (Count => 0, Traversing => False, others => null) ;
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
	 List.Cursor := List.Cursor.Next ;
      else
	 List.Cursor := List.Cursor.Prev ;
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
	 return List.Cursor.Data ;
      end if ;
   end Current_Item ;

   -------------------------------------------------------------------------
   function "="(Left, Right  : in List_Type) return Boolean is
   
   -- Returns true if both lists have the same number of elements in the
   -- same order, and items at the same relative positions are equal.
   -- Returns false otherwise.
      
   -- Exceptions:
   --    None.
      
      Pleft  : List_Element_Ptr := Left.Head ;
      Pright : List_Element_Ptr := Right.Head ;
   
   begin -- "="
      if Left.Count /= Right.Count then
	 Return False ;
      else 
	 while (Pleft /= null) and (Pright /= null) loop
	    if Pleft.Data /= Pright.Data then
	       return False ;
	    end if ;
	    Pleft := Pleft.Next ;
	    Pright := Pright.Next ;
	 end loop ;
	 return True ;
      end if ;
   end "=" ;
   
   -------------------------------------------------------------------------
   procedure Traverse(List : access List_Type; Course : in Direction) is
   
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
      if List.Count > 0 then
	  if List.Traversing then
	    Ada.Exceptions.Raise_Exception (State_Error'identity,
			    "Error using Traverse.  List is already in a traversal.") ;
	  else
	     begin
		List.Traversing := True ;
		loop
		   Process(List.Cursor.Data, Continue) ;
			
		   exit when((Course = Forward) and (List.Cursor = List.Tail)) or
		     ((Course = Backward) and (List.Cursor = List.Head)) or not Continue ;
			
		   if Course = Forward then
		      List.Cursor := List.Cursor.Next ;
		   else
		      List.Cursor := List.Cursor.Prev ;
		   end if ;
		end loop ;
		List.Traversing := False ;
	     exception
		when State_Error =>
		   List.Traversing := False ;
		   raise State_Error ;
	     end ;
	  end if ;
      end if ;
   end Traverse ;
end Dynamic_List_Manager;
