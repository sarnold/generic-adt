----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment List Package (Array Implementation)
-- Stephen L Arnold
-- generic package body List_Manager
----------------------------------------------------------------------------
-- Description: This package provides services to save items in a list.
-- The items to be saved in the list are defined via the generic type
-- parameter Element_Type.  The list grows dynamically and thus its
-- maximum size is limited only by available memory.  The abstraction
-- of a list is defined by the exported data type List_Type.
----------------------------------------------------------------------------

with Ada.Exceptions ;
with Ada.Unchecked_Deallocation ;
package body Dynamic_List_Manager is

   procedure Free is new Ada.Unchecked_Deallocation(List_Element, List_Element_Ptr) ;

   -------------------------------------------------------------------------
   
                    Item   : in     Element_Type;
   
                    Place  : in     Position_Reference := At_End) is

		    Item   : in     Element_Type;
		    List   : in out List_Type;
		    Place  : in     Position_Reference := At_End) is
   
   -- Exceptions:
   --    Overflow      Item could not be inserted into List.
   --    State_Error   List is in a traversal.
   
      New_Place : Position_Reference := Place ;
      New_Item  : List_Element_Ptr ;

      

      if List.Traversing then
   
                        "Error using Insert.  List is already in a traversal.") ;
      
      begin
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			"Error using Insert.  List is already in a traversal.") ;
         when Storage_Error =>
            Ada.Exceptions.Raise_Exception (Overflow'identity,
	 New_Item := new List_Element'(null, Item, null) ;
      end ;
	 when Storage_Error =>
	    Ada.Exceptions.Raise_Exception (Overflow'identity,
			"Error using Insert.  Not eneough free memory.") ;
      else
      
            New_Place := At_Start ;
	 List := (Count => 1, Traversing => False, others => New_Item) ;
            New_Place := At_End ;
         end if ;
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
   procedure Replace(
                     New_Item   : in     Element_Type;
	    
                     List       : in out List_Type) is

		     New_Item   : in     Element_Type;
		     Old_Item   :    out Element_Type;
		     List       : in out List_Type) is
   
   -- Exceptions:
   --    Cursor_Error     List is empty.
   --    State_Error      List is in a traversal.
   
   begin -- Replace

      if List.Traversing then
   
                        "Error using Replace.  List is already in a traversal.") ;
      
         Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			"Error using Replace.  List is already in a traversal.") ;
         Old_Item := List.Cursor.Data ;
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
			 "Error using Replace.  List is empty.") ;
   end Replace;
	 Old_Item := List.Cursor.Data ;
	 List.Cursor.Data := New_Item ;
   procedure Remove(
                    Item   :    out Element_Type;
   

   -- Removes the Item in List at the current cursor position and returns
		    Item   :    out Element_Type;
		    List   : in out List_Type) is
   

   -- Exceptions:
   --    Cursor_Error     List is empty.
   --    State_Error      List is in a traversal.
   
      Orphan : List_Element_Ptr := List.Cursor ;

   begin -- Remove
      
          Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
      
       elsif List.Traversing then
          Ada.Exceptions.Raise_Exception (State_Error'identity,
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
			"Error using Remove.  List is empty.") ;

	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			"Error using Remove.  List is in a traversal.") ;
       if List.Count = 1 then
       
       elsif List.Cursor = List.Head then
       
          List.Head.Prev := null ;
	  Clear(List) ;
          List.Count := Positive'Pred(List.Count) ;
   	  List.Head := List.Head.Next ;
	  List.Head.Prev := null ;
	  List.Cursor := List.Head ;
	  List.Count := Positive'Pred(List.Count) ;
          List.Count := Positive'Pred(List.Count) ;
   	  List.Tail := List.Tail.Prev ;
	  List.Tail.Next := null ;
	  List.Cursor := List.Tail ;
	  List.Count := Positive'Pred(List.Count) ;
          List.Count := Positive'Pred(List.Count) ;
	  List.Cursor.Prev.Next := List.Cursor.Next ;
	  List.Cursor.Next.Prev := List.Cursor.Prev ;
	  List.Cursor := List.Cursor.Prev ;
	  List.Count := Positive'Pred(List.Count) ;
   -------------------------------------------------------------------------
   procedure Clear(List   : in out List_Type) is

   
   -- If the list is empty, the procedure does nothing.

   
   -- Resets list attributes to zero (removes all items from the list). 

   

   begin  -- Clear
   
         Ada.Exceptions.Raise_Exception (State_Error'identity,
      
      else
         while (List.Head /= null) loop
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
			"Error using Clear.  List is in a traversal.") ;
            Free(Orphan) ;
	 while (List.Head /= null) loop
	    Orphan := List.Head ;
	    List.Head := List.Head.Next ;
	    Free(Orphan) ;
	 end loop ;
	 List := (Count => 0, Traversing => False, others => null) ;
   function Empty(List   : in List_Type) return Boolean is

   

   -- Exceptions:
   

   
      return List.Count = 0 ;
   end Empty ;
   
   -------------------------------------------------------------------------
   function Count(List   : in List_Type) return Natural is

   
   -- returned.

   
   --    None.

   
      return List.Count ;
   end Count ;
   
   -------------------------------------------------------------------------
   procedure Move(
                  List  : in out List_Type;
   

   -- Moves the cursor to the given place in List.  This allows the cursor to
		  List  : in out List_Type;
		  Place : in     EndPoint) is
   
   --     Cursor_Error     List is empty.
   --     State_Error      List is in a traversal.
   
   begin -- Move
      if Empty(List) then
         Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
   
      elsif List.Traversing then
         Ada.Exceptions.Raise_Exception (State_Error'identity,
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
      elsif Place = At_End  then
         List.Cursor := List.Tail ;
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
         List.Cursor := List.Head ;
      end if ;
	 List.Cursor := List.Tail ;

	 List.Cursor := List.Head ;
   procedure Move(
                  List   : in out List_Type;
   

   -- Moves the cursor one item in List.  It only mves one item,
		  List   : in out List_Type;
		  Course : in     Direction) is
   
   --     Cursor_Error     Cursor is at beginning of List and Course is Backward.
   --     Cursor_Error     Cursor is at end of List and Course is Forward.
   
   --     State_Error      List is in a traversal.

   begin -- Move
      if Empty(List) then
         Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
   
      elsif List.Traversing then
         Ada.Exceptions.Raise_Exception (State_Error'identity,
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
      elsif ((Course = Forward) and then
         (List.Cursor = List.Tail)) then
	 Ada.Exceptions.Raise_Exception (State_Error'identity,
               "Error using Move.  Can't move beyond end of List.") ;
      elsif ((Course = Backward) and then
	 (List.Cursor = List.Tail)) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
               "Error using Move.  Can't move beyond end of List.") ;
      elsif Course = Forward then
	 (List.Cursor = List.Head)) then
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
         List.Cursor := List.Cursor.Prev ;
      end if ;
	 List.Cursor := List.Cursor.Next ;

	 List.Cursor := List.Cursor.Prev ;
   function At_Start(List   : in List_Type) return Boolean is

   
   -- Returns false otherwise, even if List is empty.

   
   --    None.

   
      if Empty(List) then
         return False ;
   
         return List.Cursor = List.Head ;
      end if ;
	 return False ;

  --------------------------------------------------------------------------
   function At_End(List   : in List_Type) return Boolean is

   
   -- Returns false otherwise, even if List is empty.

   
   --    None.

   
      if Empty(List) then
         return False ;
   
         return List.Cursor = List.Tail ;
      end if ;
	 return False ;

  --------------------------------------------------------------------------
   function Current_Item(List   : in List_Type) return Element_Type is

   

   -- Exceptions:
   

   
      if Empty(List) then
         Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
   
      else
         return List.Cursor.Data ;
	 Ada.Exceptions.Raise_Exception (Cursor_Error'identity,
		"Error retrieving list element.  List is empty.") ;

	 return List.Cursor.Data ;
   function "="(Left, Right  : in List_Type) return Boolean is

   -- Returns true if both lists have the same number of elements in the
   -- same order, and items at the same relative positions are equal.
   -- Returns false otherwise.
   
   -- Exceptions:
   --    None.

      
      Pright : List_Element_Ptr := Right.Head ;

   
      Loop_Count : Natural := 0 ;
      Result : Boolean ;
   
         while (Pleft /= null) and (Pright /= null) loop
            if Pleft.Data /= Pright.Data then
	 Result := False ;
      elsif Empty(Left) and Empty(Right) then  -- empty lists are equal.
	 Result := True ;
      else 
	 for I in 1..Left.Count loop
	    exit when Left.Cursor.Data /= Right.Cursor.Data ;
	    Loop_Count := Positive'Succ(Loop_Count) ;
	 end loop ;
	 if Loop_Count /= Left.Count then  -- early exit => lists are not equal.
	   Result := False ;
	 else
	    Result := True ;
	 end if ;
   procedure Traverse(List : access List_Type; Course : in Direction) is
      return Result ;

   
   -- advances in the direction indicated by Course until an endpoint of List
   -- is reached.  Upon return, the cursor is positioned at an endpoint of
   
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
   

      Continue : Boolean := True ;

   begin -- Traverse
   
          if List.Traversing then
      
                            "Error using Traverse.  List is already in a traversal.") ;
      if List.Count /= 0 then
	  if List.Traversing then
	    Ada.Exceptions.Raise_Exception (State_Error'identity,
			    "Error using Traverse.  List is already in a traversal.") ;
	  else
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
	  end if ;
