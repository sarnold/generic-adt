----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment Binary Tree Package
-- Stephen L Arnold
-- package body Binary_Search_Tree
----------------------------------------------------------------------------
-- Description: This package provides services to save items in a binary
-- search tree and perform efficient searches for those items.
-- The tree grows dynamically and so is limited only by available memory.
-- Each element stored in the tree is considered to contain an embedded
-- key value that is used to determine where the item is stored in the
-- tree.  Any item to be modified should be removed from the tree,
-- modified, and then re-inserted.  Duplicate keys are not allowed.
--
-- Do I really need a description here, or is the spec sufficient?
----------------------------------------------------------------------------

with Ada.Exceptions ;
with Ada.Unchecked_Deallocation ;

package body Binary_Search_Tree is

   procedure Free is new Ada.Unchecked_Deallocation(Tree_Node, Tree_Node_Ptr) ;


   -------------------------------------------------------------------------
   procedure Insert(Item : in Element_Type; Tree : in out BST) is

   -- Adds Item to Tree.

   -- Exceptions:
   --   Overflow      Item could not be added to Tree.
   --   Key_Error     Key(Item) already exists.
   --   State_Error   Tree is in a traversal.

      New_item : Tree_Node_Ptr := null;
      Parent   : Tree_Node_Ptr := null;
      Target   : Tree_Node_Ptr := Tree.Root;

   begin -- Insert
      if Tree.Traversing then
         Ada.Exceptions.Raise_Exception (State_Error'identity,
                         "Error using Insert.  Tree is already in a traversal.");
      elsif Exists(K, Tree) then
         Ada.Exceptions.Raise_Exception (Key_Error'identity,
                         "Error using Insert.  Key already exists.");
      end if;
      begin
         New_Item := new Tree_Node'(Data => Item);
      exception
         when Storage_Error =>
            Ada.Exceptions.Raise_Exception (Overflow'identity,
                         "Error using Insert.  Not eneough free memory.");
      end;
      -- Find the insert spot.
      while Target /= null loop
         Parent := Target;
         if Key(Item) < Key(Target.Data) then
            Target := Target.Child(Left);
         else
            Target := Target.Child(Right);
         end if;
      end loop;
      -- Insert new item
      if Parent = null then
         Tree.Root := New_Item;
      elsif Key(Item) < Key(Parent.Data) then
         Parent.Child(Left) := New_Item;
      else
         Parent.Child(Right) := New_Item;
      end if;
      Tree.Count := Natural'Succ(Tree.Count);
   end Insert;

   -------------------------------------------------------------------------
   procedure Remove(K : in Key_Type; Tree : in out BST; Item : out Element_Type) is

   -- Removes an item corresponding to key K from Tree and returns it as Item.

   -- Exceptions:
   --   Key_Error     K does not exist in Tree.
   --   State_Error   Tree is in a traversal.

      Original : Tree_Node_Ptr := null;
      Parent   : Tree_Node_Ptr := null;
      Target   : Tree_Node_Ptr := Tree.Root;

   begin -- Remove
      if Tree.Traversing then
         Ada.Exceptions.Raise_Exception (State_Error'identity,
                         "Error using Remove.  Tree is already in a traversal.");
      elsif not Exists(K, Tree) then
         Ada.Exceptions.Raise_Exception (Key_Error'identity,
                         "Error using Remove.  Key not found.");
      else -- Find the target node
         while Target /= null loop
            exit when K = Key(Target.Data);
            Parent := Target;
            if K < Key(Target.Data) then
               Target := Target.Child(Left);
            else
               Target := Target.Child(Right);
            end if;
         end loop;
         Item := Target.Data; -- Save target's data
         -- Now figure out the node arrangment
         if Target.Child(Right) = null then
            if Target = Tree.Root then
               Tree.Root := Target.Child(Left);
            else
               Parent.Child(Child_Name) := Target.Child(Left);
            end if;
         elsif
           Target.Child(Left) = null then
            if Target = Tree.Root then
               Tree.Root := Target.Child(Right);
            else
               Parent.Child(Child_Name) := Target.Child(Right);
            end if;
         else
            Original := Target;
            Parent := Target;
            Target := Target.Child(Right);
            while Target.Child(Left) /= null loop
               Parent := Target;
               Target := Target.Child(Left);
            end loop;
            Original.Data := Target.Data;
            Parent.Child(Left) := Target.Child(Right);
         end if;
         Tree.Count := Natural'Pred(Tree.Count);
         Free(Target);
      end if;
   end Remove;

   -------------------------------------------------------------------------
   function Exists(K : in Key_Type; Tree : in BST) return Boolean is

   -- Returns true if an item with key K exists in Tree, false otherwise.

   -- Exceptions:
   --   None.

      Ptr : Tree_Node_Ptr := Tree.Root;

   begin -- Exists
      while Ptr /= null loop
         if K = Key(Ptr.Data) then
            return True;
         elsif K < Key (Ptr.Data) then
            Ptr := Ptr.Child(Left);
         else
            Ptr := Ptr.Child(Right);
         end if;
      end loop;
      return False;
   end Exists;

   -------------------------------------------------------------------------
   function Retrieve(K : in Key_Type; Tree : in BST) return Element_Type is

   -- Finds an item corresponding to key K in Tree and returns a copy of
   -- the item.

   -- Exceptions:
   --   Key_Error     K does not exist in Tree.
   --   State_Error   Tree is in a traversal.

      Ptr : Tree_Node_Ptr := Tree.Root;

   begin -- Retrieve
      if Tree.Traversing then
         Ada.Exceptions.Raise_Exception (State_Error'identity,
                         "Error using Retrieve.  Tree is already in a traversal.");
      elsif not Exists(K, Tree) then
         Ada.Exceptions.Raise_Exception (Key_Error'identity,
                         "Error using Retrieve.  Key not found.");
      else
         while Ptr /= null loop
            if K = Key(Ptr.Data) then
               return Ptr.Data;
            elsif K < Key (Ptr.Data) then
               Ptr := Ptr.Child(Left);
            else
               Ptr := Ptr.Child(Right);
            end if;
         end loop;
      end if;
   end Retrieve;

   -------------------------------------------------------------------------
   function Empty(Tree : in BST) return Boolean is

   -- Returns true if the Tree is empty and false otherwise.

   -- Exceptions:
      --    None.

   begin -- Empty
      return Tree.Count = 0;
   end Empty;

   -------------------------------------------------------------------------
   function Count(Tree : in BST) return Natural is

   -- Returns the number of items in Tree.  If the Tree is empty,
   -- zero is returned.

   -- Exceptions:
   --    None.

   begin  -- Count
      return Tree.Count;
   end Count;

   -------------------------------------------------------------------------
   procedure Clear(Tree : in out BST) is

   -- Removes all items from the Tree.  If the Tree is empty, the procedure
   -- does nothing.

   -- Exceptions:
   --   State_Error   Tree is in a traversal.

         procedure PostClear( P : in Tree_Node_Ptr) is
         begin -- Postorder
            if P /= null then
               PostClear(P.Child(Left));
               PostClear(P.Child(Right));
               Free(P);
            end if;
         end PostClear;

         -- I couldn't figure out how to do this by calling Traverse directly,
         -- but this seems pretty clear and concise to me.

   begin -- Clear
      if Tree.Count > 0 then
         if Tree.Traversing then
            Ada.Exceptions.Raise_Exception (State_Error'identity,
                            "Error using Clear.  Tree is already in a traversal.");
         else
            Postclear(Tree.Root);
            Tree.Count := 0;
         end if;
      end if;
   end Clear;

   -------------------------------------------------------------------------
   procedure Traverse(Tree : access BST; Order : in Traversal) is

   -- Passive iterator.  If it is not empty, all items in Tree are traversed
   -- according to the given Order and each item in the Tree is passed to
   -- procedure Process.  The traversal can be terminated early
   -- by setting the Continue parameter of procedure Process to false.
   -- Otherwise, the Continue parameter should be set to true.
   -- During a traversal, the state of Tree is not allowed to be changed.
   -- A state change is defined to occur when items are either inserted or
   -- removed from the Treee.  If Tree is empty, this procedure does nothing.

   -- Exceptions:
   --     State_Error     Tree is already in a traversal.
   --     State_Error     Attempt to change Tree state in procedure Process.
   --     Any exceptions raised within procedure Process are propagated.


      Continue : Boolean := True;

      procedure Inorder( P : in Tree_Node_Ptr) is
      begin -- Inorder
         if P /= null then
            Inorder(P.Child(Left));
            if Continue then
               Process(P.Data, Continue);
            end if;
            Inorder(P.Child(Right));
         end if;
      end Inorder;

      procedure Preorder( P : in Tree_Node_Ptr) is
      begin -- Preorder
         if P /= null then
            if Continue then
               Process(P.Data, Continue);
            end if;
            Preorder(P.Child(Left));
            Preorder(P.Child(Right));
         end if;
      end Preorder;

      procedure Postorder( P : in Tree_Node_Ptr) is
      begin -- Postorder
         if P /= null then
            Postorder(P.Child(Left));
            Postorder(P.Child(Right));
            if Continue then
               Process(P.Data, Continue);
            end if;
         end if;
      end Postorder;


   begin -- Traverse
      if Tree.Count > 0 then
          if Tree.Traversing then
            Ada.Exceptions.Raise_Exception (State_Error'identity,
                            "Error using Traverse.  Tree is already in a traversal.");
          else
             begin
                Tree.Traversing := True;
                case Order is
                   when In_Order => Inorder(Tree.Root);

                   when Pre_Order => Preorder(Tree.Root);

                   when Post_Order => Postorder(Tree.Root);
                end case;
                Tree.Traversing := False;
             exception
                when State_Error =>
                   Tree.Traversing := False;
                   raise State_Error;
             end;
          end if;
      end if;
   end Traverse;
end Binary_Search_Tree;





