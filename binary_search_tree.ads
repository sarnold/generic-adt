----------------------------------------------------------------------------
-- Allan Hancock College
-- CS152, Spring 2000
-- Assignment Binary Tree Package
-- Stephen L Arnold
-- generic package specification Binary_Search_Tree
----------------------------------------------------------------------------
-- Description: This package provides services to save items in a binary
-- search tree and perform efficient searches for those items.
-- The tree grows dynamically and so is limited only by available memory.
-- Each element stored in the tree is considered to contain an embedded
-- key value that is used to determine where the item is stored in the
-- tree.  Any item to be modified should be removed from the tree,
-- modified, and then re-inserted.  Duplicate keys are not allowed.
----------------------------------------------------------------------------

generic
   type Element_Type is private;
   type Key_Type     is private;
   with function Key(Item : in Element_Type) return Key_Type;
   with function "="(Left, Right : in Key_Type) return Boolean is <>;
   with function "<"(Left, Right : in Key_Type) return Boolean is <>;

package Binary_Search_Tree is

   type BST is limited private;
   type Traversal is (In_Order, Pre_Order, Post_Order);

   Overflow     :  exception;  -- Rasied when tree space runs out.
   Key_Error    :  exception;  -- Rasied for bogus key operations.
   State_Error  :  exception;  -- Rasied for more than one concurrent
                               -- traversal, or state change during
                               -- a traversal.

   -------------------------------------------------------------------------
   procedure Insert(Item : in Element_Type; Tree : in out BST);

   -- Adds Item to Tree.

   -- Exceptions:
   --   Overflow      Item could not be added to Tree.
   --   Key_Error     Key(Item) already exists.
   --   State_Error   Tree is in a traversal.


   -------------------------------------------------------------------------
   procedure Remove(K : in Key_Type; Tree : in out BST; Item : out Element_Type);

   -- Removes an item corresponding to key K from Tree and returns it as Item.

   -- Exceptions:
   --   Key_Error     K does not exist in Tree.
   --   State_Error   Tree is in a traversal.


   -------------------------------------------------------------------------
   function Exists(K : in Key_Type; Tree : in BST) return Boolean;

   -- Returns true if an item with key K exists in Tree, false otherwise.

   -- Exceptions:
   --   None.


   -------------------------------------------------------------------------
   function Retrieve(K : in Key_Type; Tree : in BST) return Element_Type;

   -- Finds an item corresponding to key K in Tree and returns a copy of
   -- the item.

   -- Exceptions:
   --   Key_Error     K does not exist in Tree.
   --   State_Error   Tree is in a traversal.


   -------------------------------------------------------------------------
   function Empty(Tree : in BST) return Boolean;

   -- Returns true if the Tree is empty and false otherwise.

   -- Exceptions:
   --    None.


   -------------------------------------------------------------------------
   function Count(Tree : in BST) return Natural;

   -- Returns the number of items in Tree.  If the Tree is empty,
   -- zero is returned.

   -- Exceptions:
   --    None.


   -------------------------------------------------------------------------
   procedure Clear(Tree : in out BST);

   -- Removes all items from the Tree.  If the Tree is empty, the procedure
   -- does nothing.

   -- Exceptions:
   --   State_Error   Tree is in a traversal.


   -------------------------------------------------------------------------
   generic
      with procedure Process(Item : in Element_Type; Continue : out boolean) ;
   procedure Traverse(Tree : access BST; Order : in Traversal) ;

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




private

   type Tree_Node;
   type Tree_Node_Ptr is access Tree_Node;

   type Child_Name is (Left, Right);
   type Children is array(Child_Name) of Tree_Node_Ptr;

   type Tree_Node is
      record
         Data  : Element_Type;
         Child : Children := (others => null);
      end record;

   type BST is
      record
         Root       : Tree_Node_Ptr := null;
         Count      : Natural       := 0;
         Traversing : Boolean       := False;
      end record ;

end Binary_Search_Tree;


