generic-adt
===========

Ada generic packages for some useful ADTs (list, stack, queue, bst) with test 
drivers and a few other examples.

Brief descriptions from the comments in the specs:

 - stand-alone program unit Simple Scanner

Description: Simple Scanner is a scanner that recognizes certain
identifiers, integers, and a limited set of single character symbols
The program will prompt for and retrieve the input text file name
if it is not provided on the command line. Identifiers start with a
letter and can be composed of any combination of letters, digits, and
the underscore. Integers always start with a base ten digit and can be
composed of any combination of base ten digits and the underscore.
Symbols are any character from the following set:
 "~!@#$%^&*()_+=-{}[]<>|/,.?"
White space (i.e., blanks, tabs, and line terminators) always separates
lexical elements. Any symbol character except the underscore also
separates lexical elements.

 - Package Specification Matrix_Math

This matrix math package defines a matrix type as a two-dimensional
unconstrained array type, indexed by natural numbers, and containing
floating point numbers. Operators are provided in the package for the
following operations: addition of two matrices, subtraction of two
matrices, multiplication of two matrices, and multiplying a scalar
and a matrix. Each operation requires the appropriate mathematical
construct, and will raise the package-defined exception BAD_DIMENSION if
any constraint is violated.

 - generic package specification List_Manager

Description: This package provides services to save items in a list.
The items to be saved in the list are defined via the generic type
parameter Element_Type. The maximum size of the list is defined by
the generic constant object Max_Size. Max_Size is an optional parameter
in the instantiation and defaults to the value shown below. The
abstraction of a list is defined by the exported data type List_Type.

 - generic package specification Dynamic_List_Manager

Description: This package provides services to save items in a list.
The items to be saved in the list are defined via the generic type
parameter Element_Type. The list grows dynamically and thus its
maximum size is limited only by available memory. The abstraction
of a list is defined by the exported data type List_Type.

 - generic package specification Priority_Queue_Manager

Description: This package provides services to save items in a prioritized
queue. The items to be saved in the queue are defined via the generic formal
type parameter Element_Type. Conceptually, each item of type Element_Type
contains all the necessary information to determine the priority of an item.
The priority information is extracted from a given item using the imported
function priority. Two priorities are compared using the imported function
less than ("<").

The prioritized queues grow dynamically and thus their maximum size
is limited only by available memory. The abstraction of a
prioritized queue is defined by the exported data type
Prioritized_Queue_Type.

 - generic package specification Stack_Manager

Description: This package provides services to save items in a stack.
The items to be saved in the stack are defined via the generic formal
type parameter Element_Type. The stack grows dynamically and thus its
maximum size is limited only by available memory. The abstraction
of a stack is defined by the exported data type Stack_Type.

 - generic package specification Binary_Search_Tree

Description: This package provides services to save items in a binary
search tree and perform efficient searches for those items.
The tree grows dynamically and so is limited only by available memory.
Each element stored in the tree is considered to contain an embedded
key value that is used to determine where the item is stored in the
tree. Any item to be modified should be removed from the tree,
modified, and then re-inserted. Duplicate keys are not allowed.

 - Procedure Merge_Sort

Description: This program implements a merge sort using the Priority
Queue, BST, and Dynamic List Manager packages. It performs a partition
(or "chunk") phase, followed by a merge phase, to sort a large disk file.

You can enter an input filename on the command line, or the program
will prompt you for the name. Output is written to the filename
'output.dat' so make sure your input file has a different name.
Also, the input file should have one integer value per line. Duplicate
processing is not guaranteed to be correct (some will be left out).


