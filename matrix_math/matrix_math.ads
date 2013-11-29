-----------------------------------------------------------------------------
--  Allan Hancock College
--  CS152, Spring 2000
--  Assignment Matrix Math
--  Stephen L Arnold
--  Package Specification Matrix_Math
-----------------------------------------------------------------------------
-- This matrix math package defines a matrix type as a two-dimensional
-- unconstrained array type, indexed by natural numbers, and containing
-- floating point numbers. Operators are provided in the package for the
-- following operations: addition of two matrices, subtraction of two
-- matrices, multiplication of two matrices, and multiplying a scalar
-- and a matrix. Each operation requires the appropriate mathematical
-- construct, and will raise the package-defined exception BAD_DIMENSION if
-- any constraint is violated.
-----------------------------------------------------------------------------
   package Matrix_Math is
   
   -- Types and functions provided by this package:
   
      type Matrix is array(positive range<>,positive range <>) of Float ;
   
      function "+" (Left, Right : in Matrix) return Matrix ; -- sum of matrix
   
      function "-" (Left, Right : in Matrix) return Matrix ; -- difference of matrix
   
      function "*" (Left, Right : in Matrix) return Matrix ; -- matrix multiplication
   
      function "*" (Left : in Float; Right : in Matrix) return Matrix ;
    -- float, matrix multiplication
   
      function "*" (Left : in Matrix; Right : in Float) return Matrix ;
    -- matrix, float multiplication
   
   -- Exceptions
   
      BAD_DIMENSION : exception ;	
   -- the appropriate dimension(s) of the operand matrices is(are) incorrect.
   
   end Matrix_Math ;
