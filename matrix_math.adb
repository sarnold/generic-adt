-----------------------------------------------------------------------------
--  Allan Hancock College
--  CS152, Spring 2000
--  Assignment Matrix Math
--  Stephen L Arnold
--  Package Body Matrix_Math
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

   with Ada.Exceptions;
   package body Matrix_Math is

    -------------------------------------------------------------------------
    --   This function performs the addition of matrix Left and matrix Right
    --   resulting in a matrix. Comparability of dimensions is checked.
    -------------------------------------------------------------------------

      function "+" (Left, Right : in Matrix) return Matrix is

         Result : Matrix(Left'range(1), Left'range(2)) ;

      begin

         if (Left'length(1) /= Right'length(1)) or (Left'length(2) /= Right'length(2)) then
            Ada.Exceptions.Raise_Exception (BAD_DIMENSION'identity,
               "Error using + . Matrix dimensions must agree.") ;
         end if ;

         for I in Left'range(1) loop
            for J in Left'range(2) loop
               Result(I,J) := Left(I,J)+Right(Right'first(1)+(I-Left'first(1)),Right'first(2)+(J-Left'first(2))) ;
            end loop ;
         end loop ;
         return Result ;
      end "+";

   --

    -------------------------------------------------------------------------
    --   This function performs the subtraction of matrix Right from matrix
    --   Left resulting in a matrix. Comparability of dimensions is checked.
    -------------------------------------------------------------------------

      function "-" (Left, Right : Matrix) return Matrix is

         Result : Matrix(Left'range(1), Left'range(2)) ;

      begin

         if (Left'length(1) /= Right'length(1)) or (Left'length(2) /= Right'length(2)) then
            Ada.Exceptions.Raise_Exception (BAD_DIMENSION'identity,
               "Error using - . Matrix dimensions must agree.") ;
         end if ;

         for I in Left'range(1) loop
            for J in Left'range(2) loop
               Result(I,J) := Left(I,J)-Right(Right'first(1)+(I-Left'first(1)),Right'first(2)+(J-Left'first(2))) ;
            end loop ;
         end loop ;
         return Result ;
      end "-" ;

   ---

    -------------------------------------------------------------------------
    --   This function performs the multiplication of matrix Left and matrix
    --   Right resulting in a matrix. Comparability of dimensions is checked.
    -------------------------------------------------------------------------

      function "*" (Left, Right : Matrix) return Matrix is

         Result : Matrix(Left'range(1), Right'range(2)) := (others => (others => 0.0)) ;
            Sum : Float ;

      begin

         if Left'length(2)/=Right'length(1) then
            Ada.Exceptions.Raise_Exception (BAD_DIMENSION'identity,
               "Error using * . Inner matrix dimensions must agree.") ;
         end if ;

         for I in Left'range(1) loop
            for J in Right'range(2) loop
               Sum := 0.0 ;
               for K in Left'range(2) loop
                  Sum := Sum+Left(I,K)*Right((K-Left'first(2))+Right'first(1),J) ;
               end loop;
               Result(I,J) := Sum ;
            end loop ;
         end loop ;
         return Result ;
      end "*" ;

   ---

    -------------------------------------------------------------------------
    --   This function performs the scalar multipliction of a matrix Right by
    --   a floating point number Left resulting in a matrix.
    -------------------------------------------------------------------------

      function "*" (Left : Float; Right : Matrix) return Matrix is

         Result : Matrix(Right'range(1), Right'range(2));

      begin
         for I in Right'range(1) loop
            for J in Right'range(2) loop
               Result(I,J) := Left * Right(I,J) ;
            end loop ;
         end loop ;
         return Result ;
      end "*" ;

   ---

    -------------------------------------------------------------------------
    --   This function performs the scalar multipliction of a matrix Left
    --   by a floating point number Right resulting in a matrix.
    -------------------------------------------------------------------------

      function "*" (Left : Matrix; Right : Float) return Matrix is

         Result:Matrix(Left'range(1), Left'range(2)) ;

      begin
         Result := Right * Left ;
         return Result ;
      end "*" ;

   end Matrix_Math;
