   with Ada.Text_IO;
   with Ada.Float_Text_IO;
   with Ada.Exceptions;
   with Ada.Numerics.Discrete_Random;
   with Ada.Command_Line;                  use Ada.Command_Line;
   with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
   with Ada.Strings.Maps.Constants;        use Ada.Strings.Maps.Constants;

   with Matrix_Math;                       use Matrix_Math;
   with Minimum_Float_String_Width;

   procedure Matrix_Math_Test is
   
   
      procedure Perform_Exception_Tests is
      -- No verbosity needed for these tests.
      
         a : Matrix(1..4,1..3) := (others => (others => 0.0));
         b : Matrix(1..2,1..2) := (others => (others => 0.0));
         c : Matrix(1..1,2..1) := (others => (others => 0.0));
      
      begin -- Perform_Exception_Tests
         Addition_Test:
         begin
            c := a + b;
            Ada.Text_IO.Put_Line("* Failed Addition Exception Test.");
            exception
               when e : others =>
                  Ada.Text_IO.Put_Line("  Passed Addition Exception Test (" &
                                       Ada.Exceptions.Exception_Name(e) & ").");
         end Addition_Test;
      
         Subtraction_Test:
         begin
            c := a - b;
            Ada.Text_IO.Put_Line("* Failed Subtraction Exception Test.");
            exception
               when e : others =>
                  Ada.Text_IO.Put_Line("  Passed Subtraction Exception Test (" &
                                       Ada.Exceptions.Exception_Name(e) & ").");
         end Subtraction_Test;
      
         Multiplication_Test:
         begin
            c := a * b;
            Ada.Text_IO.Put_Line("* Failed Multiplication Exception Test.");
            exception
               when e : others =>
                  Ada.Text_IO.Put_Line("  Passed Multiplication Exception Test (" &
                                       Ada.Exceptions.Exception_Name(e) & ").");
         end Multiplication_Test;
      end Perform_Exception_Tests;
   
   
      procedure Check_Matrix_Math(
                        Left_Operand    : in Matrix;
                        Right_Operand   : in Matrix;
                        Result          : in Matrix;
                        Expected_Result : in Matrix;
                        Test_Title      : in string;
                        Verbose         : in boolean := false) is
      
         Prefix : constant string := "| ";
      
      
         procedure Put(
                      m      : in Matrix;
                      Prefix : in string := "") is
         
            function Field_Width is new Minimum_Float_String_Width(Ada.Float_Text_IO);
         
            Widths : array(m'range(2)) of natural := (others => 0);
         
         begin -- Put
            for col in m'range(2) loop
               for row in m'range(1) loop
                  Widths(col) := natural'max(Widths(col),
                                             Field_Width(m(row,col),1,0) - 1);
               end loop;
            end loop;
         
            for row in m'range(1) loop
               Ada.Text_IO.Put(Prefix);
               for col in m'range(2) loop
                  Ada.Float_Text_IO.Put(m(row,col),Widths(col),1,0);
                  if col < m'last(2) then
                     Ada.Text_IO.Put(",");
                  end if;
               end loop;
               Ada.Text_IO.New_Line;
            end loop;
         end Put;
      
      
      begin -- Check_Matrix_Math
         if Verbose then
            Ada.Text_IO.Put_Line("+-" & (Test_Title'length+9) * '-');
            Ada.Text_IO.Put(Prefix & Test_Title);
            if Result = Expected_Result then
               Ada.Text_IO.Put_Line(" (passed)");
            else
               Ada.Text_IO.Put_Line(" (failed)");
            end if;
            Ada.Text_IO.Put_Line(Prefix);
            Ada.Text_IO.Put_Line(Prefix & "Values for Left Operand:");
            Put(Left_Operand, Prefix);
            Ada.Text_IO.Put_Line(Prefix);
            Ada.Text_IO.Put_Line(Prefix & "Values for Right Operand:");
            Put(Right_Operand, Prefix);
            Ada.Text_IO.Put_Line(Prefix);
            Ada.Text_IO.Put_Line(Prefix & "Values for Result Matrix:");
            Put(Result, Prefix);
            Ada.Text_IO.Put_Line(Prefix);
            Ada.Text_IO.Put_Line(Prefix & "Values for Expected Result Matrix:");
            Put(Expected_Result, Prefix);
            Ada.Text_IO.Put_Line("+-" & (Test_Title'length+9) * '-');
            Ada.Text_IO.New_Line(2);
         else
            if Result = Expected_Result then
               Ada.Text_IO.Put("  Passed ");
            else
               Ada.Text_IO.Put("* Failed ");
            end if;
            Ada.Text_IO.Put_Line(Test_Title & ".");
         end if;
      end Check_Matrix_Math;
   
   
      a4x3 : Matrix(101..104,101..103) := ((-6.0,-96.0,-6.0),
                                              (-61.0,-27.0,43.0), (-53.0,-45.0,35.0), (17.0,53.0,-7.0));
      b4x3 : Matrix(201..204,201..203) := ((6.0,96.0,6.0),
                                              (61.0,27.0,-43.0), (53.0,45.0,-35.0), (-17.0,-53.0,7.0));
      c4x3 : Matrix(1..4,1..3);
      z4x3 : constant Matrix(1..4,1..3) := (others => (others => 0.0));
   
      a4x2 : Matrix(101..104,101..102) := ((6.0,-6.0),(-4.0,-9.0),(-2.0,-9.0),(-2.0,5.0));
      b2x3 : Matrix(201..202,201..203) := ((4.0,-9.0,-4.0),(5.0,7.0,-3.0));
      m4x3 : constant Matrix(1..4,1..3) := ((-6.0,-96.0,-6.0),
                                               (-61.0,-27.0,43.0), (-53.0,-45.0,35.0), (17.0,53.0,-7.0));
   
      b1x1 : Matrix(1..1,1..1);
   
      subtype Small_Positive is positive range 1..20;
      package Random_Numbers is new Ada.Numerics.Discrete_Random(Small_Positive);
      Gen : Random_Numbers.Generator;
      n   : Small_Positive;
   
      Verbose : boolean := false;
   
   begin -- Matrix_Math_Test
      Verbose := (Argument_Count = 1) and then
                    (Translate(Argument(1), Lower_Case_Map) = "-v");
   
      Perform_Exception_Tests;
   
      Check_Matrix_Math(a4x3, b4x3, a4x3 + b4x3, z4x3, "Matrix Addition Test", Verbose);
      Check_Matrix_Math(a4x3, a4x3, a4x3 - a4x3, z4x3, "Matrix Subtraction Test", Verbose);
      Check_Matrix_Math(a4x2, b2x3, a4x2 * b2x3, m4x3, "Matrix Multiplication Test", Verbose);
      b1x1 := (others => (others => 1.0));
      Check_Matrix_Math(a4x3, b1x1, a4x3 * 1.0, a4x3, "Matrix Times 1 Test", Verbose);
      Check_Matrix_Math(a4x3, b1x1, 1.0 * a4x3, a4x3, "1 Times Matrix Test", Verbose);
      b1x1 := (others => (others => -1.0));
      Check_Matrix_Math(a4x3, b1x1, a4x3 * (-1.0), b4x3, "Matrix Times -1 Test", Verbose);
      Check_Matrix_Math(a4x3, b1x1, (-1.0) * a4x3, b4x3, "-1 Times Matrix Test", Verbose);
      b1x1 := (others => (others => 2.0));
      Check_Matrix_Math(a4x3, b1x1, a4x3 * 2.0, a4x3 + a4x3, "Matrix Times 2 Test", Verbose);
      Check_Matrix_Math(a4x3, b1x1, 2.0 * a4x3, a4x3 + a4x3, "2 Times Matrix Test", Verbose);
   
      Random_Numbers.Reset(Gen);
      n    := Random_Numbers.Random(Gen);
      c4x3 := z4x3;
      b1x1 := (others => (others => float(n)));
      for i in 1..n loop
         c4x3 := c4x3 + a4x3;
      end loop;
      Check_Matrix_Math(a4x3, b1x1, a4x3 * float(n), c4x3, "Matrix Times n Test", Verbose);
      Check_Matrix_Math(a4x3, b1x1, float(n) * a4x3, c4x3, "n Times Matrix Test", Verbose);
   
      exception
         when e : others =>
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line("Matrix_Math failure ...");
            Ada.Text_IO.Put_Line("Exception name: " &
                                 Ada.Exceptions.Exception_Name(e));
            Ada.Text_IO.Put_Line("Message: " &
                                 Ada.Exceptions.Exception_Message(e));
   end Matrix_Math_Test;
