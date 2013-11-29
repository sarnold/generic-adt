-----------------------------------------------------------------------------
--  Allan Hancock College
--  CS152, Spring 2000
--  Assignment Simple Scanner
--  Stephen L Arnold
--  stand-alone program unit Simple Scanner
-----------------------------------------------------------------------------
--  Description: Simple Scanner is a scanner that recognizes certain
--  identifiers, integers, and a limited set of single character symbols
--  The program will prompt for and retrieve the input text file name
--  if it is not provided on the command line.  Identifiers start with a
--  letter and can be composed of any combination of letters, digits, and
--  the underscore.  Integers always start with a base ten digit and can be
--  composed of any combination of base ten digits and the underscore.
--
--  Symbols are any character from the following set:
--    "~!@#$%^&*()_+=-{}[]<>|/,.?"
--  White space (i.e., blanks, tabs, and line terminators) always separates
--  lexical elements.  Any symbol character except the underscore also
--  separates lexical elements.
-----------------------------------------------------------------------------
   with Ada.Command_Line, Ada.Text_IO;
   with Ada.Strings.Maps.Constants, Ada.Strings.Unbounded;
	
   use Ada.Command_Line, Ada.Text_IO;
   use Ada.Strings.Maps.Constants, Ada.Strings.Unbounded;
   use Ada.Strings.Maps;

   Procedure Simple_Scanner is
   
      Input_File : File_Type;
      Filename : string(1..512);
      Last : natural;
      Buffer : string(1..1024);
      Text : unbounded_string;
      i,j : natural;
      ID_Starters : constant character_set := Letter_Set;
      ID_Followers : constant character_set := Alphanumeric_Set or To_Set('_');
      Symbols : constant character_set := To_Set("~!@#$%^&*()_+=-{}[]<>|/,.?");
      Triggers : constant character_set := ID_Starters or Symbols or Decimal_Digit_Set;
   
   
   begin -- Simple_Scanner program
   
   -- open or prompt for input file
      if Argument_Count = 1 then
         open (Input_File, In_File, Argument(1));
      else
         loop
           Put("Please enter the input file name: ");
           Get_Line(Filename, Last);
           exit when Last in Filename'range;
         end loop;
         Put_Line("Next time use the command line.");
         Open(Input_File, In_File, Filename(Filename'First..Last));
      end if;
   
   -- main processing loop
      while not End_Of_File(Input_File) loop
         Get_Line(Input_File, Buffer, Last); -- grab a line from the file
         if Last in Buffer'range then   -- process if it's not empty
            Text := To_Unbounded_String(Buffer(Buffer'First..Last));
            while Length(Text) > 0 loop
            -- find index of first character in lexeme
               i := Index(Text, Triggers);
               exit when i = 0;
            -- bail superfluous stuff
               if i > 1 then
                  Delete(Text, 1, i-1);   -- unbounded strings always start at index=1
               end if;
            -- determine the lexeme
               if Is_In(Element(Text, 1), ID_Starters) then
                  Find_Token(Text, ID_Followers, Ada.Strings.Inside, i, j);
                  Put_Line("Identifier => " & '"' & Slice(Text, i, j) & '"');
               elsif  Is_In(Element(Text, 1), Decimal_Digit_Set) then
                  Find_Token(Text, Decimal_Digit_Set, Ada.Strings.Inside, i, j);
                  Put_Line("Integer => "& Slice(Text, i, j));
               else
               -- I guess it must be a symbol
                  Put_Line("Symbol => "& (Element(Text, 1)));
                  j := 1;
               end if;
               Delete(Text, 1, j);
            end loop;
         end if;
      end loop;
      close(Input_File);
      exception
	     	when Name_Error | Use_error =>
      		Put_Line("Invalid filename -- please try again.");
 end Simple_Scanner;
