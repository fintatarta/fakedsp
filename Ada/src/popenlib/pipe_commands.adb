--------------------------------------------------------------------------------
-- PACKAGE BODY: Pipe_Commands
--
-- PURPOSE: Implementation of a thick Ada binding for calling the popen and 
--          pclose commands imported from C.
--------------------------------------------------------------------------------

with Interfaces.C; use Interfaces.C;
with Ada.Characters.Latin_1;
with System;

package body Pipe_Commands is
   
   C_Constant_EOF : Integer;
   pragma Import (C, C_Constant_EOF);
   EOF : constant Integer := C_Constant_EOF;
   
   LF : constant Integer
     := Character'Pos (Ada.Characters.Latin_1.LF); -- Unix end of line
   
   -----------------------------------------------------------------------
   -- INTERNAL FUNCTION: Popen
   --
   -- PURPOSE: Thin binding to the C "popen" command, used by the Execute
   --          function.
   -----------------------------------------------------------------------
   function Popen (Command : Char_Array; Mode : Char_Array) return Files;
   pragma Import (C, Popen);
   
   -----------------------------------------------------------------------
   -- INTERNAL FUNCTION: pclose
   --
   -- PURPOSE: Thin binding to the C "pclose" command, used by the Close
   --          procedure.
   -----------------------------------------------------------------------
   function Pclose (FileStream : Files) return Integer;
   pragma Import (C, Pclose);
   
   -----------------------------------------------------------------------
   -- INTERNAL FUNCTION: fgetc
   --
   -- PURPOSE: Thin binding to the C "fgetc" function, used by Get_Next
   --          function
   -----------------------------------------------------------------------
   
   -----------------------------------------------------------------------
   -- INTERNAL FUNCTION: fputc
   --
   -- PURPOSE: Thin binding to the C "fput" function, used by Write_Next
   --          function
   -----------------------------------------------------------------------
   function Fgetc (C_Stream : in Files) return Integer;
   pragma Import (C, Fgetc);
   
   function Fputc (C : Integer; Stream : Files) return Integer;
   pragma Import (C, Fputc);
   
   -----------------------------------------------------------------------
   -- FUNCTION: Execute
   --
   -- PURPOSE: This command executes the process indicated in the Command
   --          parameter, setting I/O according to the IO_Type parameter.
   --
   -- RETURN VALUE: The stream corresponding to the opened pipe, including
   --               the C file pointer and the mode for which the pipe was
   --               opened.
   -- EXCEPTIONS RAISED: None
   -----------------------------------------------------------------------
   function Execute (Command : in String; IO_Type : in IO_Mode) return Stream is
      Result : Stream;
   begin
      case IO_Type is
         when Read_File  => Result.FileStream := Popen (To_C (Command), To_C ("r"));
         when Write_File => Result.FileStream := Popen (To_C (Command), To_C ("w"));
      end case;
      Result.Mode := IO_Type;
      return Result;
   end Execute;
   
   -----------------------------------------------------------------------
   -- FUNCTION: Read_Next
   --
   -- PURPOSE: Reads the next line from the stream indicated by the parameter
   --          FromFile, returning an unbounded string.
   -- RETURN VALUE: An unbounded string containing the line read from the 
   --               stream.
   --
   -- EXCEPTIONS RAISED:
   --  Access_Error => when the stream was opened with write_file mode
   --  End_Of_File  => when the pipe is closed (the program indicated
   --                  by the parameter FromFile terminates).
   -----------------------------------------------------------------------
   function Read_Next (FromFile : in Stream)
                          return Unbounded_String is
      Result         : Unbounded_String := Null_Unbounded_String;
      Char_Buf       : Integer;
      C_Constant_EOF : Integer;
      pragma Import (C, C_Constant_EOF);
      EOF            : constant Integer := C_Constant_EOF;
   begin
      if Fromfile.Mode = Write_File then
         raise Access_Error;
      end if;
      --------------------------------------------------------------------
      -- Read characters one at a time until a line feed character is 
      -- encountered, indicating an end of line. The line feed character
      -- is NOT included in the returned unbounded string.
      --------------------------------------------------------------------
      loop
         Char_Buf := Fgetc (FromFile.FileStream);
         if Char_Buf = EOF then
            raise End_Of_File;
         end if;
         exit when Char_Buf = LF;
         Result := Result & Character'Val (Char_Buf);
      end loop;
      return Result;
   end Read_Next;
   
   function Read (FromFile : in Stream;
                  N_Bytes  : in Positive := 2;
                  Signed   : in Boolean := True)
                  return Integer 
   is
      type Buffer_Type is mod 2 ** 64;
      
      Result   : Buffer_Type := 0;
      Weight   : Buffer_Type := 1;
      Char_Buf : Integer;
      Top      : constant Buffer_Type := 8 ** N_Bytes;
   begin
      for K in 1 .. N_Bytes loop
         Char_Buf := Fgetc (FromFile.FileStream);
         if Char_Buf = EOF then
            raise End_Of_File;
         end if;

         Result := Result + Weight * Buffer_Type (Char_Buf);
         Weight := Weight * 256;
      end loop;
      
      if (Signed and then Result > (Top / 2)) then
         return Integer (Result - Top);
      else
         return Integer (Result);
      end if;
   end Read;
   
   -----------------------------------------------------------------------
   -- PROCEDURE: Write_Next
   --
   -- PURPOSE: Write a line of input to the stream indicated by the
   --          parameter ToFile.
   --
   -- EXCEPTIONS RAISED:
   --    Access_Error => when the stream was opened with mode Read_File
   -----------------------------------------------------------------------
   procedure Write_Next (ToFile : in Stream; Message : in String) is
      Rc : Integer;
   begin
      if ToFile.Mode = Read_File then
         raise Access_Error;
      end if;
      for I in Message'Range loop
         Rc := Fputc (Character'Pos (Message (I)), ToFile.FileStream);
      end loop;
      Rc := Fputc (LF, ToFile.FileStream); -- add end of line
   end Write_Next;
   
   -----------------------------------------------------------------------
   -- PROCEDURE: Close
   --
   -- PURPOSE: Close the stream to the parameter OpenFile
   --
   -- EXCEPTIONS RAISED: None
   -----------------------------------------------------------------------
   procedure Close (OpenFile : in Stream) is
      Rc : Integer;
   begin
      Rc := Pclose (OpenFile.FileStream);
   end Close;
   
end Pipe_Commands;
