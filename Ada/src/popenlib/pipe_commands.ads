--------------------------------------------------------------------------------
-- PACKAGE: Pipe Commands
--
-- PURPOSE: Provide a thick Ada binding to the UNX / WIN32 popen and pclose
--          commands. This allows an Ada program to call another program and
--          either send output to that program, or read input from that
--          program.
--
-- USAGE: Execute the command with mode read_file when you want to read the
-- output of the program you start. Execute the command with write_mode when
-- you want to write to the input of the program you start.
--
-- EXCEPTIONS:
--   Access_Error => Raised when a mode violation is attempted
--   End_Of_file  => Raised when the pipe is closed upon a read
--------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

package Pipe_Commands is
   
   type Stream is private;
   
   type IO_MODE is (Read_File, Write_File);
   
   function Execute (Command : in String; IO_Type : in IO_Mode)
                     return Stream;
   
   function Read_Next (FromFile : in Stream)
                       return Unbounded_String;
   
   
   function Read (FromFile : in Stream;
                  N_Bytes  : in Positive := 2;
                  Signed   : in Boolean := True)
                  return Integer
     with 
       Pre => (8 * N_Bytes <= Integer'Size);
   
   procedure Write_Next (ToFile : in Stream; Message : in String);
   
   procedure Close (OpenFile : in Stream);
   
   Access_Error : exception; -- Raised when attempt is made to violate IO_MODE
   End_Of_File  : exception; -- Raised on detection of End_of_file during read
   
private
   
   subtype Files is System.Address; -- Corresponds to a C file pointer
   
   type Stream is record
      FileStream : Files;
      Mode       : IO_Mode;
   end record;
   
end Pipe_Commands;
