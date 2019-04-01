----------------------------------------------------------------------------
-- Test commands package
----------------------------------------------------------------------------

   with Pipe_Commands; use Pipe_Commands;

   with Ada.Text_IO; use Ada.Text_IO;
   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

   procedure Pipe_Commands_Test is
   
      FileStream : stream;
   	command1 : constant string := "type pipe_commands.ads";
      command2 : constant string := "cat pipe_commands.ads";
      command3 : constant string := "tee foo.test";
      command4 : constant string := "tee foo2.test";
      Message : constant string := "This is the string I am writing.";
   
      procedure print_buf(FileStream : in stream) is
         Buffer : unbounded_String;
      begin
         loop
            begin
               Buffer := read_next(FileStream);
               put_line(to_string(Buffer));
               exception
                  when Pipe_Commands.End_of_file => 
                     exit;
            end;
         end loop;
      end print_buf;
   
   begin
      put_line("DOS listing of a file with command '" & command1 & "'");
      FileStream := execute(command1, read_file);
      print_buf(FileStream);
      close(FileStream);
   
      new_line(2);
      put_line("Listing a file with command '" & command2 & "':");
      FileStream := execute(command2, read_file);
      print_buf(FileStream);
      close(FileStream);
   
      new_line(2);
      put("Today's date is: ");
      FileStream := execute("date", read_file);
      print_buf(FileStream);
      close(FileStream);
   
      new_line(2);
      put_line("Output through a pipe to the tee command:");
      put_line("(look in foo.test for the same string.)");
      FileStream := execute(command3, write_file);
      write_next(FileStream, Message);
      close(FileStream);
   
      new_line(2);
      put_line("Attempt illegal input from a pipe to the tee command:");
      FileStream := execute(command4, write_file);
      begin
         print_buf(FileStream);
         exception
            when Access_Error =>
               put_line ("Detected improper attempt to read from a pipe opened for write only.");
      end;
      close(FileStream);
   end Pipe_Commands_Test;
