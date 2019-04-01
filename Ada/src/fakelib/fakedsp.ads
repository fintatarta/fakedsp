--
-- This package
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Fakedsp is
   Sample_Size : constant Natural := 16;

   type Sample_Type is mod 2 ** Sample_Size;

   type Sample_Array is array (Natural range <>) of Sample_Type;

   function Read_ADC return Sample_Array;

   procedure Write_Dac (Data : Sample_Array);


   type State_Type is (Sleeping, Running, End_Of_Data);
   procedure Wait_For (State : State_Type);

   type Data_Source is private;

   function File (Name : String) return Data_Source;
   function Pipe_From (Command_Name : String) return Data_Source;
   function From_Environment (Var_Name : String) return Data_Source;

   type Data_Destination is private;

   function File (Name : String) return Data_Destination;
   function Pipe_To (Command_Name : String) return Data_Destination;
   function From_Environment (Var_Name : String) return Data_Destination;


   type Frequency_Hz is delta 1.0 range 0.0 .. 1.0e6;

   Default_Frequency : constant Frequency_Hz;

   type New_Sample_Callback is access procedure;

   procedure Start (Callback        : New_Sample_Callback;
                    Input           : Data_Source;
                    Output          : Data_Destination;
                    Sampling_Freq   : Frequency_Hz := Default_Frequency;
                    In_Buffer_Size  : Positive := 1;
                    Out_Buffer_Size : Positive := 1);
private
   Default_Frequency : constant Frequency_Hz := 0.0;

   type Source_Or_Dest_Class is (File, Pipe, Env);

   type Source_Or_Dest is
      record
         Class : Source_Or_Dest_Class;
         Name  : Unbounded_String;
      end record;

   function Basic_Environment (Var_Name : String) return Source_Or_Dest
   is (Source_Or_Dest'(Class => Env,
                       Name  => To_Unbounded_String (Var_Name)));

   function Basic_Pipe (Command_Name : String) return Source_Or_Dest
   is (Source_Or_Dest'(Class => Pipe,
                       Name  => To_Unbounded_String (Command_Name)));

   function Basic_File (Name : String) return Source_Or_Dest
   is (Source_Or_Dest'(Class => File,
                       Name  => To_Unbounded_String (Name)));

   type Data_Source is new Source_Or_Dest;

   type Data_Destination is new Source_Or_Dest;

   function File (Name : String) return Data_Source
   is (Basic_File (Name));

   function File (Name : String) return Data_Destination
   is (Basic_File (Name));

   function Pipe_From (Command_Name : String) return Data_Source
   is (Basic_Pipe (Command_Name));

   function Pipe_To (Command_Name : String) return Data_Destination
   is (Basic_Pipe (Command_Name));

   function From_Environment (Var_Name : String) return Data_Source
   is (Basic_Environment (Var_Name));

   function From_Environment (Var_Name : String) return Data_Destination
   is (Basic_Environment (Var_Name));

end Fakedsp;
