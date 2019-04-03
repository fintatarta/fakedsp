with Utilities.Hybrid_Files;


package Fakedsp.Data_Streams.Text is
   type Text_Source is  limited new Data_Source with private;
   type Text_Source_Access is access Text_Source;

   Standard_IO_Name : constant String;

   function Open (Filename : String := Standard_IO_Name) return Text_Source_Access;

   procedure Read (Src           : in out Text_Source;
                   Sample        : out Sample_Type;
                   End_Of_Stream : out Boolean;
                   Channel       : Channel_Index := Channel_Index'First);

   function Sampling_Frequency (Src : Text_Source)
                                return Frequency_Hz;

   function Max_Channel (Src : Text_Source)
                         return Channel_Index;

   procedure Close (Src : in out Text_Source);

   type Text_Destination is limited new Data_Destination with private;
   type Text_Destination_Access is access Text_Destination;

   function Open (Filename     : String := Standard_IO_Name;
                  Sampling     : Frequency_Hz := 8000.0;
                  Last_Channel : Channel_Index := 1)
                  return Text_Destination_Access;

   procedure Write (Dst     : Text_Destination;
                    Sample  : Sample_Type;
                    Channel : Channel_Index := Channel_Index'First);

   procedure Close (Dst : in out Text_Destination);

   function Max_Channel (Src : Text_Destination) return Channel_Index;


   Bad_Format           : exception;
   Unimplemented_Format : exception;
private
   use Utilities.Hybrid_Files;

   Standard_IO_Name     : constant String (1 .. 1) := (1 => ASCII.NUL);



   type Text_Source is  limited new Data_Source
   with
      record
         File           : Hybrid_File;
         Top_Channel    : Channel_Index;
         Frequency      : Frequency_Hz;
         Current_Sample : Sample_Type;
         Empty          : Boolean;
      end record;

   function Sampling_Frequency (Src : Text_Source) return Frequency_Hz
   is (Src.Frequency);

   function Max_Channel (Src : Text_Source) return Channel_Index
   is (Src.Top_Channel);


   type Text_Destination is limited new Data_Destination with
      record
         File        : Hybrid_File;
         Top_Channel : Channel_Index;
         Frequency   : Frequency_Hz;
      end record;


   function Max_Channel (Src : Text_Destination) return Channel_Index
   is (Src.Top_Channel);

end Fakedsp.Data_Streams.Text;
