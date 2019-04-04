with Ada.Streams.Stream_IO;

use Ada;

package Fakedsp.Data_Streams.Wave is
   type Wave_Source is  limited new Data_Source with private;
   type Wave_Source_Access is access Wave_Source;

   function Open (Filename : String) return Wave_Source_Access;

   procedure Read (Src           : in out Wave_Source;
                   Sample        : out Sample_Type;
                   End_Of_Stream : out Boolean;
                   Channel       : Channel_Index := Channel_Index'First);

   procedure Read (Src           : in out Wave_Source;
                   Sample        : out float;
                   End_Of_Stream : out Boolean;
                   Channel       : Channel_Index := Channel_Index'First);

   function Sampling_Frequency (Src : Wave_Source)
                                return Frequency_Hz;

   function Max_Channel (Src : Wave_Source)
                         return Channel_Index;

   procedure Close (src : in out Wave_Source);

   type Wave_Destination is limited new Data_Destination with private;
   type Wave_Destination_Access is access Wave_Destination;

   function Open (Filename     : String;
                  Sampling     : Frequency_Hz;
                  Last_Channel : Channel_Index := 1)
                  return Wave_Destination_Access;

   procedure Write (Dst     : Wave_Destination;
                    Sample  : Sample_Type;
                    Channel : Channel_Index := Channel_Index'First);

   procedure Write (Dst     : Wave_Destination;
                    Sample  : Float;
                    Channel : Channel_Index := Channel_Index'First);

   procedure Close (Dst : in out Wave_Destination);

   function Max_Channel (Src : Wave_Destination) return Channel_Index;


   Bad_Format : exception;
   Unimplemented_Format: exception;
private
   type Wave_Source is  limited new Data_Source
   with
      record
         File        : Streams.Stream_IO.File_Type;
         Top_Channel : Channel_Index;
         Frequency   : Frequency_Hz;
      end record;

   function Sampling_Frequency (Src : Wave_Source) return Frequency_Hz
   is (Src.Frequency);

   function Max_Channel (Src : Wave_Source) return Channel_Index
   is (Src.Top_Channel);


   type Wave_Destination is limited new Data_Destination with
      record
         File        : Streams.Stream_IO.File_Type;
         Top_Channel : Channel_Index;
         Frequency   : Frequency_Hz;
      end record;


   function Max_Channel (Src : Wave_Destination) return Channel_Index
   is (Src.Top_Channel);

end Fakedsp.Data_Streams.Wave;
