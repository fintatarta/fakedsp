pragma Ada_2012;
with Ada.Unchecked_Conversion;

package body Fakedsp.Data_Streams.Wave is

   EOF : exception;

   subtype RIFF_Tag is String (1 .. 4);

   type Int32 is mod 2 ** 32 with Size => 32;
   type Int16 is mod 2 ** 16 with Size => 16;

   type Audio_Format is (PCM) with Size => 16;

   for Audio_Format use (PCM => 1);

   type RIFF_Chunk is
      record
         Tag        : RIFF_Tag;
         Chunk_Size : Int32;
         Format     : RIFF_Tag;
      end record;

   type Format_Chunk is
      record
         Tag            : RIFF_Tag;
         Chunk_Size     : Int32;
         Audio_Format   : Int16;
         Num_Channels   : Int16;
         Sample_Rate    : Int32;
         Byte_Rate      : Int32;
         Block_Align    : Int16;
         Bit_Per_Sample : Int16;
      end record;

   type Data_Chunk_Header is
      record
         Tag          : RIFF_Tag;
         Chunk_Size   : Int32;
      end record;

   generic
      type Chunk_Type is private;
   function Read_Chunk (From : Streams.Stream_IO.File_Type) return Chunk_Type;

   function Read_Chunk (From : Streams.Stream_IO.File_Type) return Chunk_Type
   is
      use Streams;

      subtype Buffer_Type is
        Stream_Element_Array (1 .. Chunk_Type'Size / Stream_Element'Size);

      function Convert is
        new Unchecked_Conversion (Source => Buffer_Type,
                                  Target => Chunk_Type);

      Buffer : Buffer_Type;
      Last   : Stream_Element_Count;
   begin
      Stream_IO.Read (File => From,
                      Item => Buffer,
                      Last => Last);

      if Last /= Buffer'Last then
         raise EOF;
      end if;

      return Convert (Buffer);
   end Read_Chunk;
   ----------
   -- Open --
   ----------

   function Open (Filename : String) return Wave_Source is
      use Streams.Stream_IO;

      function Read_Riff_Chunk is new Read_Chunk (RIFF_Chunk);
      function Read_Format_Chunk is new Read_Chunk (Format_Chunk);
      function Read_Data_Header is new Read_Chunk (Data_Chunk_Header);
   begin
      return Result : Wave_Source do
         Open (File => Result.File,
               Mode => In_File,
               Name => Filename);

         declare
            Riff : constant RIFF_Chunk := Read_Riff_Chunk (Result.File);
         begin
            if Riff.Tag /= "RIFF" or Riff.Format /= "WAVE" then
               raise Bad_Format;
            end if;
         end;

         declare
            Format : constant Format_Chunk := Read_Format_Chunk (Result.File);
         begin
            if Format.Tag /= "fmt "then
               raise Bad_Format;
            end if;

            if Format.Bit_Per_Sample /= 16 then
               raise Unimplemented_Format with "Only 16 bit/sample implemented";
            end if;

            if Format.Num_Channels /= 1 then
               raise Unimplemented_Format with "Only single channel files implemented";
            end if;

            Result.Frequency := Frequency_Hz (Format.Sample_Rate);
            Result.Top_Channel := Channel_Index (Format.Num_Channels - 1) + Channel_Index'First;
         end;

         declare
            Header : constant Data_Chunk_Header := Read_Data_Header (Result.File);
         begin
            if Header.Tag /= "data" then
               raise Bad_Format with "Expected tag 'data'";
            end if;
         end;
      end return;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Src           : Wave_Source;
      Sample        : out Sample_Type;
      End_Of_Stream : out Boolean;
      Channel       : Channel_Index := Channel_Index'First)
   is
      function Read_Sample is new Read_Chunk (Sample_Type);
   begin
      if Channel > Src.Top_Channel then
         raise Constraint_Error with "channel out of bound";
      end if;

      Sample := Read_Sample (Src.File);
      End_Of_Stream := False;
   exception
      when EOF =>
         End_Of_Stream := True;
   end Read;

   ----------
   -- Open --
   ----------

   function Open
     (Filename     : String;
      Sampling     : Frequency_Hz;
      Last_Channel : Channel_Index := 1)
      return Wave_Destination
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Open unimplemented");
      return raise Program_Error with "Unimplemented function Open";
   end Open;

   -----------
   -- Write --
   -----------

   procedure Write
     (Dst     : Wave_Destination;
      Sample  : Sample_Type;
      Channel : Channel_Index := Channel_Index'First)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

end Fakedsp.Data_Streams.Wave;
