pragma Ada_2012;
with Utilities;

package body Fakedsp.Data_Streams.Wave is

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


   ----------
   -- Open --
   ----------

   function Open (Filename : String) return Wave_Source is
      use Streams.Stream_IO;

      function Read_Riff_Chunk   is new Utilities.Read_Chunk (RIFF_Chunk);
      function Read_Format_Chunk is new Utilities.Read_Chunk (Format_Chunk);
      function Read_Data_Header  is new Utilities.Read_Chunk (Data_Chunk_Header);
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
      function Read_Sample is new Utilities.Read_Chunk (Sample_Type);
   begin
      if Channel > Src.Top_Channel then
         raise Constraint_Error with "channel out of bound";
      end if;

      Sample := Read_Sample (Src.File);
      End_Of_Stream := False;
   exception
      when Streams.Stream_IO.End_Error =>
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

   -----------
   -- Close --
   -----------

   procedure Close (Dst : in out Wave_Destination)
   is
   begin
         --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Close;

   -----------
   -- Close --
   -----------

   procedure Close (src : in out Wave_Source)
   is
   begin
      Streams.Stream_IO.Close (Src.File);
   end Close;


end Fakedsp.Data_Streams.Wave;
