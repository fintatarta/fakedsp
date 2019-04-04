pragma Ada_2012;
with Utilities;

package body Fakedsp.Data_Streams.Wave is

   subtype RIFF_Tag is String (1 .. 4);

   RIFF_Name   : constant RIFF_Tag := "RIFF";
   WAVE_Format : constant RIFF_Tag := "WAVE";
   Fmt_Name    : constant RIFF_Tag := "fmt ";
   Data_Name   : constant RIFF_Tag := "data";

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
         Format         : Audio_Format;
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

   function Open (Filename     : String)
                  return Wave_Source_Access
   is
      use Streams.Stream_IO;

      --        function Read_Riff_Chunk   is new Utilities.Read_Chunk (RIFF_Chunk);
      --        function Read_Format_Chunk is new Utilities.Read_Chunk (Format_Chunk);
      --        function Read_Data_Header  is new Utilities.Read_Chunk (Data_Chunk_Header);

      Result : constant Wave_Source_Access := new Wave_Source;
   begin
      Open (File => Result.File,
            Mode => In_File,
            Name => Filename);

      declare
         Riff : RIFF_Chunk;
      begin
         RIFF_Chunk'Read (Stream (Result.File), Riff);

         if Riff.Tag /= RIFF_Name or Riff.Format /= WAVE_Format then
            raise Bad_Format;
         end if;
      end;

      declare
         Format : Format_Chunk;
      begin
         Format_Chunk'Read (Stream (Result.File), Format);

         if Format.Tag /= Fmt_Name then
            raise Bad_Format with "[" & Format.Tag & "," & Fmt_Name & "]";
         end if;

         if Format.Bit_Per_Sample /= 16 then
            raise Unimplemented_Format with "Only 16 bit/sample implemented";
         end if;

         if Format.Num_Channels /= 1 then
            raise Unimplemented_Format with "Only single channel files implemented";
         end if;

         Result.Frequency := Frequency_Hz (Format.Sample_Rate);
         Result.Top_Channel := (Channel_Index (Format.Num_Channels) + Channel_Index'First)-1;
      end;

      declare
         Header : Data_Chunk_Header;
      begin
         Data_Chunk_Header'Read (Stream (Result.File), Header);

         if Header.Tag /= Data_Name then
            raise Bad_Format with "Expected tag 'data'";
         end if;
      end;

      return Result;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Src           : in out Wave_Source;
      Sample        : out Sample_Type;
      End_Of_Stream : out Boolean;
      Channel       : Channel_Index := Channel_Index'First)
   is

      --        function Read_Sample is new Utilities.Read_Chunk (Sample_Type);
   begin
      if Channel > Src.Top_Channel then
         raise Constraint_Error with "channel out of bound";
      end if;

      Sample_Type'Read (Streams.Stream_IO.Stream (Src.File), Sample);
      End_Of_Stream := False;
   exception
      when Streams.Stream_IO.End_Error =>
         End_Of_Stream := True;
   end Read;


   procedure Read (Src           : in out Wave_Source;
                   Sample        : out Float;
                   End_Of_Stream : out Boolean;
                   Channel       : Channel_Index := Channel_Index'First)
   is
      Tmp : Sample_Type;
   begin
      Src.Read (Sample        => Tmp,
                End_Of_Stream => End_Of_Stream,
                Channel       => Channel);

      Sample := Float (Tmp);
   end Read;


   ----------
   -- Open --
   ----------

   function Open (Filename     : String;
                  Sampling     : Frequency_Hz;
                  Last_Channel : Channel_Index := 1)
                  return Wave_Destination_Access
   is
      --        procedure Write_RIFF   is new Utilities.Write_Chunk (RIFF_Chunk);
      --        procedure Write_Fmt    is new Utilities.Write_Chunk (Format_Chunk);
      --        procedure Write_Header is new Utilities.Write_Chunk (Data_Chunk_Header);
      use Ada.Streams.Stream_IO;

      N_Channel       : constant Int32 := Int32 ((Last_Channel + 1)-Channel_Index'First);
      Byte_Per_Sample : constant Int32 := Int32 (Sample_Size / 8);

      Result          : Wave_Destination_Access;
   begin
      if Last_Channel /= 1 then
         raise Unimplemented_Format;
      end if;

      Result := new Wave_Destination;

      Result.Frequency := Sampling;
      Result.Top_Channel := Last_Channel;

      Streams.Stream_IO.Create (File => Result.File,
                                Mode => Streams.Stream_IO.Out_File,
                                Name => Filename);


      RIFF_Chunk'Write
        (Stream (Result.File),
         RIFF_Chunk'(Tag        => RIFF_Name,
                     Chunk_Size => 0,
                     Format     => WAVE_Format));

      Format_Chunk'Write
        (Stream (Result.File),
         Format_Chunk'(Tag            => Fmt_Name,
                       Chunk_Size     => 16,
                       Format         => PCM,
                       Num_Channels   => Int16 (N_Channel),
                       Sample_Rate    => Int32 (Sampling),
                       Byte_Rate      => Int32 (Sampling) * Byte_Per_Sample * N_Channel,
                       Block_Align    => Int16 (Byte_Per_Sample * N_Channel),
                       Bit_Per_Sample => Int16 (Sample_Size)));

      Data_Chunk_Header'Write
        (Stream (Result.File),
         Data_Chunk_Header'(Tag        => Data_Name,
                            Chunk_Size => 0));
      return Result;
   end Open;

   -----------
   -- Write --
   -----------

   procedure Write
     (Dst     : Wave_Destination;
      Sample  : Sample_Type;
      Channel : Channel_Index := Channel_Index'First)
   is
      --        procedure Write_Sample is new Utilities.Write_Chunk (Sample_Type);
   begin
      if Channel > Dst.Top_Channel then
         raise Constraint_Error with "Channel out of bound";
      end if;

      Sample_Type'Write (Streams.Stream_IO.Stream (Dst.File), Sample);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (Dst     : Wave_Destination;
                    Sample  : Float;
                    Channel : Channel_Index := Channel_Index'First)
   is
   begin
      Dst.Write (Sample  => Sample_Type (Sample),
                 Channel => Channel);
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (Dst : in out Wave_Destination)
   is
      use Streams.Stream_IO;

      Len : constant Streams.Stream_IO.Count := Size (Dst.File);

      --        procedure Write is new Utilities.Write_Chunk (Int32);
   begin
      Set_Index (Dst.File, 5);
      Int32'Write (Stream (Dst.File), Int32 (Len)-8);

      Set_Index (Dst.File, 41);
      Int32'Write (Stream (Dst.File), Int32 (Len)-44);

      Close (Dst.File);
   end Close;

   -----------
   -- Close --
   -----------

   procedure Close (Src : in out Wave_Source)
   is
   begin
      Streams.Stream_IO.Close (Src.File);
   end Close;


end Fakedsp.Data_Streams.Wave;
