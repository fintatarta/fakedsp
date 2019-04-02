pragma Ada_2012;
with Ada.Unchecked_Conversion;

package body Utilities is

   ----------------
   -- Read_Chunk --
   ----------------


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
         raise Stream_IO.End_Error;
      end if;

      return Convert (Buffer);
   end Read_Chunk;



   -----------------
   -- Write_Chunk --
   -----------------


   procedure Write_Chunk (To   : Streams.Stream_IO.File_Type;
                          Item : Chunk_Type)
   is
      use Streams;

      subtype Buffer_Type is
        Stream_Element_Array (1 .. Chunk_Type'Size / Stream_Element'Size);

      function Convert is
        new Unchecked_Conversion (Source => Chunk_Type,
                                  Target => Buffer_Type);

      Buffer : constant Buffer_Type := Convert (Item);
   begin
      Stream_IO.Write (File => To,
                       Item => Buffer);
   end Write_Chunk;
end Utilities;
