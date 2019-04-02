with Ada.Streams.Stream_IO;

use Ada;
package Utilities is
   generic
      type Chunk_Type is private;
   function Read_Chunk (From : Streams.Stream_IO.File_Type) return Chunk_Type;

   generic
      type Chunk_Type is private;
   procedure Write_Chunk (To   : Streams.Stream_IO.File_Type;
                          Item : Chunk_Type);
end Utilities;
