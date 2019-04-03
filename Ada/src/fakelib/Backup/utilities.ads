--
-- Many consider a "Utilities" package bad style and maybe it is true.
-- However, often you need some "odds and ends" stuff that is not
-- strictly related to the main code.
--
with Ada.Streams.Stream_IO;

use Ada;
package Utilities is
   -- A generic function and a generic procedure to write "stuff" to
   -- a stream.  Maybe the usual stream functions would suffice.
   generic
      type Chunk_Type is private;
   function Read_Chunk (From : Streams.Stream_IO.File_Type) return Chunk_Type;

   generic
      type Chunk_Type is private;
   procedure Write_Chunk (To   : Streams.Stream_IO.File_Type;
                          Item : Chunk_Type);
end Utilities;
