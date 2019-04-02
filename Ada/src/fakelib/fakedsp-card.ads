with Fakedsp.Data_Streams;

package Fakedsp.Card is
   procedure Start (Callback        : Callback_Handler_Access;
                    Input           : Data_Streams.Data_Source_Access;
                    Output          : Data_Streams.Data_destination_Access;
                    In_Buffer_Size  : Positive := 1;
                    Out_Buffer_Size : Positive := 1);
   -- Turn on the "virtual acquisition card."  Data will be read from the
   -- input source Input and written to Output.  Data can be read/write
   -- sample by sample or blockwise (simulating a kind of DMA).  The size
   -- of an input/output block (in samples) is specified by In_Buffer_Size
   -- and Out_Buffer_Size.
   --
   -- Samples are read from the source at the sampling frequency determined
   -- by the data source.


   function Read_ADC return Sample_Array;
   --  Read the sample previously read by the virtual ADC.  Note that if
   --  this function is called more than once before the Sample_Ready
   --  method of the handler is called, the same value is returned.  Samples
   --  that are not read (because, say, the processing is too slow) are lost.

   procedure Write_Dac (Data : Sample_Array);

   procedure Wait_For (State : State_Type);

end Fakedsp.Card;
