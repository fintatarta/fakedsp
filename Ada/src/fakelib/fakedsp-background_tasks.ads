with Fakedsp.Data_Streams;
with Fakedsp.Protected_Buffers;
with Fakedsp.Card;

--
-- This package defines the background task that does the data I/O
-- in parallel with the processing
--
private package Fakedsp.Background_Tasks is
   task Adc_Dac is
      entry Go  (Buf_In        : Protected_Buffers.Sample_Buffer_Access;
                 Buf_Out       : Protected_Buffers.Sample_Buffer_Access;
                 Input         : Data_Streams.Data_Source_Access;
                 Output        : Data_Streams.Data_Destination_Access;
                 Handler       : Card.Callback_Handler_Access);

      entry Ready;
   end Adc_Dac;

   Adc_State : Protected_Buffers.State_Buffer;
end Fakedsp.Background_Tasks;
