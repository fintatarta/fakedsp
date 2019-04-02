with Fakedsp.Data_Streams;
with Fakedsp.Protected_Buffers;

private package Fakedsp.Background_Tasks is
   task Adc_Dac is
      entry Go  (Buf_In        : Protected_Buffers.Sample_Buffer_Access;
                 Buf_Out       : Protected_Buffers.Sample_Buffer_Access;
                 Input         : Data_Streams.Data_Source_Access;
                 Output        : Data_Streams.Data_Destination_Access;
                 Handler       : Callback_Handler_Access);
   end Adc_Dac;

   Adc_State : Protected_Buffers.State_Buffer;
end Fakedsp.Background_Tasks;
