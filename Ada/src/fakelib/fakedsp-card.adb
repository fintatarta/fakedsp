pragma Ada_2012;
with Fakedsp.Protected_Buffers;
with Fakedsp.Background_Tasks;

package body Fakedsp.Card is




   In_Buffer  : Protected_Buffers.Sample_Buffer_Access := null;
   Out_Buffer : Protected_Buffers.Sample_Buffer_Access := null;


   --------------
   -- Read_ADC --
   --------------

   function Read_ADC return Sample_Array is
   begin
      return In_Buffer.Get;
   end Read_ADC;

   ---------------
   -- Write_Dac --
   ---------------

   procedure Write_Dac (Data : Sample_Array) is
   begin
      Out_Buffer.Put (Data);
   end Write_Dac;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For (State : State_Type) is
      S : State_Type;
   begin
      loop
         Background_Tasks.Adc_State.Get (S);
         exit when S = State;
      end loop;
   end Wait_For;



   -----------
   -- Start --
   -----------

   procedure Start
     (Callback        : Callback_Handler_Access;
      Input           : Data_Streams.Data_Source_Access;
      Output          : Data_Streams.Data_Destination_Access;
      In_Buffer_Size  : Positive := 1;
      Out_Buffer_Size : Positive := 1)
   is
   begin
      In_Buffer := new Protected_Buffers.Sample_Buffer (In_Buffer_Size);
      Out_Buffer := new Protected_Buffers.Sample_Buffer (Out_Buffer_Size);

      Background_Tasks.Adc_Dac.Go (Buf_In        => In_Buffer,
                                   Buf_Out       => Out_Buffer,
                                   Input         => Input,
                                   Output        => Output,
                                   Handler       => Callback);
   end Start;


end Fakedsp.Card;
