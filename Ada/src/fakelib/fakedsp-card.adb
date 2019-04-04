pragma Ada_2012;
with Fakedsp.Protected_Buffers;
with Fakedsp.Background_Tasks;

with Utilities.Task_Reaper;

package body Fakedsp.Card is
   use Protected_Buffers;



   In_Buffer  : Protected_Buffers.Sample_Buffer_Access := null;
   Out_Buffer : Protected_Buffers.Sample_Buffer_Access := null;


   function ADC_DMA_Size return Positive
   is (In_Buffer.Size);

   function DAC_DMA_Size return Positive
   is (Out_Buffer.Size);

   --------------
   -- Read_ADC --
   --------------

   function Read_ADC return Sample_Array is
      Tmp : constant Float_Array := In_Buffer.Get;
      Result : Sample_Array (Tmp'Range);
   begin
      for I in Tmp'Range loop
         Result (I) := Sample_Type (Tmp (I));
      end loop;

      return Result;
   end Read_ADC;

   --------------
   -- Read_ADC --
   --------------

   function Read_ADC return Float is
      Tmp : constant Float_Array (1 .. 1) := In_Buffer.Get;
   begin
      return Tmp (1);
   end Read_ADC;

   --------------
   -- Read_ADC --
   --------------

   function Read_ADC return Sample_Type
   is (Sample_Type (Float'(Read_ADC)));

   ---------------
   -- Write_Dac --
   ---------------

   procedure Write_Dac (Data : Sample_Array) is
      Tmp : Float_Array (Data'Range);
   begin
      for I in Tmp'Range loop
         Tmp (I) := Float (Data (I));
      end loop;

      Out_Buffer.Put (Tmp);
   end Write_Dac;

   ---------------
   -- Write_Dac --
   ---------------

   procedure Write_Dac (Data : Sample_Type) is
   begin
      Write_Dac (Float (Data));
   end Write_Dac;

   ---------------
   -- Write_Dac --
   ---------------

   procedure Write_Dac (Data : Float) is
      Tmp : constant Float_Array (1 .. 1) := (1 => Data);
   begin
      Out_Buffer.Put (Tmp);
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

   -------------------
   -- Current_State --
   -------------------

   function Current_State return State_Type
   is (Background_Tasks.Adc_State.Peek);



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

      Background_Tasks.Adc_Dac.Ready;
   end Start;

begin
   Utilities.Task_Reaper.Install_Reaper;
end Fakedsp.Card;
