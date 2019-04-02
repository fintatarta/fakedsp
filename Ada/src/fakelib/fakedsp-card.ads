with Fakedsp.Data_Streams;

package Fakedsp.Card is
   function Read_ADC return Sample_Array;

   procedure Write_Dac (Data : Sample_Array);

   type State_Type is (Sleeping, Running, End_Of_Data);

   procedure Wait_For (State : State_Type);

   type Callback_Handler is interface;

   procedure Sample_Ready (H : in out Callback_Handler)
   is abstract;

   type Callback_Handler_Access is access all Callback_Handler'Class;

   procedure Start (Callback        : Callback_Handler_Access;
                    Input           : Data_Streams.Data_Source_Access;
                    Output          : Data_Streams.Data_destination_Access;
                    In_Buffer_Size  : Positive := 1;
                    Out_Buffer_Size : Positive := 1);
end Fakedsp.Card;
