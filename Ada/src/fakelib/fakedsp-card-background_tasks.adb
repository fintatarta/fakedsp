pragma Ada_2012;
package body Fakedsp.Card.Background_Tasks is


   -------------
   -- Adc_Dac --
   -------------

   task body Adc_Dac is
      use Data_Streams;

      task Reader  is
         entry Read_From (Source : in Data_Source_Access);
         entry Get (Item : out Sample_Type;
                    Eof  : out Boolean);
      end Reader;

      task body Reader
      is
         Sample   : Sample_Type;
         Continue : Boolean := True;
         Input    : Data_Source_Access;
         End_Of_Data : Boolean;
      begin
         accept Read_From (Source : in Data_Source_Access) do
            Input := Source;
         end Read_From;

         End_Of_Data := False;

         while not End_Of_Data loop
            Input.Read (Sample, End_Of_Data);

            accept Get (Item : out Sample_Type; Eof : out Boolean) do
               Item := Sample;
               Eof := End_Of_Data;
            end Get;
         end loop;
      end Reader;

      task Writer is
         entry Write_To (Dst : Data_Destination_Access);
         entry Put (Item :  Sample_Type);
      end Writer;

      task body Writer is
         Output : Data_Destination_Access;
         Sample : Sample_Type;
      begin
         accept Write_To (Dst : in Data_Destination_Access) do
            Output := Dst;
         end Write_To;

         loop
            select
               accept Put (Item : in Sample_Type) do
                  Sample := Item;
               end Put;
            or
               terminate;
            end select;


            Output.Write (Sample);
         end loop;
      end Writer;

      Shared_In_Buffer  : Protected_Buffers.Sample_Buffer_Access;
      Shared_Out_Buffer : Protected_Buffers.Sample_Buffer_Access;

      Sampling_Period : Duration;
      User_Callbcak   : New_Sample_Callback;
   begin
      accept Go  (Buf_In        : Protected_Buffers.Sample_Buffer_Access;
                  Buf_Out       : Protected_Buffers.Sample_Buffer_Access;
                  Input         : Data_Streams.Data_Source_Access;
                  Output        : Data_Streams.Data_Destination_Access;
                  Sampling_Freq : Frequency_Hz;
                  Handler       : New_Sample_Callback)
      do
         Shared_In_Buffer := Buf_In;
         Shared_Out_Buffer := Buf_Out;
         Sampling_Period := Duration (1.0 / Sampling_Freq);

         User_Callbcak := Handler;

         Reader.Read_From (Input);
         Writer.Write_To (Output);
      end Go;

      declare
         In_Buffer  : Sample_Array (1 .. Shared_In_Buffer.Length);
         Out_Buffer : Sample_Array (1 .. Shared_Out_Buffer.Length);
         In_Cursor  : Positive := In_Buffer'First;
         Out_Cursor : Positive := Out_Buffer'First;
         Eof        : Boolean;
      begin
         Out_Buffer := Shared_Out_Buffer.Get;
         Adc_State.Set (Running);

         loop
            delay Sampling_Period;

            Reader.Get (In_Buffer (In_Cursor), Eof);

            if Eof then
               Adc_State.Set (S => End_Of_Data);
               exit;
            end if;

            if In_Cursor = In_Buffer'Last then
               Shared_In_Buffer.Put (In_Buffer);

               In_Cursor := In_Buffer'First;

               User_Callbcak.all;
            else
               In_Cursor := In_Cursor + 1;
            end if;

            Writer.Put (Out_Buffer (Out_Cursor));

            if Out_Cursor = Out_Buffer'Last then
               Out_Buffer := Shared_Out_Buffer.Get;

               Out_Cursor := Out_Buffer'First;
            else
               Out_Cursor := Out_Cursor + 1;
            end if;

         end loop;
      end;
   end Adc_Dac;

end Fakedsp.Card.Background_Tasks;
