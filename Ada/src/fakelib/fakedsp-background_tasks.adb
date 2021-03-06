pragma Ada_2012;
--  with Ada.Text_IO; use Ada.Text_IO;
package body Fakedsp.Background_Tasks is
   use Fakedsp.Card;

   -------------
   -- Adc_Dac --
   -------------

   task body Adc_Dac is
      use Data_Streams;

      task Reader  is
         entry Read_From (Source : in Data_Source_Access);
         entry Get (Item : out Float;
                    Eof  : out Boolean);
      end Reader;

      task body Reader
      is
         Sample   : Float;
         Input    : Data_Source_Access;
         End_Of_Data : Boolean;
      begin
         select
            accept Read_From (Source : in Data_Source_Access) do
               Input := Source;
            end Read_From;
         or
            terminate;
         end select;

         End_Of_Data := False;

         while not End_Of_Data loop
            Input.Read (Sample, End_Of_Data);

            select
               accept Get (Item : out Float; Eof : out Boolean) do
                  Item := Sample;
                  Eof := End_Of_Data;
               end Get;
            or
               terminate;
            end select;
         end loop;
      end Reader;

      task Writer is
         entry Write_To (Dst : Data_Destination_Access);
         entry Put (Item :  Float);
      end Writer;

      task body Writer is
         Output : Data_Destination_Access;
         Sample : Float;
      begin
         select
            accept Write_To (Dst : in Data_Destination_Access) do
               Output := Dst;
            end Write_To;
         or
            terminate;
         end select;

         loop
            select
               accept Put (Item : in Float) do
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
      User_Callbcak   : Callback_Handler_Access;
   begin
      select
         accept Go  (Buf_In        : Protected_Buffers.Sample_Buffer_Access;
                  Buf_Out       : Protected_Buffers.Sample_Buffer_Access;
                  Input         : Data_Streams.Data_Source_Access;
                  Output        : Data_Streams.Data_Destination_Access;
                  Handler       : Callback_Handler_Access)
      do
         Shared_In_Buffer := Buf_In;
         Shared_Out_Buffer := Buf_Out;
         Sampling_Period := Duration (1.0 / Input.Sampling_Frequency);

         User_Callbcak := Handler;

         Reader.Read_From (Input);
         Writer.Write_To (Output);
         end Go;
      or
         terminate;
      end select;
--        Put_Line ("T=" & Sampling_Period'Img);
      declare
         In_Buffer  : Protected_Buffers.float_Array (1 .. Shared_In_Buffer.Length);
         Out_Buffer : Protected_Buffers.float_Array (1 .. Shared_Out_Buffer.Length);
         In_Cursor  : Positive := In_Buffer'First;
         Out_Cursor : Positive := Out_Buffer'First;
         Eof        : Boolean;
      begin
         Out_Buffer := Shared_Out_Buffer.Get;
         Adc_State.Set (Running);

         accept Ready;

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

               User_Callbcak.Sample_Ready;
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

end Fakedsp.Background_Tasks;
