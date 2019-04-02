pragma Ada_2012;
package body Fakedsp is

   protected type Sample_Buffer (Size : Positive) is
      procedure Put (Item : Sample_Array)
        with Pre => Item'Length = Size;

      function Get return Sample_Array
        with Post => Get'Result'Length = Size;

      function Length return Positive;
   private
      Buffer : Sample_Array (1 .. Size) := (others => 0);
   end Sample_Buffer;

   protected body Sample_Buffer is
      procedure Put (Item : Sample_Array)
      is
      begin
         Buffer := Item;
      end Put;

      function Length return Positive
      is (Size);

      function Get return Sample_Array
      is (Buffer);

   end Sample_Buffer;

   type Sample_Buffer_Access is access Sample_Buffer;



   In_Buffer  : Sample_Buffer_Access := null;
   Out_Buffer : Sample_Buffer_Access := null;

   protected State_Buffer is
      procedure Set (S : State_Type);
      entry Get (S : out State_Type);
   private
      State   : State_Type := Sleeping;
      Changed : Boolean := True;
   end State_Buffer;

   protected body State_Buffer is
      procedure Set (S : State_Type)
      is
      begin
         State := S;
         Changed := True;
      end Set;

      entry Get (S : out State_Type) when Changed
      is
      begin
         S := State;
         Changed := False;
      end Get;
   end State_Buffer;

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
         State_Buffer.Get (S);
         exit when S = State;
      end loop;
   end Wait_For;

   type Data_Out_Stream is null record;
   type Data_In_Stream is null record;
   task Adc_Dac is
      entry Go  (Buf_In        : Sample_Buffer_Access;
                 Buf_Out       : Sample_Buffer_Access;
                 Input         : Data_In_Stream;
                 Output        : Data_Out_Stream;
                 Sampling_Freq : Frequency_Hz;
                 Handler       : New_Sample_Callback);
   end Adc_Dac;

   task body Adc_Dac is
      task Reader  is
         entry Read_From (Source : Data_In_Stream);
         entry Get (Item : out Sample_Type;
                    Eof  : out Boolean);
      end Reader;

      task body Reader
      is
         Input    : Data_In_Stream;
         Sample   : Sample_Type;
         Continue : Boolean := True;
      begin
         accept Read_From (Source : in Data_In_Stream) do
            Input := Source;
         end Read_From;

         while Continue loop
            Sample := 0;
            accept Get (Item : out Sample_Type; Eof : out Boolean) do
               Item := Sample;
               Eof := False;
               Continue := not Eof;
               pragma Compile_Time_Warning (True, "PLACEHOLDER");
            end Get;
         end loop;
      end Reader;

      task Writer is
         entry Write_To (Destination : Data_Out_Stream);
         entry Put (Item :  Sample_Type);
      end Writer;

      task body Writer is
         Output : Data_Out_Stream;
         Sample : Sample_Type;
      begin
         accept Write_To (Destination : in Data_Out_Stream) do
            Output := Destination;
         end Write_To;

         loop
            Sample := 0;

            select
               accept Put (Item : in Sample_Type) do
                  pragma Compile_Time_Warning (True, "PLACEHOLDER");
                  null;
               end Put;
            or
               terminate;
            end select;
         end loop;
      end Writer;

      Shared_In_Buffer  : Sample_Buffer_Access;
      Shared_Out_Buffer : Sample_Buffer_Access;

      Sampling_Period : Duration;
      User_Callbcak : New_Sample_Callback;
   begin
      accept Go  (Buf_In : Sample_Buffer_Access;
                  Buf_Out : Sample_Buffer_Access;
                  Input : Data_In_Stream;
                  Output : Data_Out_Stream;
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

         loop
            delay Sampling_Period;

            Reader.Get (In_Buffer (In_Cursor), Eof);

            if Eof then
               State_Buffer.Set (S => End_Of_Data);
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

   -----------
   -- Start --
   -----------

   procedure Start
     (Callback        : New_Sample_Callback;
      Input           : Data_Source;
      Output          : Data_Destination;
      Sampling_Freq   : Frequency_Hz := Default_Frequency;
      In_Buffer_Size  : Positive := 1;
      Out_Buffer_Size : Positive := 1)
   is
      In_Stream : Data_In_Stream;
      Out_Stream : Data_Out_Stream;
   begin
      In_Buffer := new Sample_Buffer (In_Buffer_Size);
      Out_Buffer := new Sample_Buffer (Out_Buffer_Size);

      Adc_Dac.Go (Buf_In        => In_Buffer,
                  Buf_Out       => Out_Buffer,
                  Input         => In_Stream,
                  Output        => Out_Stream,
                  Sampling_Freq => Sampling_Freq,
                  Handler       => Callback);
   end Start;

end Fakedsp;
