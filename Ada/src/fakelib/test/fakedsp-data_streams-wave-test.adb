procedure Fakedsp.Data_Streams.Wave.Test is
   Source : Wave_Source_Access := Open ("test_data/a.wav");

   Dst    : Wave_Destination_Access := Wave.Open (Filename     => "test_data/rumenta/b.wav",
                                           Sampling     => Source.Sampling_Frequency,
                                           Last_Channel => Source.Max_Channel);

   X      : Sample_Type;
   Eof    : Boolean;
begin
   loop
      Source.Read (Sample        => X,
                   End_Of_Stream => Eof);

      exit when Eof;

      Dst.Write (X);
   end loop;

   Source.Close;
   Dst.Close;

   declare
      Q : Wave_Source_Access := Open ("test_data/rumenta/b.wav");
   begin
      null;
   end;
end Fakedsp.Data_Streams.Wave.Test;
