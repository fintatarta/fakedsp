package Fakedsp.Data_Streams.Files is
   function Open (Name : String) return Data_Source_Access;

   function Open (Name         : String;
                  Sampling     : Frequency_Hz;
                  Last_Channel : Channel_Index := 1)
                  return Data_Destination_Access;
end Fakedsp.Data_Streams.Files;
