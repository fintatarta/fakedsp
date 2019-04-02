package Fakedsp.Data_Streams.Files is
   function Open (Name : String) return Data_Source'Class;

   function Open (Name : String;
                  Sampling : Frequency_Hz;
                  Last_Channel : Channel_Index := 1)
                  return Data_Destination'Class;
end Fakedsp.Data_Streams.Files;
