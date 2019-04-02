package Fakedsp.Data_Streams is
   type Channel_Index is range 1 .. 1024;

   type Data_Source is limited interface;

   type Data_Source_Access is access all Data_Source'Class;

   procedure Read (Src           : Data_Source;
                   Sample        : out Sample_Type;
                   End_Of_Stream : out Boolean;
                   Channel       : Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Src.Max_Channel;

   function Sampling_Frequency (Src : Data_Source)
                                return Frequency_Hz
                                is abstract;

   function Max_Channel (Src : Data_Source)
                         return Channel_Index
                         is abstract;

   procedure Close (Src : in out Data_Source)
   is abstract;


   type Data_Destination is limited interface;


   type Data_destination_Access is access all Data_Destination'Class;

   procedure Write (Dst     : Data_Destination;
                    Sample  : Sample_Type;
                    Channel : Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Dst.Max_Channel;


   function Max_Channel (Src : Data_Destination)
                         return Channel_Index
                         is abstract;


   procedure Close (Src : in out Data_Destination)
   is abstract;
end Fakedsp.Data_Streams;
