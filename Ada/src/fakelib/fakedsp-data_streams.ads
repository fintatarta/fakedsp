--
-- The data of the virtual card are read from/written to "data streams."
-- We have two kinds of data streams:
--
-- * Data_Source are used to read data from (surprised?). A data source
--   has a sampling frequency and one or more channels (currently most
--   of the code works with only one channel, but this will maybe change
--   in the future)
-- * Data_Destination used to write data (surprised again, I guess)
--
-- This package defines Data_Source and Data_Destination as interfaces.
-- The user can provide the code for its preferred data stream as long
-- as it implements the required interface.
--
-- The current code provides data streams that do I/O with WAV files and
-- data streams that do I/O with text-based files.  See descendants
-- Data_Streams.Wave and Data_Streams.Text.  See also Data_Streams.Files for
-- a flexible meta-interface to file-based data streams.
--

package Fakedsp.Data_Streams is
   type Channel_Index is range 1 .. 1024;

   type Data_Source is limited interface;

   type Data_Source_Access is access all Data_Source'Class;

   procedure Read
     (Src           : in out Data_Source;
      Sample        :    out Sample_Type;
      End_Of_Stream :    out Boolean;
      Channel       : in     Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Src.Max_Channel;
   -- Read one sample from a channel of the specified source. If
   -- the source ran out of data, End_Of_Stream is set to True, otherwise
   -- is set to False.

   procedure Read
     (Src           : in out Data_Source;
      Sample        :    out Float;
      End_Of_Stream :    out Boolean;
      Channel       : in     Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Src.Max_Channel;
   -- Read one sample from a channel of the specified source. If
   -- the source ran out of data, End_Of_Stream is set to True, otherwise
   -- is set to False.

   function Sampling_Frequency (Src : Data_Source)
                                return Frequency_Hz
                                is abstract;
   -- Return the sampling frequency of the source

   function Max_Channel (Src : Data_Source)
                         return Channel_Index
                         is abstract;
   -- Return the number of the last channel of the source

   procedure Close (Src : in out Data_Source)
   is abstract;
   -- Close the source, doing all the necessary housekeeping (if required)


   type Data_Destination is limited interface;


   type Data_destination_Access is access all Data_Destination'Class;

   procedure Write (Dst     : Data_Destination;
                    Sample  : Sample_Type;
                    Channel : Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Dst.Max_Channel;
   -- Write a sample to the destination

   procedure Write (Dst     : Data_Destination;
                    Sample  : Float;
                    Channel : Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Dst.Max_Channel;

   function Max_Channel (Src : Data_Destination)
                         return Channel_Index
                         is abstract;


   procedure Close (Src : in out Data_Destination)
   is abstract;
end Fakedsp.Data_Streams;
