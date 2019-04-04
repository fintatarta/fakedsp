--****p* Fakedsp/Data_Streams
-- DESCRIPTION
--   Since the virtual card is not... a real one (really?) the samples
--   read by the ADC need to come from some external source and also
--   the samples sento to the virtual DAC need to be written somewehere.
--
--   The most obvious choice for said external source/destinations are
--   files.  However, there are many possible format available such as
--   WAV, octave, pure text, and so on...  Moreover, the file could be a
--   file on disk or, in a Linux environment, the standard input/output or
--   a network connection or...
--
--   In order to not commit ourselves to a specific choice and allowing
--   for future expansions, the library introduces two abstract
--   interfaces that describe the minimum required to a data source/destination
--   and that can be specialized to specific formats. Currenty the library
--   provides implementations for WAV format and a text-based format
--   compatible with the octave text format.
--
--   The two interfaces defined in this package are:
--   * Data_Source are used to read data from (surprised?).
--   * Data_Destination used to write data (surprised again, I guess)
--
--   Both Data_Source and Data_Destination have a sampling frequency
--   and one or more channels.  They can I/O both values of type
--   Sample_Type (closer to what actually happen with a real ADC/DAC)
--   or of type Float.  We decided of allowing saving Float values
--   since in some case we could want to post-process the output produced
--   by the processing without the additional noise due to the quantization
--   done in order to send data to the DAC.
--
-- NOTE
--   Currently most of the code is designed to work with a single channel
--   only.  Maybe this will change in the future.
--***

package Fakedsp.Data_Streams is
   type Channel_Index is range 1 .. 1024;

   --****I* Data_Streams/Data_Source
   -- SOURCE
   type Data_Source is limited interface;

   type Data_Source_Access is access all Data_Source'Class;
   -- DESCRIPTION
   --   Abstract interface representing the generic data source.
   --   A concrete implementation of this interface needs to define:
   --   * procedure Read to get the next sample
   --   * function Sampling_Frequency that returns the
   --     sampling frequency of the source
   --   * function Max_Channel returning the number of the
   --     last channel
   --   * procedure Close that... close the source.
   --***

   --****m* Data_Source/Read
   -- SOURCE
   procedure Read
     (Src           : in out Data_Source;
      Sample        :    out Sample_Type;
      End_Of_Stream :    out Boolean;
      Channel       : in     Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Src.Max_Channel;

   procedure Read
     (Src           : in out Data_Source;
      Sample        :    out Float;
      End_Of_Stream :    out Boolean;
      Channel       : in     Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Src.Max_Channel;
   -- DESCRIPTION
   --   Read one sample from a channel of the specified source. If
   --   the source ran out of data, End_Of_Stream is set to True, otherwise
   --   is set to False.
   --***

   --****m* Data_Source/Sampling_Frequency
   -- SOURCE
   function Sampling_Frequency (Src : Data_Source) return Frequency_Hz is abstract;
   -- DESCRIPTION
   --   Return the sampling frequency of the source
   --***


   --****m* Data_Source/Max_Channel
   -- SOURCE
   function Max_Channel (Src : Data_Source) return Channel_Index is abstract;
   -- DESCRIPTION
   --   Return the number of the last channel of the source
   --***


   --****m* Data_Source/Close
   -- SOURCE
   procedure Close (Src : in out Data_Source) is abstract;
   -- DESCRIPTION
   --  Close the source, doing all the necessary housekeeping (if required)
   --***

   --****I* Data_Streams/Data_Destination
   -- SOURCE
   type Data_Destination is limited interface;


   type Data_destination_Access is access all Data_Destination'Class;
   -- DESCRIPTION
   --   Abstract interface representing the generic data destination.
   --   A concrete implementation of this interface needs to define:
   --   * procedure Write to output the next sample
   --   * function Max_Channel returning the number of the
   --     last channel
   --   * procedure Close that... close the destination.
   --***


   --****m* Data_Destination/Write
   -- SOURCE
   procedure Write (Dst     : Data_Destination;
                    Sample  : Sample_Type;
                    Channel : Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Dst.Max_Channel;

   procedure Write (Dst     : Data_Destination;
                    Sample  : Float;
                    Channel : Channel_Index := Channel_Index'First)
   is abstract
     with Pre'Class => Channel <= Dst.Max_Channel;
   -- DESCRIPTION
   --    Output a sample to the destination
   --***

   --****m* Data_Destination/Max_Channel
   -- SOURCE
   function Max_Channel (Src : Data_Destination) return Channel_Index is abstract;
   -- DESCRIPTION
   --   Return the number of the last channel of the source
   --***


   --****m* Data_Destination/Close
   -- SOURCE
   procedure Close (Src : in out Data_Destination) is abstract;
   -- DESCRIPTION
   --   Close the destination, doing all the necessary housekeeping
   --***

end Fakedsp.Data_Streams;
