--
-- The user can use as data stream for sample I/O files with different
-- format.  Every implemented format corresponds to a different implementation
-- of the Data_Source/Data_Destination interface defined in the package
-- Data_Streams.
--
-- In order to make it easier to manage different format at runtime,
-- this package acts as a kind of "broker" that recognizes the format
-- and create the correct object to handle it.
--
-- The interface of this package is really simple: it provides two
-- Open functions that look at the filename to guess the format.  The
-- functions are able to handle also standard input/output via the
-- special filename "-".
--
package Fakedsp.Data_Streams.Files is
   type File_Type is (Wav_File, Text_File, Unknown);

   function Open (Filename : String;
                  Format   : File_Type := Unknown)
                  return Data_Source_Access;
   -- Open a Data_Source based on the file with the specified filename and
   -- format (WAV, text-based, ...).   If the special
   -- filename "-" is used, the standard input is used.
   --
   -- If Format = Unknown, then
   --
   -- (a) if Filename = "-", format Text_File is used by default, otherwise
   --
   -- (b) the format is guessed on the basis of the extension (maybe
   --     in the future we will check the content too).
   --
   -- Sampling frequency and number of channels are read from the
   -- specified file.

   function Open (Filename     : String;
                  Sampling     : Frequency_Hz;
                  Format       : File_Type := Unknown;
                  Last_Channel : Channel_Index := 1)
                  return Data_Destination_Access
     with Pre => Sampling /= Unspecified_Frequency;
   -- Open a Data_Destination based on the file with the specified filename and
   -- format (WAV, text-based, ...).   If the special
   -- filename "-" is used, the standard output is used.
   --
   -- If Format = Unknown, then
   --
   -- (a) if Filename = "-", format Text_File is used by default, otherwise
   --
   -- (b) the format is guessed on the basis of the extension (maybe
   --     in the future we will check the content too).

end Fakedsp.Data_Streams.Files;
