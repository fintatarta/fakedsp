--****p* Data_Streams/Files
-- DESCRIPTION
--    The user can use as data stream for sample I/O files with different
--    format.  Every implemented format corresponds to a different implementation
--    of the Data_Source or Data_Destination interface defined in the package
--    Data_Streams.
--
--    In order to make it easier to manage different format at runtime,
--    this package acts as a kind of "broker" that recognizes the format
--    and create the correct object to handle it.
--
--    The interface of this package is really simple: it provides two
--    Open functions (Open.source and Open.destination) that look
--    at the filename to guess the format if this is not explicitely
--    specified by the user.  If given the special filename the opening
--    functions use the standard input/output.
--
--    |html <h2>Options in filename</h2>
--
--    Sometimes it is necessary to give to the procedure opening the
--    source/destination some information that cannot be found in the
--    file itself.  For example, if the file is just a raw sequence
--    of samples, it is not possible to read from it the sampling
--    frequency.  As another example, when we open a data destination
--    that can accept both Sample_Type and Float formats, we cannot know
--    which format the user desires.
--
--    In order to solve this kind of problems we allow to append to the
--    filename an option string.  The option string is separated from
--    the filename by "::" and it is a sequence of assignaments
--    key=value separated by commas.  For example,
--
--    |html <center><code>/tmp/impulse.txt::fmt=float,foo,bar=0</code></center>
--
--    is a filename that identifies the file
--    |html <code>/tmp/impulse.txt</code> and associates with it three options
--    |html <code>fmt, foo</code> and
--    |html <code>bar</code> with values, respectively,
--    |html <code>float</code>, the empty string and 0.
--
--    Note that the option section is defined as the part of the filename
--    that goes
--    |html <b>from the last "::" to the end</b>.  Therefore, the following
--    filename
--
--    |html <center><code>/tmp/pippo::jimmy=9,::bar=0</code></center>
--
--    is a filename that identifies the file
--    |html <code>/tmp/pippo::jimmy=9,</code> (yes, with a comma at the end...
--    pretty silly filename, I agree...) with option
--    |html <code>bar=0</code> and
--    |html <b>not</b> a file
--    |html <code>/tmp/pippo</code> with options
--    |html <code>jimmy=9</code> and
--    |html <code>::bar=0</code>.
--
--    Note that this structure does not allow to have neither "::" nor ","  in an option
--    value.  Currently no escape mechanism is used, maybe later.
--
--    Finally, notes that the option mechanism applies
--    |html <b>also to the special name "-"</b>.
--    For example, if we open a data detination with name
--
--    |html <center><code>-::fmt=float</code></center>
--
--    the output samples will be written to the standard output
--    in floating point format.
--***
package Fakedsp.Data_Streams.Files is
   --****t* Files/File_Type
   -- SOURCE
   type File_Type is (Wav_File, Text_File, Unknown);
   -- DESCRIPTION
   --   Enumeration type representing the currently recognized format.
   --   This type is here since we want to allow the user to specify
   --   the format when the source/destination is open, without
   --   any guessing on the library side.
   --
   --   It is a good idea to extend this type when a new format is added.
   --***

   --****f* Files/Open.Source
   -- SOURCE
   function Open (Filename : String;
                  Format   : File_Type := Unknown)
                  return Data_Source_Access;
   -- DESCRIPTION
   --   Open a Data_Source based on the file with the specified filename and
   --   format (WAV, text-based, ...).   If the special
   --   filename "-" is used, the standard input is used.
   --
   --   If Format = Unknown, then:
   --   * if Filename = "-", format Text_File is used by default, otherwise
   --   * the format is guessed on the basis of the extension (maybe
   --     in the future we will check the content too).
   --
   --   Sampling frequency and number of channels are read from the
   --   specified file or from the options read from the filename
   --***

   --****f* Files/Open.Destination
   -- SOURCE
   function Open (Filename     : String;
                  Sampling     : Frequency_Hz;
                  Format       : File_Type := Unknown;
                  Last_Channel : Channel_Index := 1)
                  return Data_Destination_Access
     with Pre => Sampling /= Unspecified_Frequency;
   -- DESCRIPTION
   --   Open a Data_Destination based on the file with the specified filename and
   --   format (WAV, text-based, ...).   If the special
   --   filename "-" is used, the standard output is used.
   --
   --   If Format = Unknown, then
   --
   --   * if Filename = "-", format Text_File is used by default, otherwise
   --   * the format is guessed on the basis of the extension.
   --***

end Fakedsp.Data_Streams.Files;
