pragma Ada_2012;
with Ada.Directories;

with Fakedsp.Data_Streams.Wave;
with Fakedsp.Data_Streams.Text;

package body Fakedsp.Data_Streams.Files is
   subtype Known_Types is
     File_Type
   range
     File_Type'First .. File_Type'Pred (File_Type'Last);

   subtype File_Extension is String (1 .. 3);

   Extension_To_Type : constant array (Known_Types) of File_Extension :=
                         (Wav_File  => "wav",
                          Text_File => "txt");
   ------------------
   -- Name_To_Type --
   ------------------

   ------------------
   -- Name_To_Type --
   ------------------

   function Name_To_Type (Name   : String;
                          Format : File_Type) return File_Type
   is
      Ext : constant String := Ada.Directories.Extension (Name);
   begin
      if Format /= Unknown then
         return Format;
      end if;

      if Ext'Length /= File_Extension'Length then
         return Unknown;
      end if;

      for T in File_Type loop
         if Extension_To_Type (T) = Ext then
            return T;
         end if;
      end loop;

      return Unknown;
   end Name_To_Type;
   ----------
   -- Open --
   ----------

   function Open (Filename : String;
                  Format   : File_Type := Unknown)
                  return Data_Source_Access is
   begin
      if Filename = "-" then
         if Format = Unknown or Format = Text_File then
            return Data_Source_Access (Text.Standard_Input);
         else
            raise Text.Unimplemented_Format
              with "Standard input only with text format";
         end if;
      end if;

      case Name_To_Type (filename, Format) is
         when Wav_File =>
            return Data_Source_Access (Wave.Open (Filename));

         when Text_File =>
            return Data_Source_Access (Text.Text_Source_Access'(Text.Open (Filename)));

         when Unknown =>
            raise Constraint_Error with "Unknown file type for '" & Filename & "'";
      end case;
   end Open;

   ----------
   -- Open --
   ----------

   function Open (Filename     : String;
                  Sampling     : Frequency_Hz;
                  Format       : File_Type := Unknown;
                  Last_Channel : Channel_Index := 1)
                  return Data_Destination_Access
   is
   begin
      if Filename = "-" then
         if Format = Unknown or Format = Text_File then
            return Data_Destination_Access (Text.Standard_Output);
         else
            raise Text.Unimplemented_Format
              with "Standard output only with text format";
         end if;
      end if;

      case Name_To_Type (Filename, Format) is
         when Wav_File =>
            return Data_Destination_Access (Wave.Open (Filename, Sampling, Last_Channel));

         when Text_File =>
            return Data_Destination_Access (Text.Text_Destination_Access'(Text.Open (Filename)));

         when Unknown =>
            raise Constraint_Error with "Unknown file type for '" & Filename & "'";
      end case;
   end Open;

end Fakedsp.Data_Streams.Files;
