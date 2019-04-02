pragma Ada_2012;
with Ada.Directories;

with Fakedsp.Data_Streams.Wave;
--  with Fakedsp.Data_Streams.Text;

package body Fakedsp.Data_Streams.Files is

   type File_Type is (Wav, Text, Unknown);

   subtype Known_Types is
     File_Type
   range
     File_Type'First .. File_Type'Pred (File_Type'Last);

   subtype File_Extension is String (1 .. 3);

   Extension_To_Type : array (Known_Types) of File_Extension := (Wav  => "wav",
                                                                 Text => "txt");
   ------------------
   -- Name_To_Type --
   ------------------

   function Name_To_Type (Name : String) return File_Type
   is
      Ext : String := Ada.Directories.Extension (Name);
   begin
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

   function Open (Name : String) return Data_Source'Class is
   begin
      case Name_To_Type (Name) is
         when Wav =>
            return Wave.Open (Name);

         when Text =>
            raise Constraint_Error with  "Text format unimplemented";
            --              return Text.Open (Name);

         when Unknown =>
            raise Constraint_Error with "Unknown file type for '" & Name & "'";
      end case;
   end Open;

   ----------
   -- Open --
   ----------

   function Open (Name         : String;
                  Sampling     : Frequency_Hz;
                  Last_Channel : Channel_Index := 1)
                  return Data_Destination'Class
   is
   begin
      case Name_To_Type (Name) is
         when Wav =>
            return Wave.Open (Name, Sampling, Last_Channel);

         when Text =>
            raise Constraint_Error with  "Text format unimplemented";
            --              return Text.Open (Name);

         when Unknown =>
            raise Constraint_Error with "Unknown file type for '" & Name & "'";
      end case;
   end Open;

end Fakedsp.Data_Streams.Files;
