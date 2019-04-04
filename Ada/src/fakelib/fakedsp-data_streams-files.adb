pragma Ada_2012;
with Ada.Strings.Fixed;

with Fakedsp.Data_Streams.Wave;
with Fakedsp.Data_Streams.Text;
with Utilities.Option_Lists;

use Ada;
use Utilities;
with Ada.Text_IO;
with Ada.Directories;

package body Fakedsp.Data_Streams.Files is

   subtype Known_Types is
     File_Type
   range
     File_Type'First .. File_Type'Pred (File_Type'Last);

   subtype File_Extension is String (1 .. 3);


   type Parsed_Filename (N : Positive) is
      record
         Name    : String (1 .. N);
         Typ     : File_Type;
         Options : Option_Lists.Option_List;
      end record;

   function Parse (Filename : String;
                   Format   : File_Type) return Parsed_Filename
   is
      use Option_Lists;

      Extension_To_Type : constant array (Known_Types) of File_Extension :=
                            (Wav_File  => "wav",
                             Text_File => "txt");
      ------------------
      -- Name_To_Type --
      ------------------

      function Name_To_Type (Name   : String;
                             Format : File_Type) return File_Type
      is
      begin
         if Format /= Unknown then
            return Format;
         end if;

         if Name = "-" then
            if Format = Unknown or Format = Text_File then
               return Text_File;
            else
               raise Text.Unimplemented_Format
               with "Standard input only with text format";
            end if;
         end if;

         declare
            Ext : constant String := Ada.Directories.Extension (Name);
         begin
            Ada.Text_IO.Put_Line ("E=(" & Ext & ")(" & Name & ")" );

            if Ext'Length /= File_Extension'Length then
               return Unknown;
            end if;

            for T in File_Type loop
               if Extension_To_Type (T) = Ext then
                  return T;
               end if;
            end loop;
         end;

         return Unknown;
      end Name_To_Type;

      Pos             : constant Natural := Strings.Fixed.Index (Source  => Filename,
                                                                 Pattern => "::",
                                                                 Going   => Strings.Backward);

      Optionless_Name : constant String := (if Pos = 0 then
                                               Filename
                                            else
                                               Filename (Filename'First .. Pos - 1));

      Options        : constant Option_List := (if Pos = 0 then
                                                   Empty_List
                                                else
                                                   Parse (Filename (Pos + 2 .. Filename'Last)));

   begin
      Ada.Text_IO.Put_Line ("F=(" & Optionless_Name & ")");
      return Parsed_Filename'(N       => Optionless_Name'Length,
                              Name    => Optionless_Name,
                              Typ     => Name_To_Type (Optionless_Name, Format),
                              Options => Options);
   end Parse;


   ----------
   -- Open --
   ----------

   function Open (Filename : String;
                  Format   : File_Type := Unknown)
                  return Data_Source_Access
   is
      Parsed : constant Parsed_Filename := Parse (Filename, Format);
   begin
      if Parsed.Name = "-" then
         if Parsed.Typ = Unknown or Parsed.Typ = Text_File then
            return Data_Source_Access (Text.Standard_Input (Parsed.Options));
         else
            raise Text.Unimplemented_Format
            with "Standard input only with text format";
         end if;
      end if;

      case Parsed.Typ is
         when Wav_File =>
            return Data_Source_Access (Wave.Open (Parsed.Name));

         when Text_File =>
            return Data_Source_Access (Text.Open_Source (Parsed.Name));

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
      Parsed : constant Parsed_Filename := Parse (Filename, Format);
   begin
      if Parsed.Name = "-" then
         if Parsed.Typ = Unknown or Parsed.Typ = Text_File then
            return Data_Destination_Access (Text.Standard_Output (Parsed.Options));
         else
            raise Text.Unimplemented_Format
            with "Standard input only with text format";
         end if;
      end if;

      case Parsed.Typ is
         when Wav_File =>
            return Data_Destination_Access (Wave.Open (Parsed.Name, Sampling, Last_Channel));

         when Text_File =>
            return Data_Destination_Access (Text.Open_Destination (Filename => Parsed.Name,
                                                                   Options  => Parsed.Options));

         when Unknown =>
            raise Constraint_Error with "Unknown file type for '" & Filename & "'";
      end case;
   end Open;

end Fakedsp.Data_Streams.Files;
