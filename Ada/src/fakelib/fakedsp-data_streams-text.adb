pragma Ada_2012;
with Ada.Strings.Fixed;
use Ada;
with Ada.Text_IO;

package body Fakedsp.Data_Streams.Text is
   use Utilities;

   Sampling_Frequency_Key : constant String := "Fs";


   ----------
   -- Open --
   ----------

   function Open (Filename : String := Standard_IO_Name) return Text_Source_Access is

      procedure Parse_Header (Item : Text_Source_Access) is
         use Text_IO;
         use Strings;
         use Strings.Fixed;

         procedure Parse_Line (Item           : Text_Source_Access;
                               Line           : String;
                               Colon_Position : Natural)
         is
            Key   : constant String :=
                      Trim (Line (Line'First .. Colon_Position - 1), Both);

            Value : constant String :=
                      Trim (Line (Colon_Position + 1 .. Line'Last), Both);
         begin
            if Key = "" then
               Put_Line (Standard_Error, "Warning: empty key in '" & Line & "'");

            elsif Value = "" then
               Put_Line (Standard_Error, "Warning: empty value in '" & Line & "'");

            else
               if Key = Sampling_Frequency_Key then
                  Item.Frequency := Frequency_Hz'Value (Value);
               else
                  Put_Line (Standard_Error,
                            "Warning: unknown key '" & Key
                            & " ' in '" & Line & "'");
               end if;
            end if;

         end Parse_Line;

         function To_Be_Skipped (Line : String) return Boolean
         is (Line'Length = 0 or else Line (Line'First) = '#');


      begin
         Item.Empty := True;

         while not End_Of_File (Item.File) loop
            declare
               Line            : constant String := Trim (Get_Line (Item.File), Both);
               Colon_Position  : Natural;
            begin
               if not To_Be_Skipped (Line)  then
                  Colon_Position := Index (Source  => Line,
                                           Pattern => ":");

                  if Colon_Position = 0 then
                     Item.Current_Sample := Sample_Type'Value (Line);
                     Item.Empty := False;
                     return;
                  else
                     Parse_Line (Item, Line, Colon_Position);
                  end if;
               end if;
            end;
         end loop;
      end Parse_Header;

      Result : constant Text_Source_Access := new Text_Source'(File           => <>,
                                                      Top_Channel    => 1,
                                                      Frequency      => 8000.0,
                                                      Current_Sample => 0,
                                                      Empty          => False);

   begin
      if Filename = Standard_IO_Name then
         Open (Result.File, Text_IO.In_File);
      else
         Open (File => Result.file,
               Name => Filename,
               M    => Text_IO.In_File);
      end if;


      Parse_Header (Result);

      return Result;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Src           : in out Text_Source;
      Sample        : out Sample_Type;
      End_Of_Stream : out Boolean;
      Channel       : Channel_Index := Channel_Index'First)
   is
      pragma Unreferenced (Channel);
   begin
      -- Output sample is always set to the current sample, even if
      -- Empty is true and we are at the end of the file. In this way
      -- Sample has some sensible value and it makes sense too... It is
      -- like the ADC is keeping the last read value active
      Sample := Src.Current_Sample;

      if Src.Empty then
         End_Of_Stream := True;
         return;
      end if;

      if Hybrid_Files.End_Of_File (Src.File) then
         Src.Empty := True;
      else
         Src.Current_Sample := Sample_Type'Value (Hybrid_Files.Get_Line (Src.File));
         Src.Empty := False;
      end if;


   end Read;

   -----------
   -- Close --
   -----------

   procedure Close (Src : in out Text_Source) is
   begin
      Hybrid_Files.Close (Src.File);

      Src.Empty := True;
   end Close;

   ----------
   -- Open --
   ----------

   function Open
     (Filename     : String := Standard_IO_Name;
      Sampling     : Frequency_Hz := 8000.0;
      Last_Channel : Channel_Index := 1)
      return Text_Destination_Access
   is
      Result : constant Text_Destination_Access :=
                 new Text_Destination'(File           => <>,
                                       Top_Channel    => Last_Channel,
                                       Frequency      => Sampling);

   begin
      if Filename = Standard_IO_Name then
         Hybrid_Files.Open (File => Result.File,
                            M    => Text_IO.Out_File);
      else
         Hybrid_Files.Create (File => Result.File,
                              Name => Filename,
                              M    => Text_IO.In_File);

      end if;

      Hybrid_Files.Put_Line (Result.File,
                        Sampling_Frequency_Key
                        & ":"
                        & Frequency_Hz'Image (Result.Frequency));

      Hybrid_Files.New_Line (Result.File);

      return Result;
   end Open;

   -----------
   -- Write --
   -----------

   procedure Write
     (Dst     : Text_Destination;
      Sample  : Sample_Type;
      Channel : Channel_Index := Channel_Index'First)
   is
      pragma Unreferenced (Channel);
   begin
      Hybrid_Files.Put_Line (Dst.File, Sample_Type'Image (Sample));
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (Dst : in out Text_Destination) is
   begin
      Hybrid_Files.Close (Dst.File);
   end Close;

end Fakedsp.Data_Streams.Text;
