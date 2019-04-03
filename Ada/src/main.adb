with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Command_Line;     use Ada.Command_Line;


with Fakedsp.Card;
with Fakedsp.Data_Streams.Files;
with Notch_Filters;

procedure Main is
   --
   -- This example processes the input using a notch filter that remove
   -- a specified frequency F0 placing the poles at distance R from
   -- the origin.  Both F0 and R are specified by the user on the
   -- command line. By default R=0.9.
   --

   Parsing_Error : exception;

   procedure Parse_Command_Line
     (F0          : out Fakedsp.Frequency_Hz;
      Pole_Radius : out Float;
      Src         : out Fakedsp.Data_Streams.Data_Source_Access;
      Dst         : out Fakedsp.Data_Streams.Data_Destination_Access)
   is
   begin
      if not (Argument_Count in  3 .. 4) then
         raise Parsing_Error;
      end if;

      F0 := Fakedsp.Frequency_Hz'Value (Argument (1));

      Src := Fakedsp.Data_Streams.Files.Open (Argument (2));

      Dst := Fakedsp.Data_Streams.Files.Open (Filename     => Argument (3),
                                              Sampling     => Src.Sampling_Frequency,
                                              Last_Channel => 1);

      Pole_Radius := (if Argument_Count = 4 then
                         Float'Value (Argument (4))
                      else
                         0.9);
   end Parse_Command_Line;


   Src : Fakedsp.Data_Streams.Data_Source_Access;
   Dst : Fakedsp.Data_Streams.Data_Destination_Access;

   F0          : Fakedsp.Frequency_Hz;
   Pole_Radius : Float;

begin
   -- Read from the command line the filter parameters F0 and
   -- the radius of the poles, togheter with the name of the files
   -- used for I/O.  Note that the special name "-" can be used to
   -- denote standard input/output.   Note that the sampling frequency
   -- can be read from the source.
   Parse_Command_Line (F0, Pole_Radius, Src, Dst);

   declare
      use Notch_Filters;
      use Fakedsp;

      -- Create the required notch filter.  Note that the type Notch_Filter
      -- (defined in the package Notch_Filters) implements the callback
      -- interface defined in Faksdsp.Card.  Therefore, we can use the
      -- Notch_filter as handler of the virtual interrupts produced by
      -- virtual card.
      Filter : constant Notch_Filters.Filter_Access :=
                 New_Filter (F0 => Normalized_Frequency (F0 / Src.Sampling_Frequency),
                             R  => Pole_Radius);
   begin
      --  That is, just turn the virtual card on
      Card.Start (Callback        => Card.Callback_Handler_Access (Filter),
                  Input           => Src,
                  Output          => Dst);

      -- ...and wait for the end
      Card.Wait_For (Card.End_Of_Data);
   end;

   Src.Close;
   Dst.Close;

   Set_Exit_Status (Success);

exception
   when Parsing_Error =>
      Put_Line (Standard_Error,  "Usage: "& Command_Name & " f0 file_in file_out [pole_radius]");
      Set_Exit_Status (Failure);
end Main;
