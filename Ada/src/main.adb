with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Command_Line;     use Ada.Command_Line;


with Fakedsp.Card;
with Fakedsp.Data_Streams.Files;
with Notch_Filters;

procedure Main is

   Src : Fakedsp.Data_Streams.Data_Source_Access;
   Dst : Fakedsp.Data_Streams.Data_destination_Access;

   F0  : Integer;




   R   : constant Float := 0.9;

begin
   if Argument_Count /= 3 then
      Put_Line (Standard_Error, "Voglio f0, file in, file out");
      Set_Exit_Status (Failure);
      return;
   end if;

   F0 := Integer'Value (Argument (1));

   Src := Fakedsp.Data_Streams.Files.Open (Argument (2));
   Dst := Fakedsp.Data_Streams.Files.Open (Name         => Argument (3),
                                           Sampling     => Src.Sampling_Frequency,
                                           Last_Channel => 1);

   declare
      use Notch_Filters;
      use Fakedsp;

      Filter : constant Filter_Access :=
        New_Filter (F0 => Normalized_Frequency (Float (F0) / Float (Src.Sampling_Frequency)),
                    R  => R);
   begin
      Card.Start (Callback        => Callback_Handler_Access (Filter),
                  Input           => Src,
                  Output          => Dst);

      Card.Wait_For (End_Of_Data);
   end;

   Src.Close;
   Dst.Close;
end Main;
