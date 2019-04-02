pragma Ada_2012;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

package body Notch_Filters is

   ----------------
   -- New_Filter --
   ----------------

   function New_Filter
     (F0 : Normalized_Frequency;
      R : Float)
      return Filter_Access
   is
      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      C : constant Float := -2.0 * Cos (2.0 * Pi * Float (F0));
   begin
      return new Notch_Filter'(Num0   => 1.0,
                               Num1   => C,
                               Num2   => 1.0,
                               Den1   => C * R,
                               Den2   => R * R,
                               Status => (others => 0.0));

   end New_Filter;

   ------------------
   -- Sample_Ready --
   ------------------

   procedure Sample_Ready (X : in out Notch_Filter) is
      use Fakedsp;
      use Fakedsp.Card;

      buffer : Sample_Array (1 .. 1);
      Input : Float;
      Output : Float;
   begin
      Buffer := Card.Read_ADC;
      Input := Float (Buffer (1));

      Output := 0.35*(X.Num0 * Input + X.Status (1));
      X.Status (1) := Output * X.Den1 + Input * X.Num1 + X.Status (2);
      X.Status (2) := Output * X.Den2 + Input * X.Num2;

--        Put_Line (Input'Img & ".," & Output'Img);
      Buffer (1) := Sample_Type (Output);
      Card.Write_Dac (Buffer);
   end Sample_Ready;

end Notch_Filters;
