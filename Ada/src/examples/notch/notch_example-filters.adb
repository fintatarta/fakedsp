pragma Ada_2012;
with Ada.Numerics.Elementary_Functions;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Notch_Example.Filters is

   ----------------
   -- New_Filter --
   ----------------

   function New_Filter
     (F0   : Normalized_Frequency;
      R    : Float;
      Gain : Float := 1.0)
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
                               Gain   => Gain,
                               Status => (others => 0.0));

   end New_Filter;

   ------------------
   -- Sample_Ready --
   ------------------

   procedure Sample_Ready (X : in out Notch_Filter) is
      use Fakedsp;

      Input  : Float;
      Output : Float;


      function Saturate (X : Float) return Sample_Type;
      -- Convert a Float to Sample_Type protecting against overflow
      -- by saturating if X is outside the representable range.
      -- It should never happen, but even if just one sample overflows,
      -- the code dies on an exception; therefore, it is worth to
      -- protect against overflows. (Meglio aver paura che toccarne...)

      function Saturate (X : Float) return Sample_Type
      is (if X > Float (Sample_Type'Last) then
             Sample_Type'Last
          elsif X < Float (Sample_Type'First) then
             Sample_Type'First
          else
             Sample_Type (X));
   begin
      Input := Float (Sample_Type'(Card.Read_ADC));

      -- Do you recognize the form I used?

      Output := X.Num0 * Input + X.Status (1);

      --  Note the '-' sign in the following lines!!!
      --  Den1 and Den2 are the denominator coefficients, we must
      --  use them with the '-' sign!  (Yes... I too fell in it...)
      X.Status (1) := - Output * X.Den1 + Input * X.Num1 + X.Status (2);
      X.Status (2) := - Output * X.Den2 + Input * X.Num2;

      --        Put_Line (Input'Img & ".," & Output'Img);

      Card.Write_Dac (Saturate (Output * X.Gain));
   end Sample_Ready;

end Notch_Example.Filters;
