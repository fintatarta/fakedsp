with Fakedsp.Card;

package Notch_Example.filters is
   type Normalized_Frequency is digits 16 range 0.0 .. 1.0;

   type Notch_Filter is new Fakedsp.Card.Callback_Handler with private;

   type Filter_Access is access Notch_Filter;

   function New_Filter (F0   : Normalized_Frequency;
                        R    : Float;
                        Gain : Float := 1.0)
                        return Filter_Access;
   -- Create a notch filter removing the specified normalized frequency
   -- and with the compensation poles placed at radius R.  A global gain
   -- can be specified.

   procedure Sample_Ready (X : in out Notch_Filter);
   -- Callback function required by the Callback_Handler interface
private
   type Memory_Type is array (1 .. 2) of Float;

   type Notch_Filter is new Fakedsp.Card.Callback_Handler
   with
      record
         Num0, Num1, Num2 : Float;        -- Numerator coefficients
         Den1, Den2       : Float;        -- Denominator coefficients
         Status           : Memory_Type;  -- Past samples
         Gain             : Float;        --
      end record;

end Notch_Example.Filters;
