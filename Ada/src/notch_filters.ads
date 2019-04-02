with Fakedsp.Card;

package Notch_Filters is
   type Normalized_Frequency is digits 16 range 0.0 .. 1.0;

   type Notch_Filter is new Fakedsp.Callback_Handler with private;

   type Filter_Access is access Notch_Filter;

   function New_Filter (F0 : Normalized_Frequency; R : Float)
                        return Filter_Access;

   procedure Sample_Ready (X : in out Notch_Filter);
private
   type Memory_Type is array (1 .. 2) of Float;

   type Notch_Filter is new Fakedsp.Callback_Handler
     with
      record
         Num0, Num1, Num2 : Float;
         Den1, Den2 : Float;
         Status     : Memory_Type;
      end record;

end Notch_Filters;
