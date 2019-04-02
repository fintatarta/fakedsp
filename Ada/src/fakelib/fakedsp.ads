--
-- This package
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Fakedsp is
   Sample_Size : constant Natural := 16;

   type Sample_Type is
   range -(2 ** (Sample_Size - 1)) .. 2 ** (Sample_Size - 1)-1
     with Size => 16;
   -- Type representing the sample read from the ADC/written to the DAC

   type Sample_Array is array (Natural range <>) of Sample_Type;


   type Frequency_Hz is delta 1.0 range 0.0 .. 1.0e6;

   Default_Frequency : constant Frequency_Hz;


   type Callback_Handler is interface;
   --  A Callback_Handler play a role similar to an interrupt handler
   --  A Callback_Handler must define a procedure Sample_Ready that
   --  will be called when a sample is ready at the ADC to be read.
   --  Why an object rather than a simple callback?  Because an object
   --  can carry a state (e.g., the old samples in a filter)

   procedure Sample_Ready (H : in out Callback_Handler)
   is abstract;

   type Callback_Handler_Access is access all Callback_Handler'Class;


   type State_Type is (Sleeping, Running, End_Of_Data);
   -- The "virtual acquisition card" can be in one of these 3 states


private
   Default_Frequency : constant Frequency_Hz := 0.0;

end Fakedsp;
