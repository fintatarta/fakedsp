--
-- This package
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Fakedsp is
   Sample_Size : constant Natural := 16;

   type Sample_Type is mod 2 ** Sample_Size;

   type Sample_Array is array (Natural range <>) of Sample_Type;

   type Frequency_Hz is delta 1.0 range 0.0 .. 1.0e6;

   Default_Frequency : constant Frequency_Hz;

private
   Default_Frequency : constant Frequency_Hz := 0.0;

end Fakedsp;
