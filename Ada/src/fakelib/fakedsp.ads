--
-- This hierarchy of packages provide
--

package Fakedsp is
   Sample_Size : constant := 16;

   type Sample_Type is
   range -(2 ** (Sample_Size - 1)) .. 2 ** (Sample_Size - 1)-1
     with Size => 16;
   -- Type representing the sample read from the ADC/written to the DAC

   type Sample_Array is array (Natural range <>) of Sample_Type;


   type Frequency_Hz is delta 1.0 range 0.0 .. 1.0e6;
   -- Fixed point type (yes! Ada has fixed point... :-) used to represent
   -- frequency
end Fakedsp;
