--
-- This hierarchy of packages provides a library to write code with
-- a DSP-style on a PC.  The user will be most probably interested in
-- two packages:
--
-- * Fakedsp.Card that provides the interface to the "virtual DSP card"
--
-- * Fakedsp.Data_Streams and its descendants that provide the way
--   the virtual card read/write samples.  Most probably the user will
--   use just Fakedsp.Data_Streams.Files that provide a unified interface
--   to file-based data-streams.
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

   Unspecified_Frequency : constant Frequency_Hz := 0.0;
end Fakedsp;
