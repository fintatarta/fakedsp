--****p*  Fakedsp/Fakedsp
-- DESCRIPTION
--  The library fakedsp allows  to write code with a DSP-style on a normal PC.
--  I wrote this to help my students in doing their lab activities.
--
--  The user of the library will be most probably interested in
--  the package Fakedsp.Card that provides the interface to the
--  "virtual DSP card."
--
--  Another package of interest is probably Fakedsp.Data_Streams.Files
--  that implement the Data_Source/Data_Destination interfaces
--  (defined in Fakedsp.Data_Streams) that represent the main abstraction
--  for data I/O.
--***
package Fakedsp is
   -- Number of bits of the sample read from the ADC/written to the DAC
   Sample_Size : constant := 16;

   -- Type representing the sample read from the ADC/written to the DAC
   type Sample_Type is
   range -(2 ** (Sample_Size - 1)) .. 2 ** (Sample_Size - 1)-1
     with Size => 16;

   type Sample_Array is array (Natural range <>) of Sample_Type;


   type Frequency_Hz is delta 1.0 range 0.0 .. 1.0e6;
   -- Fixed point type (yes! Ada has fixed point... :-) used to represent
   -- frequency

   Unspecified_Frequency : constant Frequency_Hz := 0.0;
end Fakedsp;
