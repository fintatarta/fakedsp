with Fakedsp.Data_Streams;

package Fakedsp.Card is
    type State_Type is (Sleeping, Running, End_Of_Data);
   -- The "virtual acquisition card" can be in one of these 3 states.
   -- At the beginning, before calling the function Start, it
   -- is in Sleeping; after start it goes Running until it ends the
   -- data from the source; when out of data, the card goes in End_Of_Data.

   function Current_State return State_Type;
   -- Return the current state of the virtual card

   procedure Wait_For (State : State_Type);
   -- Wait for the state to get equal to State and return.
   -- Similar to a while loop calling Current_State, but more efficient
   -- since it checks the state only when it changes.

   type Callback_Handler is interface;
   --  A Callback_Handler play a role similar to an interrupt handler
   --  A Callback_Handler must define a procedure Sample_Ready that
   --  will be called when a sample is ready at the ADC to be read.
   --  Why an object rather than a simple callback?  Because an object
   --  can carry a state (e.g., the old samples in a filter)

   procedure Sample_Ready (H : in out Callback_Handler)
   is abstract;

   type Callback_Handler_Access is access all Callback_Handler'Class;

   procedure Start (Callback        : Callback_Handler_Access;
                    Input           : Data_Streams.Data_Source_Access;
                    Output          : Data_Streams.Data_destination_Access;
                    In_Buffer_Size  : Positive := 1;
                    Out_Buffer_Size : Positive := 1)
     with
       Pre => Current_State = Sleeping,
       Post => Current_State = Running;

   -- Turn on the "virtual acquisition card."  Data will be read from the
   -- input source Input and written to Output.  Data can be read/write
   -- sample by sample or blockwise (simulating a kind of DMA).  The size
   -- of an input/output block (in samples) is specified by In_Buffer_Size
   -- and Out_Buffer_Size.
   --
   -- Samples are read from the source at the sampling frequency determined
   -- by the data source.


   function Read_ADC return Sample_Array
       with Pre => Current_State > Sleeping;

   --  Read the sample previously read by the virtual ADC.  Note that if
   --  this function is called more than once before the Sample_Ready
   --  method of the handler is called, the same value is returned.  Samples
   --  that are not read (because, say, the processing is too slow) are lost.

   function Read_ADC return Sample_Type
     with Pre => Current_State > Sleeping;

   -- Similar to the function above, but it can be called only when Start
   -- was called with Out_Buffer_Size=1

   procedure Write_Dac (Data : Sample_Array)
       with Pre => Current_State > Sleeping;
   -- Write a block of samples to the virtual DAC


   procedure Write_Dac (Data : Sample_Type)
       with Pre => Current_State > Sleeping;
   -- Similar to the function above, but it can be called only when Start
   -- was called with Out_Buffer_Size=1

end Fakedsp.Card;
