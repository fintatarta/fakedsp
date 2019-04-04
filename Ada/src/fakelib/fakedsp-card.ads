--****p*  Fakedsp/Fakedsp.card
-- DESCRIPTION
--  This package provides the interface to a "virtual DSP card" with an ADC
--  and a DAC. The virtual ADC reads sample at a specified sampling frequency
--  and at the same instants the virtual DAC updates its output. 
--  The user needs to register a "virtual interrupt handler"
--  that will be called every time the virtual ADC reads a new sample.
--  The interrupt handler can read data from the ADC using the function
--  Read_ADC and can write data to the DAC usign Write_DAC.
--  
--  The interrupt handler must be a variable of a type that implements 
--  (is a descendant of) the interface Callback_Handler.  Every
--  implementation of Callback_Handler must provide a procedure Sample_Ready
--  that will be called after the virtual ADC acquired a new sample.
--
--  The typical way of using this package is as follows:
--  * The user codes an implementation of the interface Callback_Handler
--    with the processing code (typically) included in the Sample_Ready 
--    procedure. 
--  * The user creates a Data_Source (see Fakedsp.Data_Streams) that will 
--    be read by the virtual ADC in order to get the "virtually 
--    acquired" samples.  The easiest way to create a Data_Source is to 
--    use the "broker constructor" in Fakedsp.Data_Streams.Files.
--  * The user creates a Data_Destination (see Fakedsp.Data_Streams) that will 
--    be written by the virtual DAC with the value produced by the processing
--    code written by the user.  
--    The easiest way to create a Data_Source is to 
--    use the "broker constructor" in Fakedsp.Data_Streams.Files.
--***
with Fakedsp.Data_Streams;

package Fakedsp.Card is
   --****t* Fakedsp.card/State_Type
   -- DESCRIPTION
   --   The "virtual acquisition card" can be in one of 3 states: Sleeping,
   --   Running, End_Of_Data.
   --
   --   At the beginning, before calling the function Start, it
   --   is in Sleeping; after start it goes Running until it ends the
   --   data from the source; when out of data, the card goes in End_Of_Data.
   --
   -- SOURCE
   type State_Type is (Sleeping, Running, End_Of_Data);
   --***
      
   --****m* Fakedsp.card/Current_State
   -- SOURCE   
   function Current_State return State_Type;
   -- DESCRIPTION
   --  Return the current state of the virtual card
   --***
   
   --****m* Fakedsp.card/Wait_For
   -- SOURCE
   procedure Wait_For (State : State_Type);
   -- DESCRIPTION
   --   Wait for the state to get equal to State and return.
   --   Similar to a while loop calling Current_State, but more efficient
   --   since it checks the state only when it changes.
   --***
   
   --****I* Fakedsp.card/Callback_Handler
   -- SOURCE
   type Callback_Handler is interface;
   
   procedure Sample_Ready (H : in out Callback_Handler) is abstract;

   -- DESCRIPTION
   --   A Callback_Handler defines an interface similar to that of 
   --   an interrupt handler.
   --
   --   A concrete implementation of interface Callback_Handler is just 
   --   required to define a procedure Sample_Ready that
   --   will be called when a new sample from the ADC is ready to be 
   --   acquired.
   --
   --   Why an object rather than a simple callback?  Because an object
   --   can carry a state.  For example, if the handler implements some
   --   filter it needs to keep the past story of the signal and this
   --   would be more cumbersome with a simple callback. 
   --***

   --****t* Fakedsp.card/Callback_Handler_Access
   -- SOURCE
   type Callback_Handler_Access is access all Callback_Handler'Class;
   --***
   
   --****m* Callback_Handler/Sample_Ready
   -- SOURCE
   --    procedure Sample_Ready (H : in out Callback_Handler) is abstract;
   --
   -- DESCRIPTION
   --   Procedure to be implemented by any concrete implementation of 
   --   Callback_Handler. It will be called every time a new sample
   --   is ready.
   --***

   --****m* Fakedsp.card/Start
   -- SOURCE
   procedure Start (Callback        : Callback_Handler_Access;
                    Input           : Data_Streams.Data_Source_Access;
                    Output          : Data_Streams.Data_Destination_Access;
                    In_Buffer_Size  : Positive := 1;
                    Out_Buffer_Size : Positive := 1)
     with
       Pre => Current_State = Sleeping,
       Post => 
         Current_State = Running
         and ADC_DMA_Size = In_Buffer_Size
         and DAC_DMA_Size = Out_Buffer_Size;
   -- DESCRIPTION
   --   Turn on the "virtual acquisition card."  Data will be read from the
   --   input source Input and written to Output.  Data can be read/write
   --   sample by sample or blockwise (simulating a kind of DMA).  The size
   --   of an input/output block (in samples) is specified by In_Buffer_Size
   --   and Out_Buffer_Size.
   --   
   --   Samples are read from the source at the sampling frequency determined
   --   by the data source.
   --
   -- NOTES
   --   So far I tested it only for In_Buffer_Size=1 and Out_Buffer_Size=1.
   --   The more general case most probably works, but it is untested.
   --***
   
   --****m* Fakedsp.card/ADC_DMA_Size,DAC_DMA_Size
   -- SOURCE
   function ADC_DMA_Size return Positive
     with Pre => Current_State > Sleeping;
   
   function DAC_DMA_Size return Positive
     with Pre => Current_State > Sleeping;
   --***
   
   --****m* Fakedsp.card/Read_ADC
   -- SOURCE
   function Read_ADC return Sample_Array
     with Pre => Current_State > Sleeping;
   
   function Read_ADC return Sample_Type
     with Pre => Current_State > Sleeping and ADC_DMA_Size = 1;
   
   function Read_ADC return Float
     with Pre => Current_State > Sleeping and ADC_DMA_Size = 1;
   -- DESCRIPTION 
   --  Read the sample (or block of samples) previously
   --  read by the virtual ADC.  Note that if this function is called
   --  more than once before the Sample_Ready method of the handler is
   --  called, the same value is returned.  Samples that are not read
   --  (because, say, the processing is too slow) are lost.
   --
   --  The three functions differ in their return argument. The functions
   --  that return single samples (Sample_Type or Float) can be called
   --  only if Start was called with Out_Buffer_Size=1
   --***


   --****m* Fakedsp.card/Write_DAC
   -- SOURCE
   procedure Write_Dac (Data : Sample_Array)
     with Pre => Current_State > Sleeping;

   procedure Write_Dac (Data : Sample_Type)
     with Pre => Current_State > Sleeping and DAC_DMA_Size = 1;
   
   procedure Write_Dac (Data : Float)
     with Pre => Current_State > Sleeping and DAC_DMA_Size = 1;
   -- DESCRIPTION
   --   Write a sample (or a block of samples) to the virtual DAC.
   -- 
   --   The two procedures accepting scalars (Sample_Type or Float)
   --   Can be called only if Start was called with Out_Buffer_Size=1
   --***

end Fakedsp.Card;
