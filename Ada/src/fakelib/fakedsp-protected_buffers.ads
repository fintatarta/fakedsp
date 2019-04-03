pragma Assertion_Policy (disable);

private package Fakedsp.Protected_Buffers is

   ------------------
   -- State_Buffer --
   ------------------

   protected type State_Buffer is

      procedure Set (S : State_Type);
      entry Get (S : out State_Type);
   private
      State   : State_Type := Sleeping;
      Changed : Boolean := True;
   end State_Buffer;

   -------------------
   -- Sample_Buffer --
   -------------------

   protected type Sample_Buffer (Size : Positive) is

      procedure Put (Item : Sample_Array)
        with Pre => Item'Length = Size;

      function Get return Sample_Array
        with Post => Get'Result'Length = Size;

      function Length return Positive;
   private
      Buffer : Sample_Array (1 .. Size) := (others => 0);
   end Sample_Buffer;



   type Sample_Buffer_Access is access Sample_Buffer;

end Fakedsp.Protected_Buffers;
