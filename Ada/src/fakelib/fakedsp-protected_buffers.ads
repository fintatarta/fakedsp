--
-- This package defines few protected objects used to communicate
--
pragma Assertion_Policy (disable);

with Fakedsp.Card;  use Fakedsp.Card;

private package Fakedsp.Protected_Buffers is

   type Float_Array is array  (Positive range <>) of Float;

   ------------------
   -- State_Buffer --
   ------------------

   protected type State_Buffer is

      procedure Set (S : State_Type);
      entry Get (S : out State_Type);
      function Peek return State_Type;
   private
      State   : State_Type := Sleeping;
      Changed : Boolean := True;
   end State_Buffer;

   -------------------
   -- Sample_Buffer --
   -------------------

   protected type Sample_Buffer (Size : Positive) is

      procedure Put (Item : Float_Array)
        with Pre => Item'Length = Size;

      function Get return Float_Array
        with Post => Get'Result'Length = Size;

      function Length return Positive;
   private
      Buffer : Float_Array (1 .. Size) := (others => 0.0);
   end Sample_Buffer;



   type Sample_Buffer_Access is access Sample_Buffer;

end Fakedsp.Protected_Buffers;
