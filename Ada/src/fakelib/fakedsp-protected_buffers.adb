pragma Ada_2012;
pragma Assertion_Policy (disable);

package body Fakedsp.Protected_Buffers is

   ------------------
   -- State_Buffer --
   ------------------


   protected body State_Buffer is
      ---------
      -- Set --
      ---------

      procedure Set (S : State_Type)
      is
      begin
         State := S;
         Changed := True;
      end Set;

      ---------
      -- Get --
      ---------

      entry Get (S : out State_Type) when Changed
      is
      begin
         S := State;
         Changed := False;
      end Get;

      function Peek return State_Type
      is (State);
   end State_Buffer;

   -------------------
   -- Sample_Buffer --
   -------------------


   protected body Sample_Buffer is
      ---------
      -- Put --
      ---------

      procedure Put (Item : Sample_Array)
      is
      begin
         Buffer := Item;
      end Put;

      function Length return Positive
      is (Size);

      function Get return Sample_Array
      is
      begin
         return Buffer;
      end Get;

   end Sample_Buffer;



end Fakedsp.Protected_Buffers;
