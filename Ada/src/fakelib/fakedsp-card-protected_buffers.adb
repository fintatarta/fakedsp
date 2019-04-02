pragma Ada_2012;
package body Fakedsp.Card.Protected_Buffers is

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
      is (Buffer);

   end Sample_Buffer;



end Fakedsp.Card.Protected_Buffers;
