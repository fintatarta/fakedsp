pragma Ada_2012;
with Ada.Text_IO;

with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;

use Ada;

package body Utilities.Task_Reaper is

   -----------------
   -- Grim_Reaper --
   -----------------


   protected Reaper is
      procedure Handler (C : Task_Termination.Cause_Of_Termination;
                         T : Task_Identification.Task_Id;
                         X : Exceptions.Exception_Occurrence);
   end Reaper;


   protected body Reaper is

      ---------------
      -- Last_Gasp --
      ---------------

      procedure Handler (C : Task_Termination.Cause_Of_Termination;
                         T : Task_Identification.Task_Id;
                         X : Exceptions.Exception_Occurrence)
      is
         use Task_Identification;
         use Task_Termination;
         use Exceptions;

         use Ada.Text_IO;
      begin
         Put (Standard_Error, "---> Task " & Image (T) & " ");
         case C is
            when Normal =>
               Put_Line (Standard_Error, "exited");
            when Abnormal =>
               Put_line (Standard_Error, "aborted ");
            when Unhandled_Exception =>
               Put_Line (Standard_Error, "died with exception '"
                         & Exception_Information (X)
                         & "'");
         end case;
      end Handler;

   end Reaper;

   procedure Install_Reaper
   is
   begin
      Task_Termination.Set_Dependents_Fallback_Handler (Task_Reaper.Reaper.Handler'Access);
   end Install_Reaper;
end Utilities.Task_Reaper;
