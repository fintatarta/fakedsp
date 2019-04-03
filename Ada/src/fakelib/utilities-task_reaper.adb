pragma Ada_2012;
with Ada.Text_IO;

with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;

use Ada;

package body Utilities.Task_Reaper is
   Verbosity : Verbosity_Level;

   -----------------
   -- Grim_Reaper --
   -----------------


   protected Reaper is
      procedure Handler (Cause : Task_Termination.Cause_Of_Termination;
                         Id    : Task_Identification.Task_Id;
                         Excp  : Exceptions.Exception_Occurrence);
   end Reaper;


   protected body Reaper is

      ---------------
      -- Last_Gasp --
      ---------------

      procedure Handler (Cause : Task_Termination.Cause_Of_Termination;
                         Id    : Task_Identification.Task_Id;
                         Excp  : Exceptions.Exception_Occurrence)
      is
         use Task_Identification;
         use Task_Termination;
         use Exceptions;

         use Ada.Text_IO;

         function Message (Msg : String) return String
         is ("---> Task " & Image (Id) & " " & Msg);
      begin
         case Cause is
            when Normal =>
               if Verbosity = Always then
                  Put_Line (Standard_Error, Message ("exited"));
               end if;

            when Abnormal =>
               if Verbosity >= Abort_Or_Exception then
                  Put_Line (Standard_Error, Message ("aborted "));
               end if;

            when Unhandled_Exception =>
               Put_Line (Standard_Error,
                         Message ("died with exception '"
                         & Exception_Information (Excp)
                         & "'"));
         end case;
      end Handler;

   end Reaper;

   procedure Install_Reaper (Level : Verbosity_Level := Exception_Only)
   is
   begin
      Task_Termination.Set_Dependents_Fallback_Handler (Task_Reaper.Reaper.Handler'Access);
      Verbosity := Level;
   end Install_Reaper;
end Utilities.Task_Reaper;
