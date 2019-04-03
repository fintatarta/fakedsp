--
-- A characteristic of Ada task is that if an exception happens they
-- silently die.  This can make debugging difficult.  The procedure
-- Install_Reaper installs a handler that print some useful information
-- to standard error when a task dies.
--


package Utilities.Task_Reaper  is
   type Verbosity_Level is (Exception_Only,  Abort_Or_Exception, Always);

   procedure Install_Reaper  (Level : Verbosity_Level := Exception_Only);
   -- Install a reaper.  If Level is Exception_Only, a message is printed
   -- only if a task die because of an exception; if Level is
   -- Abort_Or_Exception a message is printed if the task call is aborted
   -- or die by an exception; if Level is Always the message is printed
   -- also for normal termination.
end Utilities.Task_Reaper;
