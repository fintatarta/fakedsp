--
-- A characteristic of Ada task is that if an exception happens they
-- silently die.  This can make debugging difficult.  The procedure
-- Install_Reaper installs a handler that print some useful information
-- to standard error when a task dies.
--


package Utilities.Task_Reaper  is

   procedure Install_Reaper;
end Utilities.Task_Reaper;
