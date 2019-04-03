pragma Ada_2012;
package body Utilities.Hybrid_Files is


   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (To : Hybrid_File; Item : String) is
   begin
      if To.Standard_IO then
         Text_IO.Put_Line (Item);
      else
         Text_IO.Put_Line (To.File, Item);
      end if;
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (To : Hybrid_File) is
   begin
      if to.Standard_IO then
         Text_IO.New_Line;
      else
         Text_IO.New_Line (To.File);
      end if;
   end New_Line;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out Hybrid_File;
      Name : String;
      M    : Text_IO.File_Mode)
   is
   begin
      Text_IO.Open (File => File.File,
                    Mode => M,
                    Name => Name);

      File.Opened := True;
      File.Standard_IO := False;
      File.Mode := M;
   end Open;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out Hybrid_File;
      Name : String;
      M    : Text_IO.File_Mode)
   is
   begin
      Text_IO.Create (File => File.File,
                      Mode => M,
                      Name => Name);

      File.Opened := True;
      File.Standard_IO := False;
      File.Mode := M;
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out Hybrid_File;
      M    : Text_IO.File_Mode)
   is
   begin
      File.Opened := True;
      File.Standard_IO := True;
      File.Mode := M;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Hybrid_File) is
   begin
      if not File.Standard_IO then
         Text_IO.Close (File.File);
      end if;

      File.Opened := False;
   end Close;

end Utilities.Hybrid_Files;
