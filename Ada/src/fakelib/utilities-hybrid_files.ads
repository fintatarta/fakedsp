with Ada.Text_IO;
package Utilities.Hybrid_Files is
   use type Text_IO.File_Mode;

   type Hybrid_File is limited private;

   function Mode (Item : Hybrid_File) return Text_IO.File_Mode;

   function Get_Line (From : Hybrid_File) return String
     with Pre => Mode (From) = Text_IO.In_File;

   function End_Of_File (From : Hybrid_File) return Boolean
     with Pre => Mode (From) = Text_IO.In_File;

   procedure Put_Line (To : Hybrid_File; Item : String)
     with Pre => Mode (To) = Text_IO.Out_File;

   procedure New_Line (To : Hybrid_File)
     with Pre => Mode (To) = Text_IO.Out_File;

   procedure Open (File : in out Hybrid_File;
                   Name : String;
                   M    : Text_IO.File_Mode)
     with Post => Mode (File) = M;


   procedure Create
     (File : in out Hybrid_File;
      Name : String;
      M    : Text_IO.File_Mode)
     with Post => Mode (File) = M;

   procedure Open (File : in out Hybrid_File;
                   M    : Text_IO.File_Mode)
     with Post => Mode (File) = M;

   procedure Close (File : in out Hybrid_File);
private
   type Hybrid_File is limited
      record
         File        : Text_IO.File_Type;
         Opened      : Boolean := False;
         Standard_IO : Boolean;
         Mode        : Text_IO.File_Mode;
      end record;


   function Get_Line (From : Hybrid_File) return String
   is (if From.Standard_IO then
          Text_IO.Get_Line
       else
          Text_IO.Get_Line (From.File));

   function End_Of_File (From : Hybrid_File) return Boolean
   is (if From.Standard_IO then
          Text_IO.End_Of_File (Text_IO.Standard_Input)
       else
          Text_IO.End_Of_File (From.File));

   function Mode (Item : Hybrid_File) return Text_IO.File_Mode
   is (Item.Mode);

end Utilities.Hybrid_Files;
