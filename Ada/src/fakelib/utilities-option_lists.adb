pragma Ada_2012;
with Utilities.Tokenize;

package body Utilities.Option_Lists is

   -----------
   -- Parse --
   -----------

   function Parse
     (Input           : String;
      Entry_Separator : Character := ',';
      Key_Separator   : Character := '=';
      Trim_Key        : Strings.Trim_End := Strings.Both;
      Trim_Value      : Strings.Trim_End := Strings.Both;
      Key_Case        : Case_Action := Force_Lower;
      Default_Value   : String := "")
      return Option_List
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Parse unimplemented");
      return raise Program_Error with "Unimplemented function Parse";
   end Parse;

end Utilities.Option_Lists;
