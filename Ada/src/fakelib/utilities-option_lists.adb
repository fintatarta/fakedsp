pragma Ada_2012;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

with Utilities.Tokenize;
with Ada.Strings;


package body Utilities.Option_Lists is

   -----------
   -- Parse --
   -----------

   function Parse
     (Input           : String;
      Entry_Separator : Character := ',';
      Key_Separator   : Character := '=';
      Trim_Key        : Boolean := True;
      Trim_Value      : Boolean := True;
      Key_Case        : Case_Action := Force_Lower;
      Default_Value   : String := "")
      return Option_List
   is
      use Tokenize;

      procedure Split_Key_Value (Item      : String;
                                 Separator : Character;
                                 Key       : out Unbounded_String;
                                 Value     : out Unbounded_String)
      is
         use Ada.Strings;

         Pos : constant Natural := Fixed.Index (Source  => Item,
                                                Pattern => Separator & "");
      begin
         if Pos = 0 then
            -- No key-value separator: it is pure key
            Key := To_Unbounded_String (Item);
            Value := To_Unbounded_String (Default_Value);

         elsif Pos = Item'Last then
            -- The separator is at the end: value is the empty string
            Key := To_Unbounded_String (Item (Item'First .. Pos - 1));
            Value := Null_Unbounded_String;

         else
            -- The separator is at the end: value is the empty string
            Key := To_Unbounded_String (Item (Item'First .. Pos - 1));
            Value := To_Unbounded_String (Item (Pos + 1 .. Item'Last));

         end if;

         if Trim_Key then
            Key := Trim (Key, Strings.Both);
         end if;

         if Key = "" then
            raise Constraint_Error with "Empty key in '" & Item & "'";
         end if;


         if Trim_Value then
            Value := Trim (Value, Strings.Both);
         end if;
      end Split_Key_Value;

      function Manage_Case (X : String; How : Case_Action) return string
      is ((case How is
              when Force_Lower =>
                 Characters.Handling.To_Lower (X),

              when Force_Upper =>
                 Characters.Handling.To_Upper (X),

              when Keep        =>
                 X));

      function Manage_Case (X : Unbounded_String; How : Case_Action) return string
      is (Manage_Case (To_String (X), How));


      Entries : constant Token_List :=
                  Split (To_Be_Splitted    => Input,
                         Separator         => Entry_Separator,
                         Collate_Separator => True);

      Result : Option_List;
   begin
      for Pair of Entries loop
         declare
            Key : Unbounded_String;
            Value : Unbounded_String;
         begin
            Split_Key_Value (Pair, Key_Separator, Key, Value);

            Result.Include (Key      => Manage_Case (Key, Key_Case),
                            New_Item => To_String (Value));
         end;
      end loop;

      return Result;
   end Parse;

end Utilities.Option_Lists;
