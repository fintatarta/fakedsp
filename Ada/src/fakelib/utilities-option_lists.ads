with Ada.Strings;
with Ada.Containers.Indefinite_Ordered_Maps;

use Ada;

package Utilities.Option_Lists is
   package Option_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => String);

   subtype Option_List is Option_Maps.Map;

   type Case_Action is (Force_Lower, Force_Upper, Keep);

   function Parse (Input           : String;
                   Entry_Separator : Character := ',';
                   Key_Separator   : Character := '=';
                   Trim_Key        : Strings.Trim_End := Strings.Both;
                   Trim_Value      : Strings.Trim_End := Strings.Both;
                   Key_Case        : Case_Action := Force_Lower;
                   Default_Value   : String := "")
                   return Option_List;
end Utilities.Option_Lists;
