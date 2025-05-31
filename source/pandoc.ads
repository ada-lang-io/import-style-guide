with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with League.JSON.Objects;
with League.Strings.Hash;
with League.Strings;
with League.JSON.Values;

package Pandoc is

   type String_Array is array (Natural range <>) of
     League.Strings.Universal_String;

   type Object_Type is
     (Block_Plain,
      Block_Para,
      Block_LineBlock,
      Block_CodeBlock,
      Block_RawBlock,
      Block_BlockQuote,
      Block_OrderedList,
      Block_BulletList,
      Block_DefinitionList,
      Block_Header,
      Block_HorizontalRule,
      Block_Table,
      Block_Figure,
      Block_Div,
      Inline_String,
      Inline_Emph,
      Inline_Underline,
      Inline_Strong,
      Inline_Strikeout,
      Inline_Superscript,
      Inline_Subscript,
      Inline_SmallCaps,
      Inline_Quoted,
      Inline_Cite,
      Inline_Code,
      Inline_Space,
      Inline_SoftBreak,
      Inline_LineBreak,
      Inline_Math,
      Inline_RawInline,
      Inline_Link,
      Inline_Image,
      Inline_Note,
      Inline_Span);

   type Content_Arr is array (Natural range <>)
     of League.JSON.Values.JSON_Value;

   function Get_Type (B : League.JSON.Objects.JSON_Object) return Object_Type;

   function "+" (T : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   function Attr
     (Key   : League.Strings.Universal_String;
      Value : League.Strings.Universal_String;
      Id    : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
          return League.JSON.Values.JSON_Value;

   function Attr
     (Id    : League.Strings.Universal_String;
      Key   : String_Array;
      Value : String_Array) return League.JSON.Values.JSON_Value;

   function Div
     (Attr : League.JSON.Values.JSON_Value;
      Content : Content_Arr) return League.JSON.Values.JSON_Value;

   function Div
     (Attr : League.JSON.Values.JSON_Value;
      Content : League.JSON.Values.JSON_Value)
        return League.JSON.Values.JSON_Value;

   Type_String : constant League.Strings.Universal_String := +"t";
   Content_String : constant League.Strings.Universal_String := +"c";

private

   Obj_String_Representation :
     constant array (Object_Type) of League.Strings.Universal_String := (
      +"Plain",
      +"Para",
      +"LineBlock",
      +"CodeBlock",
      +"RawBlock",
      +"BlockQuote",
      +"OrderedList",
      +"BulletList",
      +"DefinitionList",
      +"Header",
      +"HorizontalRule",
      +"Table",
      +"Figure",
      +"Div",
      +"Str",
      +"Emph",
      +"Underline",
      +"Strong",
      +"Strikeout",
      +"Superscript",
      +"Subscript",
      +"SmallCaps",
      +"Quoted",
      +"Cite",
      +"Code",
      +"Space",
      +"SoftBreak",
      +"LineBreak",
      +"Math",
      +"RawInline",
      +"Link",
      +"Image",
      +"Note",
      +"Span"
   );

   package Type_Map is new Ada.Containers.Hashed_Maps (
     Key_Type => League.Strings.Universal_String,
     Element_Type => Object_Type,
     Hash => League.Strings.Hash,
     Equivalent_Keys => League.Strings."=");

   Type_Mapping : Type_Map.Map := Type_Map.Empty;

end Pandoc;
