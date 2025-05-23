with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with League.JSON.Objects;
with League.Strings;

package Pandoc is

   Pandoc_Type_Error : exception;

   package Ustr renames League.Strings;

   type Ustr_Array is array (Natural range <>) of Ustr.Universal_String;

   type Object_Type is (
     Block_Plain,
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
     Inline_Span
    );

   function Get_Type (
     B : League.JSON.Objects.JSON_Object) return Object_Type;

   function "+" (T : Wide_Wide_String) return Ustr.Universal_String
     renames Ustr.To_Universal_String;

   Type_String : constant Ustr.Universal_String := +"t";
   Content_String : constant Ustr.Universal_String := +"c";

private

   function Hash (Item : Ustr.Universal_String)
     return Ada.Containers.Hash_Type;

   Obj_String_Representation :
     constant array (Object_Type) of Ustr.Universal_String := (
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
     Key_Type => Ustr.Universal_String,
     Element_Type => Object_Type,
     Hash => Hash,
     Equivalent_Keys => Ustr."=");

   Type_Mapping : Type_Map.Map := Type_Map.Empty;

end Pandoc;
