pragma Ada_2022;

with League.JSON.Arrays;

package body Pandoc is

   function Attr
     (Key   : League.Strings.Universal_String;
      Value : League.Strings.Universal_String;
      Id    : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
          return League.JSON.Values.JSON_Value is
           (Attr (Id, [Key], [Value]));

   function Attr
     (Id    : League.Strings.Universal_String;
      Key   : String_Array;
      Value : String_Array) return League.JSON.Values.JSON_Value
   is
      Outer : League.JSON.Arrays.JSON_Array;
      Other : League.JSON.Arrays.JSON_Array;
      Inner : League.JSON.Arrays.JSON_Array;
   begin
      Outer.Append (League.JSON.Values.To_JSON_Value (Id));
      Outer.Append (Other.To_JSON_Value);

      for K in Key'Range loop
         declare
            Pair : League.JSON.Arrays.JSON_Array;
         begin
            Pair.Append (League.JSON.Values.To_JSON_Value (Key (K)));
            Pair.Append (League.JSON.Values.To_JSON_Value (Value (K)));
            Inner.Append (Pair.To_JSON_Value);
         end;
      end loop;

      Outer.Append (Inner.To_JSON_Value);

      return Outer.To_JSON_Value;
   end Attr;

   function Div (
      Attr : League.JSON.Values.JSON_Value;
      Content : Content_Arr) return League.JSON.Values.JSON_Value
   is
      Block : League.JSON.Objects.JSON_Object;
      Out_Content : League.JSON.Arrays.JSON_Array;
      Content_Block : League.JSON.Arrays.JSON_Array;
   begin
      Block.Insert (
         Type_String,
         League.JSON.Values.To_JSON_Value (
            Obj_String_Representation (Block_Div)
         )
      );

      for C in Content'Range loop
         Content_Block.Append (Content (C));
      end loop;

      Out_Content.Append (Attr);
      Out_Content.Append (Content_Block.To_JSON_Value);

      Block.Insert (
         Content_String,
         Out_Content.To_JSON_Value
      );

      return Block.To_JSON_Value;
   end Div;

   function Div
     (Attr    : League.JSON.Values.JSON_Value;
      Content : League.JSON.Values.JSON_Value)
      return League.JSON.Values.JSON_Value
   is
      Block : League.JSON.Objects.JSON_Object;
      Out_Content : League.JSON.Arrays.JSON_Array;
   begin
      Block.Insert (
         Type_String,
         League.JSON.Values.To_JSON_Value (
            Obj_String_Representation (Block_Div)
         )
      );

      Out_Content.Append (Attr);
      Out_Content.Append (Content);

      Block.Insert (
         Content_String,
         Out_Content.To_JSON_Value
      );

      return Block.To_JSON_Value;
   end Div;

   function Get_Type (B : League.JSON.Objects.JSON_Object)
     return Object_Type is (Type_Mapping (B (Type_String).To_String));

begin

   for Key in Object_Type loop
      declare
         Str_Rep : constant League.Strings.Universal_String :=
           Obj_String_Representation (Key);
      begin
         Type_Mapping.Insert (Str_Rep, Key);
      end;
   end loop;

end Pandoc;
