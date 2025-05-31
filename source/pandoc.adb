with League.JSON.Arrays;

package body Pandoc is

   function Attr (
      Id : Ustr.Universal_String;
      Key : Ustr_Array;
      Value : Ustr_Array) return League.JSON.Values.JSON_Value
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

   function Div (
      Attr : League.JSON.Values.JSON_Value;
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
     return Object_Type is
   begin
      return Type_Map.Element (
        Type_Mapping.Find (
          B (Type_String).To_String
        )
      );
   end Get_Type;

   function Hash (Item : Ustr.Universal_String)
     return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (League.Hash_Type'(Item.Hash));
   end Hash;

begin

   for Key in Object_Type loop
      declare
         Str_Rep : constant Ustr.Universal_String :=
           Obj_String_Representation (Key);
      begin
         Type_Mapping.Insert (Str_Rep, Key);
      end;
   end loop;

end Pandoc;
