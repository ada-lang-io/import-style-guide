pragma Ada_2022;

with League.JSON.Arrays;
with League.String_Vectors;

package body Pandoc is

   function "+" (Left : Object_Type) return League.JSON.Values.JSON_Value is
     (League.JSON.Values.To_JSON_Value
        (Obj_String_Representation (Left)));

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
      Block.Insert (Type_String, +Block_Div);

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
      Block.Insert (Type_String, +Block_Div);

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

   function Link
     (Text, URL : League.Strings.Universal_String)
      return League.JSON.Values.JSON_Value
   is
      Words  : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (' ');
      Block : League.JSON.Objects.JSON_Object;
      Content : League.JSON.Arrays.JSON_Array;
      Nil     : League.JSON.Arrays.JSON_Array;
      List_1  : League.JSON.Arrays.JSON_Array;
      List_2  : League.JSON.Arrays.JSON_Array;
   begin
      Block.Insert (Type_String, +Inline_Link);

      Nil.Append
        (League.JSON.Values.To_JSON_Value
          (League.Strings.Empty_Universal_String));
      Nil.Append (League.JSON.Arrays.Empty_JSON_Array.To_JSON_Value);
      Nil.Append (League.JSON.Arrays.Empty_JSON_Array.To_JSON_Value);

      for J in 1 .. Words.Length loop
         declare
            Object : League.JSON.Objects.JSON_Object;
            Space  : League.JSON.Objects.JSON_Object;
         begin
            Object.Insert (Type_String, +Inline_String);
            Object.Insert
              (Content_String,
               League.JSON.Values.To_JSON_Value (Words (J)));
            List_1.Append (Object.To_JSON_Value);

            if J /= Words.Length then
               Space.Insert (Type_String, +Inline_Space);
               List_1.Append (Space.To_JSON_Value);
            end if;
         end;
      end loop;

      List_2.Append (League.JSON.Values.To_JSON_Value (URL));
      List_2.Append
        (League.JSON.Values.To_JSON_Value
          (League.Strings.Empty_Universal_String));

      Content.Append (Nil.To_JSON_Value);
      Content.Append (List_1.To_JSON_Value);
      Content.Append (List_2.To_JSON_Value);
      Block.Insert (Content_String, Content.To_JSON_Value);

      return Block.To_JSON_Value;
   end Link;

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
