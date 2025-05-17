with League.JSON.Values;

package body Pandoc is

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
