with Ada.Wide_Wide_Text_IO;

with League.Holders;
with League.JSON.Documents;
with League.JSON.Arrays;
with League.JSON.Objects;
with League.JSON.Values;
with League.Strings;

procedure Aqs2mdx is
   use type League.Strings.Universal_String;
   use type League.Holders.Universal_Integer;

   function "+" (T : Wide_Wide_String) return League.Strings.Universal_String
      renames League.Strings.To_Universal_String;

   procedure Read_JSON
     (Doc : out League.JSON.Documents.JSON_Document);

   function Traverse (Blocks : League.JSON.Arrays.JSON_Array)
     return League.JSON.Arrays.JSON_Array;

   function Traverse_List (List : League.JSON.Arrays.JSON_Array)
     return League.JSON.Arrays.JSON_Array;

   function Traverse_Block (Block : League.JSON.Objects.JSON_Object)
     return League.JSON.Arrays.JSON_Array;

   function Traverse_Link (Block : League.JSON.Objects.JSON_Object)
     return League.JSON.Values.JSON_Value;

   Wiki : constant League.Strings.Universal_String :=
     +"https://en.wikipedia.org/wiki/";

   Wikibook : constant League.Strings.Universal_String :=
     +"https://en.wikibooks.org/wiki/";

   ---------------
   -- Read_JSON --
   ---------------

   procedure Read_JSON
     (Doc : out League.JSON.Documents.JSON_Document)
   is
      Text : League.Strings.Universal_String;
   begin
      while not Ada.Wide_Wide_Text_IO.End_Of_File loop
         declare
            Line : constant Wide_Wide_String := Ada.Wide_Wide_Text_IO.Get_Line;
         begin
            if not Text.Is_Empty then
               Text.Append (Wide_Wide_Character'Val (10));
            end if;

            Text.Append (Line);
         end;
      end loop;

      Doc := League.JSON.Documents.From_JSON (Text);
   end Read_JSON;

   --------------
   -- Traverse --
   --------------

   function Traverse (Blocks : League.JSON.Arrays.JSON_Array)
     return League.JSON.Arrays.JSON_Array
   is
      List : League.JSON.Arrays.JSON_Array;
   begin
      for J in 1 .. Blocks.Length loop
         declare
            Block : constant League.JSON.Objects.JSON_Object :=
               Blocks (J).To_Object;

            Result : constant League.JSON.Arrays.JSON_Array :=
               Traverse_Block (Block);
         begin
            for K in 1 .. Result.Length loop
               List.Append (Result (K));
            end loop;
         end;
      end loop;

      return List;
   end Traverse;

   --------------------
   -- Traverse_Block --
   --------------------

   function Traverse_Block (Block : League.JSON.Objects.JSON_Object)
     return League.JSON.Arrays.JSON_Array
   is
      List : League.JSON.Arrays.JSON_Array;
   begin
      if Block (+"t").To_String.To_Wide_Wide_String = "Table" then
         --  Flatting tables because no multiline tables in .md

         declare
            Content : constant League.JSON.Arrays.JSON_Array :=
              Block (+"c").To_Array;

            Rows    : constant League.JSON.Arrays.JSON_Array :=
              Content (5).To_Array;

            Columns : constant League.JSON.Arrays.JSON_Array :=
              Rows (1).To_Array;
         begin
            pragma Assert (Content.Length = 5);
            pragma Assert (Rows.Length = 1);
            pragma Assert (Columns.Length <= 3);

            for J in 1 .. Columns.Length loop
               declare
                  Item : constant League.JSON.Arrays.JSON_Array :=
                     Columns (J).To_Array;
               begin
                  pragma Assert (Item.Length <= 1);

                  for K in 1 .. Item.Length loop
                     List.Append (Item (K));
                  end loop;
               end;
            end loop;
         end;

      elsif Block (+"t").To_String.To_Wide_Wide_String = "Header"
        and then Block (+"c").To_Array.Element (1).To_Integer = 2
        and then Block (+"c").To_Array.Element (2)
          .To_Array.Element (1).To_String = +"introduction"
      then
         --  Drop toppest 'Introduction' section header
         null;

      elsif Block (+"t").To_String.To_Wide_Wide_String = "Link" then
         List.Append (Traverse_Link (Block));

      elsif Block (+"c").To_Array.Length > 0 then
         declare
            --  Traverse nested blocks
            Copy : League.JSON.Objects.JSON_Object := Block;
         begin
            Copy.Insert
              (+"c", Traverse_List (Block (+"c").To_Array).To_JSON_Value);

            List.Append (Copy.To_JSON_Value);
         end;

      else  --  Something else (if any?)
         List.Append (Block.To_JSON_Value);
      end if;

      return List;
   end Traverse_Block;

   -------------------
   -- Traverse_Link --
   -------------------

   function Traverse_Link (Block : League.JSON.Objects.JSON_Object)
     return League.JSON.Values.JSON_Value
   is
      Copy : League.JSON.Objects.JSON_Object := Block;
      Args : League.JSON.Arrays.JSON_Array := Copy (+"c").To_Array;
      Fix  : League.JSON.Arrays.JSON_Array := Args (3).To_Array;
      Link : League.Strings.Universal_String := Fix (1).To_String;
   begin
      if Fix (2).To_String.To_Wide_Wide_String = "wikilink" then
         if Link.Starts_With ("w:") then
            Link := Wiki & Link.Tail_From (3);
         else
            Link := Wikibook & Link;
         end if;

         Fix.Replace (1, League.JSON.Values.To_JSON_Value (Link));
         Fix.Replace (2, League.JSON.Values.To_JSON_Value (+""));
         Args.Replace (3, Fix.To_JSON_Value);
         Copy.Insert (+"c", Args.To_JSON_Value);
      end if;

      return Copy.To_JSON_Value;
   end Traverse_Link;

   -------------------
   -- Traverse_List --
   -------------------

   function Traverse_List (List : League.JSON.Arrays.JSON_Array)
     return League.JSON.Arrays.JSON_Array
   is
      Result : League.JSON.Arrays.JSON_Array;
   begin
      for J in 1 .. List.Length loop
         declare
            Item : constant League.JSON.Values.JSON_Value := List (J);
         begin
            if Item.Is_Object then
               declare
                  Block : constant League.JSON.Objects.JSON_Object :=
                    Item.To_Object;

                  Blocks : constant League.JSON.Arrays.JSON_Array :=
                    Traverse_Block (Block);
               begin
                  for K in 1 .. Blocks.Length loop
                     Result.Append (Blocks (K));
                  end loop;
               end;

            elsif Item.Is_Array then
               Result.Append (Traverse_List (Item.To_Array).To_JSON_Value);

            else
               Result.Append (Item);

            end if;
         end;
      end loop;

      return Result;
   end Traverse_List;

   Doc  : League.JSON.Documents.JSON_Document;

begin
   Read_JSON (Doc);

   declare
      Object : League.JSON.Objects.JSON_Object := Doc.To_JSON_Object;
      Blocks : League.JSON.Values.JSON_Value := Object (+"blocks");
   begin
      Blocks := Traverse (Blocks.To_Array).To_JSON_Value;
      Object.Insert (+"blocks", Blocks);
      Doc := Object.To_JSON_Document;
   end;

   Ada.Wide_Wide_Text_IO.Put_Line (Doc.To_JSON.To_Wide_Wide_String);
end Aqs2mdx;
