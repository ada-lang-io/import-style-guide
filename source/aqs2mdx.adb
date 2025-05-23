with Ada.Wide_Wide_Text_IO;

with League.Holders;
with League.JSON.Documents;
with League.JSON.Arrays;
with League.JSON.Objects;
with League.JSON.Values;
with League.Strings;

with Pandoc;

procedure Aqs2mdx is
   use type League.Strings.Universal_String;
   use type League.Holders.Universal_Integer;
   use all type Pandoc.Object_Type;

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
      Pandoc_Type : constant Pandoc.Object_Type := Pandoc.Get_Type (Block);
   begin
      case Pandoc_Type is
         when Block_Table =>
            --  Flatten tables since there are no native multiline tables
            --  in .md
            declare
               --  Table structure in pandoc-types-1.23.1. See
               --  https://hackage.haskell.org/package/pandoc-types-1.23.1/docs/
               --  Text-Pandoc-Definition.html
               --
               --  Table:
               --  Attr Caption [ColSpec] TableHead [TableBody] TableFoot
               --  1    2       3         4         5           6

               Content : constant League.JSON.Arrays.JSON_Array :=
                 Block (Pandoc.Content_String).To_Array;

               Table_Body_List : constant League.JSON.Arrays.JSON_Array :=
                 Content (5).To_Array;

               --  A body of a table, with an intermediate head, intermediate
               --  body, and the specified number of row header columns in the
               --  intermediate body.
               --
               --  TableBody Attr RowHeadColumns [Row] [Row]
               --            1    2              3     4
               Table_Body : constant League.JSON.Arrays.JSON_Array :=
                 Table_Body_List (1).To_Array;

               Row_List : constant League.JSON.Arrays.JSON_Array :=
                 Table_Body (4).To_Array;

               --  A table row.
               --  Row Attr [Cell]
               --      1    2
               Row : constant League.JSON.Arrays.JSON_Array :=
                 Row_List (1).To_Array;

               Cell_List : constant League.JSON.Arrays.JSON_Array :=
                 Row (2).To_Array;

            begin
               pragma Assert (Content.Length = 6);
               pragma Assert (Table_Body_List.Length = 1);
               pragma Assert (Row_List.Length = 1);
               pragma Assert (Cell_List.Length <= 3);

               for J in 1 .. Cell_List.Length loop
                  declare
                     --  A table cell.
                     --  Cell Attr Alignment RowSpan ColSpan [Block]
                     --       1    2         3       4       5
                     Cell : constant League.JSON.Arrays.JSON_Array :=
                       Cell_List (J).To_Array;

                     Block_List : constant League.JSON.Arrays.JSON_Array :=
                       Cell (5).To_Array;
                  begin
                     pragma Assert (Cell.Length = 5);

                     for K in 1 .. Block_List.Length loop
                        List.Append (Block_List (K));
                     end loop;
                  end;
               end loop;
            end;
         when Block_Header =>
            if Block (Pandoc.Content_String).To_Array.Element (1)
                .To_Integer = 2
              and then Block (Pandoc.Content_String).To_Array.Element (2)
                .To_Array.Element (1).To_String = +"introduction"
              --  This relies on the fact that Pandoc converts a title
              --  From mediawiki and adds a lower-case id to the header
            then
               --  Drop toplevel 'Introduction' section header
               null;
            else
               List.Append (Block.To_JSON_Value);
            end if;
         when Inline_Link =>
            List.Append (Traverse_Link (Block));
         when others =>
            if Block (Pandoc.Content_String).To_Array.Length > 0 then
               declare
                  --  Traverse nested blocks
                  Copy : League.JSON.Objects.JSON_Object := Block;
                  Arr : constant League.JSON.Arrays.JSON_Array :=
                    Block (Pandoc.Content_String).To_Array;
               begin
                  Copy.Insert (
                     Pandoc.Content_String,
                     Traverse_List (Arr).To_JSON_Value);
                  List.Append (Copy.To_JSON_Value);
               end;
            else
               List.Append (Block.To_JSON_Value);
            end if;
      end case;

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
