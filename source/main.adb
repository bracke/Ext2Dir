-- StrongEd$WrapWidth=256
-- StrongEd$Mode=Ada
--

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;--         use Ada.Strings.Unbounded;  
with System.Unsigned_Types;         use System.Unsigned_Types;
With Ada.Strings;                   use Ada.Strings;
With Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with Reporter;

with RASCAL.FileExternal;           use RASCAL.FileExternal;
with RASCAL.FileName;               use RASCAL.FileName;

package body Main is

   --

   package FileExternal renames RASCAL.FileExternal;
   package Utility      renames RASCAL.Utility;
   
   --

   procedure Main is

      Object_Type         : File_Object_Type;
      arg_type            : Options;
      nr                  : integer :=0;
      nr_of_args          : natural;
      argument            : ustring;
      path                : ustring;
      single_option       : character := 'H';
      Source_Not_Found    : Exception;
      Target_Not_Found    : Exception;
      Source_Is_A_File    : Exception;
      Target_Is_A_File    : Exception;
      Too_Manny_Arguments : Exception;
      Print_Help          : Exception;
      Unknown_Parameter   : Exception;
   begin
      nr_of_args := Ada.Command_line.Argument_Count;
      if nr_of_args > (Options'Pos(Options'Last)+2) then
         raise Too_Manny_Arguments;
      end if;
      for x in 1..nr_of_args loop
         argument := U(Ada.Command_Line.Argument(x));
         if Ada.Strings.Unbounded.Element(argument,1) = '-' then
            for y in 2..Ada.Strings.Unbounded.Length(argument) loop
               single_option := Ada.Strings.Unbounded.Element(argument,y);
               case single_option is
               when 'E'|'e'     => arg_type := Remove_Extension;
               when 'N'|'n'     => arg_type := Change_Name;
               when 'T'|'t'     => arg_type := Set_Filetype;
               when 'A'|'a'     => arg_type := Set_access;
               when 'R'|'r'     => arg_type := Remove_Original;
               when 'V'|'v'     => arg_type := Verbose;
               when 'D'|'d'     => arg_type := Descend;
               when 'H'|'h'     => arg_type := Help;
               when 'S'|'s'     => arg_type := Move_Sub_Content;
               when others      => raise Unknown_Parameter;
               end case;
               bits(arg_type) := true;
            end loop;
         else
            if S(paths(Source)) = "" then
               paths(source) := argument;
            else
               paths(target) := argument;
            end if;
         end if;
      end loop;
      
      if bits(Help) then
         raise Print_Help;
      end if;

      if Ada.Strings.Unbounded.Length(paths(target)) > 0 then
         path := paths(target);
         
         Object_Type := FileExternal.Get_ObjectType(S(path));
         case Object_Type is
         when not_found    => raise Target_Not_Found;
         when file_object  => raise Target_Is_A_File;
         when dir|image    => null;
         end case;

         if Ada.Strings.Unbounded.Length(paths(source)) > 0 then
            path := paths(source);
            Object_Type := FileExternal.Get_ObjectType(S(path));
            case Object_Type is
            when not_found    => raise Source_Not_Found;
            when file_object  => raise Source_Is_A_File;
            when dir|image    => Proces_Dir(S(paths(Source)),S(paths(target)));
            end case;
         else
            Ada.Text_IO.Put_Line("Source path needed");
         end if;
      else
         Ada.Text_IO.Put_Line("Target path needed");
      end if;
   exception
      when Print_Help         => Help;
      when Unknown_Parameter  => Ada.Text_IO.Put_Line("Error: Unknown parameter");
      when Target_Not_Found   => Ada.Text_IO.Put_Line("Error: Target not found");
      when Target_Is_A_File   => Ada.Text_IO.Put_Line("Error: Target is a file, should be a directory");
      when Source_Not_Found   => Ada.Text_IO.Put_Line("Error: Source not found");
      when Source_Is_A_File   => Ada.Text_IO.Put_Line("Error: Source is a file, should be a directory");
      when others             => raise;
   end Main;
      
   --
      
   procedure Proces_File(FilePath : in String;
                         Target   : in String) is
      
      FileName    : string  := Get_Leaf(FilePath);
      path        : string  := Target & ".";
      ending      : string  := FileName;
      name        : string  := FileName;
      new_name    : ustring := U("");
      target_dir  : ustring := U(Trim(Target,both));
      new_path    : ustring := U("");
      source_path : ustring := U(StripTrailingSpaces(StripTrailingZeroes(FilePath)));
      i           : natural := 0;
      Object_Type : File_Object_Type;
         
      File_Has_No_Ending         : exception;
      Target_exist_but_is_a_file : exception;
   begin
      if Bits(Verbose) then
         Ada.Text_IO.Put_Line(filename);
      end if;

      i := Count(ending,"/");
      if i = 0 then
         raise File_Has_No_Ending;
      end if;

      i := index(ending,"/",backward);

      if i > ending'first then
         Delete(ending,ending'first,i);
      end if;

      i := index(name,"/",backward);

      if i > name'First then
         if Bits(Remove_Extension) then
            Delete(name,i,name'last);
         end if;
      end if;

      if ending /= name then

         if Ada.Strings.Unbounded.Length(target_dir) = 0 then
            target_dir := U(path & ending);
         else
            target_dir := U(S(target_dir) & "." & ending);
         end if;
         target_dir := U(StripTrailingSpaces(StripTrailingZeroes(S(target_dir))));

         -- Find or create directory
         Object_Type := FileExternal.Get_ObjectType(S(target_dir));
         case Object_Type is
         when not_found    => FileExternal.Create_Directory(S(target_dir));
         when file_object  => raise Target_exist_but_is_a_file;
         when dir|image    => null;
         end case;
   
         if Bits(Change_Name) then
            -- Change name to lowercase bdy
            -- and uppercase first character
            new_name := U(Ada.Characters.Handling.To_Lower(StripTrailingSpaces(name)));
            Ada.Strings.Unbounded.Replace_Element(new_Name,1,
                      Ada.Characters.Handling.To_Upper(Ada.Strings.Unbounded.Element(new_name,1)));
   
            new_path := U(S(target_dir) & "." & S(new_name));
         else
            new_path := U(S(target_dir) & "." & StripTrailingSpaces(name));
         end if;
      else
         new_path := U(S(target_dir) & "." & StripTrailingSpaces(name));
      end if;

      -- Copy file
      FileExternal.Set_Attributes(S(source_path),FileExternal.Get_Attributes(S(source_path)) or Attribute_Owner_Read);
      FileExternal.Copy(S(source_path),S(new_path));

      if Bits(Set_Access) then
         -- Set access attributes
         FileExternal.Set_Attributes(S(new_path),FileExternal.Get_Attributes(S(source_path)) or Attribute_Owner_Read+Attribute_Owner_Write);
      end if;
      
      if Bits(Set_FileType) then
         -- Set filetype
         FileExternal.Set_FileType(S(new_path),16#fff#);
      end if;
   exception
      when File_Has_No_Ending => null;
      when others             => raise;
   end Proces_File;
   
   --
   
   procedure Proces_Dir (Path   : in String;
                         Target : in String) is

      Object_Type : File_Object_Type;
      Dir_List    : Directory_Pointer := Get_DirectoryList(Path);
   begin
      for i in Dir_List'Range loop
         Object_Type := FileExternal.Get_ObjectType(Path & "." & S(Dir_List(i)));

         case Object_Type is
         when not_found    => null;
         when file_object  => if Ada.Strings.Unbounded.Count(Dir_List(i),"/") > 0 then
                                 Proces_File(Path & "." & S(Dir_List(i)),Target);
                                 if Bits(Remove_Original) then
                                    FileExternal.Delete_File(Path & "." & S(Dir_List(i)));
                                 end if;
                              end if;
         when dir|image    => if Bits(Descend) then
                                 if Bits(Move_Sub_Content) then
                                    Proces_Dir(Path & "." & S(Dir_List(i)),Target);
                                 else
                                    if not Exists(Target & "." & S(Dir_List(i))) then
                                       Create_Directory (Target & "." & S(Dir_List(i)));
                                    end if;
                                    Proces_Dir(Path & "." & S(Dir_List(i)),Target & "." & S(Dir_List(i)));
                                 end if;
                              end if;
         end case;
      end loop;
    end Proces_Dir;

   --

   procedure Help is
   begin
      Ada.Text_IO.Put_Line("Usage: FileSort [-[a][e][d][s][n][t][r]] source [target]");
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("  -a      Set access attributes of files in target to unprotected");
      Ada.Text_IO.Put_Line("  -e      Remove file extension");
      Ada.Text_IO.Put_Line("  -d      Descend - recursive descend through directory structure");
      Ada.Text_IO.Put_Line("  -s      Move subdirectory files up to start directory.");
      Ada.Text_IO.Put_Line("  -n      Change name");
      Ada.Text_IO.Put_Line("  -t      Set filetype to text (&fff)");
      Ada.Text_IO.Put_Line("  -r      Remove original");
      Ada.Text_IO.Put_Line("  -v      verbose mode, provides progress info");
      Ada.Text_IO.Put_Line("");
   end Help;

   --
   
end Main;


