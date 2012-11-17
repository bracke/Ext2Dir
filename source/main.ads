-- StrongEd$WrapWidth=256
-- StrongEd$Mode=Ada
--

with RASCAL.Utility; use RASCAL.Utility;

package Main is

   type Names is array(natural range <>) of ustring;
   
   type Options is
      (Remove_Extension,Change_Name,Set_FileType,
       Set_Access,Remove_Original,Verbose,Help,Descend,Move_Sub_Content);
   
 
   type Bit_Array is
      array (Options'Range) of Boolean;
   
   type Path_Variables is
         (source,target);
   
   type String_Array is
      array (Path_Variables'Range) of ustring;
   
   Bits  : Bit_Array    := (others => false);
   Paths : String_Array := (U(""),U(""));
  
  --

   procedure Main;

   --

   procedure Proces_File(FilePath : in string;
                         Target   : in String);
   
   --

   procedure Proces_Dir (Path   : in String;
                         Target : in String);

   --

   procedure Help;
   
   ----
         
end Main;