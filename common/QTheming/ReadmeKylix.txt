For Kylix users:
  Copy the "Themes" directory to the directory where the executable file is or
  write the following lines in an initialization section:

 ---------------------------------------------
{$IFDEF LINUX}
 initialization
   ThemeServices.ThemesDir := '/Here/is/the/parent/directory/of/Themes';
   if ThemeServices.ThemesEnabled and not Assigned(ThemedStyle) then
     ThemedStyle := TThemedStyle.Create;
{$ENDIF LINUX}
 end.
 ---------------------------------------------