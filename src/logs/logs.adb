with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;


package body Logs is
    
    
    procedure Set_Level(Level : Level_t) is
    begin
        Logs.Info("Set logging level to " & To_Lower(Level'Image));
        Logs.Level := Level;
    end Set_Level;
    
    
    procedure Error(Item : String) is
    begin
        if Level >= Error then
            Log(Error, Item);
        end if;
    end Error;
    
    procedure Info(Item : String) is
    begin
        if Level >= Info then
            Log(Info, Item);
        end if;
    end Info;
    
    procedure Debug(Item : String) is
    begin
        if Level >= Debug then
            Log(Debug, Item);
        end if;
    end Debug;
    
    
    procedure Inc_Indent is
    begin
        Incr := Incr + 1;
    end Inc_Indent;
    
    procedure Dec_Indent is
    begin
        if Incr > 0 then Incr := Incr - 1; end if;
    end Dec_Indent;
            
    
    procedure Log(Level : Level_t; Item : String)
    is
        Clock_Format  : String := Formatting.Image(Clock);
        Level_Format  : String := Level_Image(Level);
        Indent_Format : String := (if Incr > 0 then (Incr * 2) * ':' & " " else "");
    begin                
        Put_Line("[" & Clock_Format & "][" & Level_Format & "] " & Indent_Format & Item);
    end Log;
    
    
end Logs;
