with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;


package body Logs is
    
    
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
    
    procedure Set_Level(Level : Level_t) is
    begin
        Logs.Level := Level;
    end Set_Level;
    
    
    procedure Log(Level : Level_t; Item : String)
    is
        Clock_Colored : String := Formatting.Image(Clock);
        Level_Colored : String := Level_Image(Level_t'Pos(Level) + 1);
    begin
        Put_Line("[" & Clock_Colored & "][" & Level_Colored & "] " & Item);
    end Log;
    
    
end Logs;