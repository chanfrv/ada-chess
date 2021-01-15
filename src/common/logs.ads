package Logs is    
    
    type Level_t is (Error, Info, Debug);
    
    
    procedure Set_Level(Level : Level_t);
    
    procedure Error(Item : String);
    procedure Info(Item : String);
    procedure Debug(Item : String);
    
private
    
    Level : Level_t := Error;
    
    Level_Image : constant array (1 .. 3) of String(1 .. 14) :=
    (
        ASCII.ESC & "[31mERROR" & ASCII.ESC & "[0m",
        ASCII.ESC & "[00mINFO " & ASCII.ESC & "[0m",
        ASCII.ESC & "[33mDEBUG" & ASCII.ESC & "[0m"
    );
    
    
    procedure Log(Level : Level_t; Item : String);    
    
end Logs;