package Board.Strings.Pretty is

    type Color_ANSI_t is (Black, Red, Green, Yellow, Blue, Purple, Cyan, White);
    
    
    Palette_Reset : constant String(1 .. 4) := ASCII.ESC & "[0m";
    
    Palette_Bright_Fg : array (Color_ANSI_t) of String(1 .. 7) :=
    (
        ASCII.ESC & "[1;30m",
        ASCII.ESC & "[1;31m",
        ASCII.ESC & "[1;32m",
        ASCII.ESC & "[1;33m",
        ASCII.ESC & "[1;34m",
        ASCII.ESC & "[1;35m",
        ASCII.ESC & "[1;36m",
        ASCII.ESC & "[1;37m"
    );
    
    Palette_Bright_Bg : constant array (Color_ANSI_t) of String(1 .. 7) :=
    (
        ASCII.ESC & "[1;40m",
        ASCII.ESC & "[1;41m",
        ASCII.ESC & "[1;42m",
        ASCII.ESC & "[1;43m",
        ASCII.ESC & "[1;44m",
        ASCII.ESC & "[1;45m",
        ASCII.ESC & "[1;46m",
        ASCII.ESC & "[1;47m"
    );
    
    Palette_Dull_Fg : constant array (Color_ANSI_t) of String(1 .. 5) :=
    (
        ASCII.ESC & "[90m",
        ASCII.ESC & "[91m",
        ASCII.ESC & "[92m",
        ASCII.ESC & "[93m",
        ASCII.ESC & "[94m",
        ASCII.ESC & "[95m",
        ASCII.ESC & "[96m",
        ASCII.ESC & "[97m"
    );
    
    Palette_Dull_Bg : constant array (Color_ANSI_t) of String(1 .. 6) :=
    (
        ASCII.ESC & "[100m",
        ASCII.ESC & "[101m",
        ASCII.ESC & "[102m",
        ASCII.ESC & "[103m",
        ASCII.ESC & "[104m",
        ASCII.ESC & "[105m",
        ASCII.ESC & "[106m",
        ASCII.ESC & "[107m"
    );
    
    
    -- Pretty prints the board on the standard output.
    procedure Pretty_Print(Board : in Board_t; Background : in Color_ANSI_t := Black);
    procedure Pretty_Print(Board : in Board_t; CurrPlayer : in Color_t; Background : in Color_ANSI_t := Black);
    

end Board.Strings.Pretty;
