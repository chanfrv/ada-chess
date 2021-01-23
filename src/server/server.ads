with GNAT.Sockets; use GNAT.Sockets;

with Board; use Board;
with Board.Strings.Pretty; use Board.Strings.Pretty;


package Server is

    -- Index type in the player array
    subtype Player_Index_t is Natural range 1 .. 2;
    -- Record associating a player color and its network identity
    type Player_t is
        record
            Id      : Player_Index_t;
            Color   : Color_t;
            Socket  : Socket_Type;
            Channel : Stream_Access;
        end record;
    -- Player array containing 2 player records
    type Players_t is array (Player_Index_t) of Player_t;


    procedure Launch(Port : Port_Type; Board_Color : Color_ANSI_t := Black);

end Server;
