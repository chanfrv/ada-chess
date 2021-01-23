with GNAT.Sockets; use GNAT.Sockets;
with Board.Strings.Pretty; use Board.Strings.Pretty;


package Client is

    procedure Launch(Addr        : Inet_Addr_Type;
                     Port        : Port_Type;
                     Board_Color : Color_ANSI_t := Black);

end Client;
