with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Board; use Board;
with Board.Strings; use Board.Strings;
with Board.Strings.Pretty; use Board.Strings.Pretty;
with Logs;


package body Client is
    
    
    procedure Launch(Addr        : Inet_Addr_Type;
                     Port        : Port_Type;
                     Board_Color : Color_ANSI_t := Black)
    is
        -- Network information
        Address     : Sock_Addr_Type;
        Socket      : Socket_Type;
        Channel     : Stream_Access;

        -- Game information
        GameState   : GameResult_t;
        Str         : String(1 .. 80);
        Last        : Natural;
        My_Color    : Color_t;
    
    begin
        -- Connect
        Address.Addr := Addr;
        Address.Port := Port;

        Create_Socket(Socket);
        Set_Socket_Option(Socket, Socket_Level, (Reuse_Address, True));
        Connect_Socket(Socket, Address);
        Channel := Stream(Socket);

        Logs.Info("Connecting to " & Image(Address));

        -- Play
        My_Color := Color_t'Value(String'Input(Channel));
        GameState := Playing;

        Game_Loop:
        while GameState in Playing | Check loop
            -- Receive the board
            Pretty_Print(Board => Board_t'Input(Channel), Background => Board_Color);
            
            -- Get and send move
            Str := (others => Character'Val(0));
            Put(To_lower(My_Color'Image) & "> ");
            Get_Line(Str, Last);

            -- Send the move to the server
            if Str(1 .. Last) = "resign" then
                exit Game_Loop;

            else
                String'Output(Channel, Str(1 .. Last));

                -- Receive response (success, error, you win...)
                Logs.Info(String'Input(Channel));
            end if;

        end loop Game_Loop;

        -- Disconnect
        Close_Socket(Socket);
        
    exception
        when Socket_Error =>
            Put_Line("Server disconnected");
            Close_Socket(Socket);
    end Launch;

    
end Client;
