with GNAT.Sockets; use GNAT.Sockets;
with Gnat.Regexp; use GNAT.Regexp;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Board; use Board;


procedure Client is
    -- Network information
    Address     : Sock_Addr_Type;
    Socket      : Socket_Type;
    Channel     : Stream_Access;

    -- Game information
    GameState   : GameResult_t;
    Str         : String(1 .. 80);
    Last        : Natural;
    My_Color    : Color_t;

    -- TODO put the complete algebraic notation regex
    Move_String : constant String := "[KQRBN]?([a-h]|[1-8]|[a-h][1-8])?[x]?[a-h][1-8]";
    Move_Regexp : constant Regexp := Compile(Move_String);
begin
    -- Connect
    Address.Addr := Addresses(Get_Host_By_Name(Host_Name), 1);
    Address.Port := 5876;

    if Argument_Count > 0 then
        Address.Port := Port_Type'Value(Argument(1));
    end if;

    Create_Socket(Socket);
    Set_Socket_Option(Socket, Socket_Level, (Reuse_Address, True));
    Connect_Socket(Socket, Address);
    Channel := Stream(Socket);

    Put_Line("Connecting to " & Image(Address));

    -- Play
    My_Color := Color_t'Value(String'Input(Channel));
    GameState := Playing;

Game_Loop:
    while GameState = Playing or GameState = Check loop
        -- Get and send move
        Str := (others => Character'Val(0));
        Put(To_lower(My_Color'Image) & "> ");
        Get_Line(Str, Last);

        -- Exit on "quit" command
        exit Game_Loop when Str(1 .. Last) = "quit";

        -- Send the move to the server
        if Str(1 .. Last) = "quit" then
            exit Game_Loop;
        elsif Str(1 .. Last) = "forfeit" then
            null;
        elsif Match(Str(1 .. Last), Move_Regexp) = True then
            String'Output(Channel, Str(1 .. Last));
        else
            Put_Line("Invalid command: " & Str(1 .. Last) &
                     ", a move must follow the pattern '" & Move_String & "'");
        end if;

        -- Receive response (success, error, you win...)
        Put_Line(String'Input(Channel));
        --GameState := GameResult_t'Value();
    end loop Game_Loop;

    -- Disconnect
    Close_Socket(Socket);
end Client;
