with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Board; use Board;
with Board.Strings; use Board.Strings;
with Board.Strings.Parse; use Board.Strings.Parse;
with Board.Strings.Pretty; use Board.Strings.Pretty;
with Logs;


package body Server is
    
    
    -- Image of a player
    function Image(Player : Player_t) return String is
    begin
        return "Player "
          & Trim(Player.Id'Image, Ada.Strings.Left)
          & " (" & To_Lower(Player.Color'Image) & ")";
    end Image;
    
    -- Wait for a player, accept him, assign him a color
    procedure AcceptPlayer(Id      : in Player_Index_t;
                           Color   : in Color_t;
                           Server  : in out Socket_Type;
                           Address : in out Sock_Addr_Type;
                           Player  : out Player_t)
    is
        -- Local network info
        Socket  : Socket_Type;
        Channel : Stream_Access;
    begin
        Accept_Socket(Server, Socket, Address);
        Channel := Stream(Socket);
        -- TODO Authenticate, assign color...
        Logs.Info("Assigned color " & Color'Image);
        String'Output(Channel, Color'Image);
        Player := (Id => Id, Color => Color, Socket => Socket, Channel => Channel);
    end AcceptPlayer;

    -- Get the white player from the player record to set him
    -- as the first player to move.
    function GetWhitePlayer(Players : in Players_t) return Natural is
        WhiteIndex : Natural := 0;
    begin
        White_Loop:
        for Index in Players'First .. Players'Last loop
            if Players(Index).Color = White then
                WhiteIndex := Index;
                exit White_Loop;
            end if;
        end loop White_Loop;
        
        return WhiteIndex;
    end GetWhitePlayer;

    
    
    procedure Launch(Port : Port_Type; Board_Color : Color_ANSI_t := Black)
    is
        -- Server network information
        Address         : Sock_Addr_Type;
        Server          : Socket_Type;
        Players         : Players_t;

        -- Game information
        GameState       : GameResult_t;
        Board           : Board_t;
        CurrPlayerIndex : Player_Index_t;
        MoveResult      : MoveResult_t;
        CurrPlayer      : Player_t;
        CurrMove        : Move_t;
    begin        
        -- Connect
        Address.Addr := Addresses(Get_Host_By_Name(Host_Name), 1);
        Address.Port := Port;
    
        Create_Socket(Server);
        Set_Socket_Option(Server, Socket_Level, (Reuse_Address, True));
        Bind_Socket(Server, Address);
        Listen_Socket(Server);

        Logs.Info("Listening on " & Image(Address));

        -- Accept players
        for Index in Player_Index_t'First .. Player_Index_t'Last loop
            Logs.Info("Waiting for player " & Trim(Index'Image, Ada.Strings.Left));
            AcceptPlayer(Index, Color_t'Val(Index - 1), Server, Address, Players(Index));
        end loop;
        
        -- Play
        Board := StartBoard;
        GameState := Playing;
        CurrPlayerIndex := GetWhitePlayer(Players);
        
        Game_Loop:
        while GameState in Playing | Check loop
            -- Get the current player info
            CurrPlayer := Players(CurrPlayerIndex);
            
            -- Send the board
            Board_t'Output(CurrPlayer.Channel, Board);
            
            -- pretty print
            Pretty_Print(Board, CurrPlayer.Color, Board_Color);
            
            -- Receive the move from the current player
            if Traverse(String'Input(CurrPlayer.Channel), CurrMove) = False then
                Logs.Error("Invalid move from " & Image(CurrPlayer));
                String'Output(CurrPlayer.Channel, "Invalid move from " & Image(CurrPlayer));
                
            else
                -- Valid move string
                Logs.Info("Move string parsed as '" & Image(CurrMove) & "'");
                
                -- Play the move
                MoveResult := Move(Board, CurrMove, CurrPlayer.Color);
                
                -- Decide what to do depending on the move
                case MoveResult is
                    when Valid_Move =>
                        Logs.Info(Image(CurrPlayer) & " moved");
                        String'Output(CurrPlayer.Channel, Image(CurrPlayer) & " moved");
                        -- Check if the game ended
                        GameState := Game_Ended(Board, CurrPlayer.Color);
                        Logs.Info("Game state: " & GameState'Image);
                        -- change current player
                        CurrPlayerIndex := 3 - CurrPlayerIndex;
                    when Invalid_Move | Ambiguous_Move =>
                        Logs.Error("Invalid move from " & Image(CurrPlayer));
                        String'Output(CurrPlayer.Channel, "Invalid move from " & Image(CurrPlayer));
                end case;
            end if;
            
        end loop Game_Loop;
        
        -- Close
        Close_Socket(Server);
        Close_Socket(Players(1).Socket);
        Close_Socket(Players(2).Socket);
    
    exception
        when Socket_Error | End_Error =>
            Logs.Error("Lost connection");
            -- Close
            Close_Socket(Server);
            Close_Socket(Players(1).Socket);
            Close_Socket(Players(2).Socket);
            
    end Launch;
    
    
end Server;
