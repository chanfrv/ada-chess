with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Board; use Board;


procedure Server is

	type Player_t is
		record
			Color   : Color_t;
			Socket  : Socket_Type;
			Channel : Stream_Access;
		end record;
	
	subtype Player_Index_t is Natural range 1 .. 2;
	type Players_t is array (Player_Index_t) of Player_t;
		
	procedure AcceptPlayer(Color : in Color_t;
						Server  : in out Socket_Type;
						Address : in out Sock_Addr_Type;
						Player  : out Player_t) is
		Socket      : Socket_Type;
		Channel     : Stream_Access;
	begin
		Accept_Socket(Server, Socket, Address);
		Channel := Stream(Socket);
		-- TODO Authenticate, assign color...
		Put_Line("Assigned color " & Color'Image);
		String'Output(Channel, Color'Image);
		Player := (Color => Color, Socket => Socket, Channel => Channel);
	end AcceptPlayer;
		
	function GetWhitePlayer(Players : in Players_t) return Player_Index_t is
	begin
		-- TODO
		return 1;
	end GetWhitePlayer;
		
	function GetMove(Player : in Player_t ; Input : in String) return Move_t is
	begin
		Put_Line("[" & Player.Color'Image & "] Moved '" & Input & "'");
		-- TODO
		return ((A, 1), (A, 2));
	end GetMove;

	Address    : Sock_Addr_Type;
	Server     : Socket_Type;
	Players    : Players_t;
	
	Finished   : Boolean;
	Board      : Board_t;
	CurrPlayerIndex : Player_Index_t;
	CurrMove   : Move_t;
	MoveResult : MoveResult_t;
	CurrPlayer : Player_t;
begin
	-- Connect
	Address.Addr := Addresses(Get_Host_By_Name (Host_Name), 1);
	Address.Port := 5876;
	Create_Socket(Server);
	Set_Socket_Option(Server, Socket_Level, (Reuse_Address, True));
	Bind_Socket(Server, Address);
	Listen_Socket(Server);
	
	-- Accept players
	for Index in Player_Index_t'First .. Player_Index_t'Last loop
		Put_Line("Waiting for player " & Index'Image);
		AcceptPlayer(Color_t'Val(Index - 1), Server, Address, Players(Index));
	end loop;
	
	-- Play
	Board := StartBoard;
	Finished := False;
	CurrPlayerIndex := GetWhitePlayer(Players);
	
	while not Finished loop
		CurrPlayer := Players(CurrPlayerIndex);
		Put_Line("Waiting for " & CurrPlayer.Color'Image & " move");
		
		CurrMove := GetMove(CurrPlayer, String'Input(CurrPlayer.Channel));
		MoveResult := Move(Board, CurrMove);
		
		case MoveResult is
		when Invalid_Move =>
			Put_Line("Invalid move from " & CurrPlayer.Color'Image);
			String'Output(CurrPlayer.Channel, "Invalid move from " & CurrPlayer.Color'Image);
			-- TODO send error to current player
			null;
		when Success =>
			Put_Line(CurrPlayer.Color'Image & " moved");
			String'Output(CurrPlayer.Channel, CurrPlayer.Color'Image & " moved");
			-- TODO broadcast move to the other player
			CurrPlayerIndex := 3 - CurrPlayerIndex;
		end case;
		
		Finished := Game_Ended(Board);
	end loop;

	-- Close
	Close_Socket(Server);
	Close_Socket(Players(1).Socket);
	Close_Socket(Players(2).Socket);
end Server;
