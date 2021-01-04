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
		
	procedure AcceptPlayer(Server : in out Socket_Type; Address : in out Sock_Addr_Type; Player : in out Player_t) is
	begin
		Accept_Socket(Server, Player.Socket, Address);
		Player.Channel := Stream(Player.Socket);
		Put_Line("Accepted client");
		-- TODO Authenticate, assign color...
	end AcceptPlayer;
		
	function GetWhitePlayer(Players : in Players_t) return Player_Index_t is
	begin
		-- TODO
		return 1;
	end GetWhitePlayer;
		
	function GetMove(Input : in String) return Move_t is
	begin
		-- TODO
		return ((A, 1), (A, 2));
	end GetMove;

	Address    : Sock_Addr_Type;
	Server     : Socket_Type;
	Players    : Players_t;
	
	Finished   : Boolean;
	Board      : Board_t;
	CurrPlayer : Player_Index_t;
	CurrMove   : Move_t;
	MoveResult : MoveResult_t;
begin
	-- Connect
	Address.Addr := Addresses(Get_Host_By_Name (Host_Name), 1);
	Address.Port := 5876;
	Create_Socket(Server);
	Set_Socket_Option(Server, Socket_Level, (Reuse_Address, True));
	Bind_Socket(Server, Address);
	Listen_Socket(Server);
	Put_Line("Listening for connections");
	
	-- Accept players
	for Index in 1 .. 2 loop
		AcceptPlayer(Server, Address, Players(Index));
	end loop;
	
	-- Play
	Board := StartBoard;
	Finished := False;
	CurrPlayer := GetWhitePlayer(Players);
	
	while not Finished loop
		CurrMove := GetMove(String'Input(Players(CurrPlayer).Channel));
		MoveResult := Move(Board, CurrMove);
	
		case MoveResult is
		when Invalid_Move =>
		-- TODO send error to current player
			null;
		when Success =>
			-- TODO broadcast move to the other player
			CurrPlayer := 3 - CurrPlayer;
		end case;
				
		Finished := True;
	end loop;
				
	-- Close
	Close_Socket(Server);
	Close_Socket(Players(1).Socket);
	Close_Socket(Players(2).Socket);
end Server;
