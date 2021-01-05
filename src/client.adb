with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Board; use Board;


procedure Client is
	Address  : Sock_Addr_Type;
	Socket   : Socket_Type;
	Channel  : Stream_Access;
		
	Finished : Boolean;
	Str      : String(1 .. 80);
	Last     : Natural;
	My_Color : Color_t;
begin
	-- Connect
	Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
	Address.Port := 5876;
	Create_Socket (Socket);
	Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
	Connect_Socket (Socket, Address);
	Channel := Stream (Socket);
	
	-- Play
	My_Color := Color_t'Value(String'Input(Channel));
	Finished := False;
	
Finished_Loop:
	while not Finished loop
		-- Get and send move
		Str := (others => Character'Val(0));
		Put(My_Color'Image & "> ");
		Get_Line(Str, Last);
		
		exit Finished_Loop when Str(1 .. Last) = "quit";
		
		String'Output(Channel, Str);
		
		-- Receive response (success, error, you win...)
		Put_Line("[SERVER] " & String'Input(Channel));
		--Finished := True;
	end loop Finished_Loop;
		
	-- Disconnect
	Close_Socket (Socket);
end Client;
