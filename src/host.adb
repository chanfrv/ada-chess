with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;


procedure Host is
	Address  : Sock_Addr_Type;
	Socket   : Socket_Type;
	Channel  : Stream_Access;
begin
	Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
	Address.Port := 5876;
	Create_Socket (Socket);
	Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
	Connect_Socket (Socket, Address);
   
	Channel := Stream (Socket);
	String'Output (Channel, "Hello world");
	Close_Socket (Socket);
end Host;
