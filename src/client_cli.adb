with GNAT.Sockets; use GNAT.Sockets;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Client;


procedure Client_CLI is

    Addr : Inet_Addr_Type := Addresses(Get_Host_By_Name(Host_Name), 1);
    Port : Port_Type      := 5876;

begin
    if Argument_Count > 0 then
        Port := Port_Type'Value(Argument(1));
    end if;

    Client.Launch(Addr, Port);

exception
    when Socket_Error =>
        Put_Line("Server not found, exiting.");

end Client_CLI;
