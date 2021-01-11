with GNAT.Sockets; use GNAT.Sockets;
with Ada.Command_Line; use Ada.Command_Line;

with Server;


procedure Server_CLI is
    
    Port : Port_Type := 5876;
    
begin
    if Argument_Count > 0 then
        Port := Port_Type'Value(Argument(1));
    end if;
        
    -- Launch server
    Server.Launch(Port);
    
end Server_CLI;
