with GNAT.Sockets; use GNAT.Sockets;
with Ada.Command_Line; use Ada.Command_Line;

with Board.Strings.Pretty; use Board.Strings.Pretty;
with Client;
with Logs;


procedure Client_CLI is

    Addr        : Inet_Addr_Type := Addresses(Get_Host_By_Name(Host_Name), 1);
    Port        : Port_Type      := 5876;
    Board_Color : Color_ANSI_t   := Black;
    Arg_Index   : Natural        := 1;
    Level       : Logs.Level_t   := Logs.Info;
begin

    while Arg_Index <= Argument_Count loop

        if Arg_Index + 1 > Argument_Count then
            Logs.Error("Option '" & Argument(Arg_Index) & "' needs a value");
            Set_Exit_Status(Failure);
            return;

        elsif Argument(Arg_Index) = "-p" then
            Port := Port_Type'Value(Argument(Arg_Index + 1));

        elsif Argument(Arg_Index) = "-c" then
            Board_Color := Color_ANSI_t'Value(Argument(Arg_Index + 1));

        elsif Argument(Arg_Index) = "-l" then
            Level := Logs.Level_t'Value(Argument(Arg_Index + 1));

        else
            Logs.Error("Unrecognized option '" & Argument(Arg_Index) & "'");
            Set_Exit_Status(Failure);
            return;
        end if;

        Arg_Index := Arg_Index + 2;
    end loop;

    -- Setup logger
    Logs.Set_Level(Level);

    -- Launch
    Client.Launch(Addr, Port, Board_Color);

exception
    when Constraint_Error =>
        Logs.Error("Unrecognized value '" & Argument(Arg_Index + 1) & "'");
        Set_Exit_Status(Failure);

end Client_CLI;
