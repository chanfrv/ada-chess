with GNAT.Sockets; use GNAT.Sockets;
with Board; use Board;


package Communication is

    type Command is (Move, Forfeit);
    
    type CommandLine(Id : Command) is
        record
            case Id is
                when Move =>
                    Move : Move_t;
                when Forfeit =>
                    null;
            end case;
        end record;
    
    function Image(Id : Command) return String;
    function Image(Cmd: CommandLine) return String;
    
    procedure Send(Channel : Stream_Access; Cmd : CommandLine);
    
end Communication;
