with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;


package body Board.Strings.Pretty is

    
    procedure Print_Files_Left(File : in File_t; Rank : in Rank_t) is
    begin
        if File = a then
            Put(" " & Image(Rank) & " ");
        end if;
    end Print_Files_Left;
        
    procedure Print_Files_Right(File : in File_t;
                                Rank : in Rank_t)
    is
    begin
        if File = h then
            Put(Palette_Reset & " " & Image(Rank));
        end if;
    end Print_Files_Right;
    
    procedure Put_Cell(Board      : in Board_t;
                       File       : in File_t;
                       Rank       : in Rank_t;
                       Background : in Color_ANSI_t)
    is
        Cell : Opt_Cell_t := Board(File, Rank);
        
        Bg : String := (if (Rank + File_t'Pos(File) + 1) mod 2 = 0
                        then Palette_Dull_Bg(White)
                        else Palette_Dull_Bg(Background));
        
        Fg : String := (if not Cell.IsEmpty and then Cell.Value.Color = White
                        then Palette_Bright_Fg(Black)
                        else Palette_Bright_Fg(Black));
    begin
        Put(Bg & " " & Fg & Image(Cell) & " ");
    end Put_Cell;
    
    
    procedure Pretty_Print(Board : in Board_t; Background : in Color_ANSI_t := Black) is
    begin
        -- Top ranks
        Put_Line("    a  b  c  d  e  f  g  h");
        
        for Rank in 1 .. 8 loop
              
            for File in a .. h loop
                -- Left files
                Print_Files_Left(File, Rank);
                
                -- Print the cell
                Put_Cell(Board, File, Rank, Background);
                
                -- Right files
                Print_Files_Right(File, Rank);
            end loop;
            
            New_Line;
            
        end loop;
        
        -- Bottom ranks
        Put_Line("    a  b  c  d  e  f  g  h");
        
    end Pretty_Print;
    
    
    procedure Pretty_Print(Board : in Board_t; CurrPlayer : in Color_t; Background : in Color_ANSI_t := Black)
    is
        Token_White : String := (if CurrPlayer = White then "➤" else " ");
        Token_Black : String := (if CurrPlayer = Black then "➤" else " ");
    begin
        -- Top ranks
        Put_Line("    a  b  c  d  e  f  g  h");

        for Rank in 1 .. 8 loop
            
            for File in a .. h loop
                -- Left files
                Print_Files_Left(File, Rank);
                
                -- Print the cell
                Put_Cell(Board, File, Rank, Background);
                
                -- Right files
                Print_Files_Right(File, Rank);
                
                if File = h then
                    case Rank is
                        when 2 => Put("    " & Token_White & " White");
                        when 7 => Put("    " & Token_Black & " Black");
                        when others => null;
                    end case;
                end if;
            end loop;
            
            New_Line;
            
        end loop;
        
        -- Bottom ranks
        Put_Line("    a  b  c  d  e  f  g  h");

    end Pretty_Print;
    

end Board.Strings.Pretty;
