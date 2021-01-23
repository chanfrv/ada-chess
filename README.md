# ADA CHESS

A chess game written in Ada, where both players play by sending tcp commands to a server.

```
    a  b  c  d  e  f  g  h
 1  ♖  ♘  ♗  ♕  ♔  ♗  ♘  ♖ 1
 2  ♙  ♙  ♙  ♙     ♙  ♙  ♙ 2      White
 3                         3
 4              ♙          4
 5                         5
 6                         6
 7  ♟︎  ♟︎  ♟︎  ♟︎  ♟︎  ♟︎  ♟︎  ♟︎ 7    ➤ Black
 8  ♜  ♞  ♝  ♛  ♚  ♝  ♞  ♜ 8
    a  b  c  d  e  f  g  h
```

## Build

Build the project with the command:
```
gprbuild -d chess.gpr
```

## Server

### Usage

```
server_cli [OPTIONS]
```

#### *`-p PORT`* set the port to listen on
#### *`-c COLOR`* set the board color among ANSI colors
#### *`-l LOGLEVEL`* set the log level among Error, Info or Debug


## Client

### Usage

```
client_cli [OPTIONS]
```

#### *`-p PORT`* set the port to listen on
#### *`-c COLOR`* set the board color among ANSI colors
#### *`-l LOGLEVEL`* set the log level among Error, Info or Debug
