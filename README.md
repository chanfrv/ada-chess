# ADA CHESS

A chess game written in Ada, where both players play by sending tcp commands to a server.

![Board](.github/colored_board.png)

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

### Options

#### `-p PORT` set the port to listen on
#### `-c COLOR` set the board color among ANSI colors
#### `-l LOGLEVEL` set the log level among Error, Info or Debug


## Client

### Usage

```
client_cli [OPTIONS]
```

### Options

#### `-p PORT` set the port to listen on
#### `-c COLOR` set the board color among ANSI colors
#### `-l LOGLEVEL` set the log level among Error, Info or Debug
