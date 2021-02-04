# ADA CHESS

Un jeu d'echecs écrit en Ada, où les deux joueurs jouent en se connectant à un serveur tcp/ip.

![Board](.github/colored_board.png)

## 1. Instructions de build

Le projet construit deux binaires, `server_cli` et `client_cli`, avec la commande suivante :
```
gprbuild -d chess.gpr
```

## 2. Manuel d'utilisation

### 2.1. Server

#### Utilisation

```
server_cli [OPTIONS]
```

#### Options

##### `-p PORT` choisit le port où écouter (par défaut 5876)
##### `-c COLOR` choisit la couleur du plateau d'echecs parmi les couleurs du stadar ANSI (par défaut Black)
##### `-l LOGLEVEL` choisit le niveau de logs parmi Error, Info ou Debug (par défaut Info)


### 2.3. Client

#### Utilisation

```
client_cli [OPTIONS]
```

#### Options

##### `-p PORT` choisit le port où écouter (par défaut 5876)
##### `-c COLOR` choisit la couleur du plateau d'echecs parmi les couleurs du stadar ANSI (par défaut Black)
##### `-l LOGLEVEL` choisit le niveau de logs parmi Error, Info ou Debug (par défaut Info)

#### Mouvement

Le jeu est basé sur terminal, pour bouger le joueur doit utiliser la [notation algébrique](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)) côté client, cette string de mouvement est ensuite parsée par le serveur, validée puis executée.

Par exemple, pour bouger un pion de e7 vers e5, la notation est 'e5'. Pour bouger un cavalier de e8 vers f6, la notation est 'Nf6', ou 'Nef6' en cas d'ambiguité entre les cavaliers si les deux peuvent aller en f6. Les coordonnées d'origine sont donc optionelles, mais les spécifier reste un mouvement valide, cela accélère même le traitement de la commande.

![State machine](.github/digraph_parser.png)

## 3. Architecture du projet

TODO

## 4. Safe programming

TODO

## 5. DO178

TODO
