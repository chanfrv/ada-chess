# ADA CHESS

Un jeu d'echecs écrit en Ada, où les deux joueurs jouent en se connectant à un serveur tcp/ip.

![Board](.github/colored_board.png)

## 1. Instructions de build

Le projet construit deux binaires, `server_cli` et `client_cli`, avec la commande suivante :
```
gprbuild -d chess.gpr
```

## 2. Manuel d'utilisation

### 2.1 Serveur

#### Utilisation

```
server_cli [OPTIONS]
```

#### Options

##### `-p PORT` choisit le port où écouter (par défaut 5876)
##### `-c COLOR` choisit la couleur du plateau d'echecs parmi les couleurs du standard ANSI (par défaut Black)
##### `-l LOGLEVEL` choisit le niveau de logs parmi Error, Info ou Debug (par défaut Info)


### 2.2 Client

#### Utilisation

```
client_cli [OPTIONS]
```

#### Options

##### `-p PORT` choisit le port où écouter (par défaut 5876)
##### `-c COLOR` choisit la couleur du plateau d'echecs parmi les couleurs du standard ANSI (par défaut Black)
##### `-l LOGLEVEL` choisit le niveau de logs parmi Error, Info ou Debug (par défaut Info)

#### Mouvement

Le jeu est basé sur terminal, pour bouger le joueur doit utiliser la [notation algébrique](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)) côté client, cette string de mouvement est ensuite parsée par le serveur, validée puis executée.

Par exemple, pour bouger un pion de e7 vers e5, la notation est 'e5'. Pour bouger un cavalier de e8 vers f6, la notation est 'Nf6', ou 'Nef6' en cas d'ambiguité entre les cavaliers si les deux peuvent aller en f6. Les coordonnées d'origine sont donc optionelles, mais les spécifier reste un mouvement valide, cela accélère même le traitement de la commande.

## 3. Architecture du projet

### 3.1 Modules

Les différents modules sont découpés par des sous-dossiers de `src/`, ils sont les suivants:

#### - `client` qui contient le client appélé par `client_cli`
#### - `server` qui contient le serveur appelé par `server_cli`
#### - `board` qui contient l'essentiel du projet, toutes les fonctionnalités allant du parsing à l'execution du mouvement sont effectuées dans ce module
#### - `logs` le module du logger
#### - `optional` un record generique permettant de créer des valeurs optionelles comme `std::optional` du c++

### 3.2 Graph de dépendances

Les deux racines sont client et server, ces deux modules utilisent board et logs, board utlisant lui-même logs et optional.

```
     client
    /      \
   /        v
   |        server
    \      /      \
     v    v        v
      logs <------- board.* --> optional
```

### 3.3 Client

Le client consiste simplement en une procedure `Launch()` qui tente de se connecter au serveur à l'adresse donnée. Si la connexion est un succès, le client va ensuite entrer la boucle de jeu où tant que la partie n'est pas terminée, on lit les mouvements de l'utilisateur et on les envoie au serveur.

### 3.4 Serveur

Le serveur lui aussi consiste simplement en un procédure `Launch()` qui écoute sur localhost au port donné. Ce serveur doit donc forcément être lancé avant les clients. Une fois les deux clients connectés, on entre là aussi la boucle de jeu où tant que la partie n'est pas terminée, on reçoit les mouvements des joueurs, ces dernières sont ensuite parsées, validées puis executées. Le nouveau board est ensuite envoyé aux joueurs puis on passe au joueur  suivant.

### 3.5 Board

Le board est le module principal du programme, il définit les différents types dont les pièces et le plateau ainsi qui les fonctions de mouvements. Le module est donc responsable de la validation et de l'execution des mouvements considérés "normaux".

### 3.5.b Board.Castling

Ce sous-module est tourné spécifiquement vers la règle du castling. Lorsque le parseur reçoit "0-0" ou "0-0-0", le mouvement est donc respectivement soit un castling côté Roi soit un castling côté Reine qui, si les conditions sont réunies, est executé sans passer par les règles de mouvement classiques (même si elles sont quand même respectées).

### 3.5.c Board.EnPassant

Ce sous-module est spécifique pour la règle dite en passant. Lorsqu'un pion bouge de 2 cases (donc lors de son premier mouvement), cette pièce est considérée une cible valide pour un en passant lors du prochain tour.

La fonction `EnPassant_Handler()` utilise donc le mouvement précédent pour déterminer si le mouvement courant est un en passant et l'execute, elle ensuite ne considère plus aucune pièce pour en passant car celui-ci n'est possible que pour 1 tour, et enfin enregistre le pion courant s'il bouge de 2 cases pour le prochain tour.

### 3.5.d Board.Strings

Ce module contient les fonctions `Image()` pour tous les composants du module `Board`. Ces fonctions convertissent donc les types internes en chaînes de charactère. Ce module est particulièrement utilisé pour le logging et le pretty printer.

### 3.5.e Board.Strings.Parse

Le parser est responsable de convertir la chaîne de charactère en notation algébrique vers le type interne de mouvement, ce mouvement étant ensuite donné au module principal `Board` pour la validation et l'execution.

Le parser est implémenté par une machine à états, elle même implémentée par un graph dans une matrice d'adjacence. La chaîne de charactères est donnée en entrée de la fonction `Traverse()` qui itère de token en token pour matcher la grammaire et remplir les informations du mouvement.

La grammaire est donc la suivante :

![State machine](.github/digraph_parser.png)

D'un point de vue grammaire, tous les champs sont optionels à part le champ `To` qui sont les coordonnées d'arrivée, c'est pourquoi il est possible de partir de n'importe quel état avant `To` vers le suivant jusqu'à `To`, puis de n'importe quel état après `To` vers le suivant jusqu'à la fin. Les deux tokens spéciaux étant le castling qui n'a pas besoin de coordonnées.

### 3.5.f Board.Strings.Pretty

Le pretty printer consiste en une procédure `Pretty_Print()` qui prend le board en entrée (et optionellement le joueur courant) et affiche le plateau avec les pièces en couleurs sur le terminal.


### 3.6 Logs

Le loggeur est assez simple, il consiste en une variable globale `Level` qui est soit `Error`, soit `Info`, soit `Debug`. Lorsqu'une des procédures `Error()`, `Info()` ou `Debug()` est executée, si le niveau est supérieur où égal au niveau choisit avec les arguements des binaires, le loggeur va affichée une string formatée au format `[CLOCK][LEVEL] MESSAGE`.

### 3.7 Optional

Ce record utilitaire permet donc d'implémenter un équivalent de `std::optional` du c++, il a un paramètre booléen `IsEmpty` qui résulte en un record null si `True`, une valeur de type `T` si `False`. Ce optional est souvent utilisé dans le board pour représenter une case vide ou encore des valeurs non renseignées dons la notation algébrique.


## 4. DO178

### 4.1 High Level Requirements

### 4.2 Low Level Requirements

### 4.3 Tests
