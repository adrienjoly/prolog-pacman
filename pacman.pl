% Projet Prolog : Pacman
% ======================
%  - Celine AUSSOURD
%  - Adrien JOLY

% ____________________
% Predicats dynamiques

:- dynamic posPacman/2.         % posPacman(X,Y) : Coordonnées de pacman
:- dynamic dirPacman/1.         % dirPacman(Dir) : Direction de pacman

:- dynamic posFantome/3.        % posFantome(N,X,Y) : Coordonnées du fantome N
:- dynamic dirFantome/2.        % dirFantome(N,Dir) : Direction du fantome N
:- dynamic cibleFantome/3.      % cibleFantome(N,X,Y) : Coordonnées de la cible du fantome N

:- dynamic decomptePotion/1.    % decomptePotion(T) : Nombre de tours avant la fin d'effet de potion
:- dynamic fin/1.               % fin(B)
:- dynamic score/1.             % score(S)
:- dynamic visite/2.            % pour la gestion du score ' . ' devient '   '


% _________________________
% Définition des constantes

largeur(19). % Largeur du niveau : nombre de cases colonnes
hauteur(21). % Hauteur du niveau : nombre de cases lignes
dims(Xmax,Ymax) :- largeur(Xmax), hauteur(Ymax).
symetrieHoriz(X,X2) :- largeur(Xmax), Xmax2 is Xmax-1, X2 is Xmax2-X.

debugMode :- fail.
%debugMode.  % décommenter pour passer en mode débogage (traces, etc...)

% __________________________
% Compatibilité avec bProlog

%between(Min,Max,V) :- V >= Min, V =< Max.
%get_single_char(X) :- get0(X).

% ______________________
% Validation de position

ligne(Y) :- hauteur(Haut), Ymax is Haut-1, between(0,Ymax,Y). % Y >= 0, Y < Haut.
colonne(X) :- largeur(Larg), Xmax is Larg-1, between(0,Xmax,X). % X >= 0 ,X < Larg.
case(X,Y) :- ligne(Y),colonne(X).
caseCirculable(X,Y) :- case(X,Y), not(obstacle(X,Y)).
obstacle(X,Y) :- mur(X,Y) ; posFantome(_N,X,Y).
        
% angle : case circulable connectée perpendiculairement à au moins 2 autres cases circulables

angle(X,Y) :-
    caseCirculable(X,Y),
    ( ( gauche(X1,Y1,X,Y), voisinCirculable(X,Y,X1,Y1) ) ;
      ( droite(X2,Y2,X,Y), voisinCirculable(X,Y,X2,Y2) ) ),
    ( ( dessus(X3,Y3,X,Y), voisinCirculable(X,Y,X3,Y3) ) ;
      ( dessous(X4,Y4,X,Y), voisinCirculable(X,Y,X4,Y4) ) ).

% carrefour : case circulable connectée à au moins 3 autres cases circulables

carrefour(X,Y) :-
    caseCirculable(X,Y),
    gauche(X1,Y1,X,Y), voisinCirculable(X,Y,X1,Y1),
    droite(X2,Y2,X,Y), voisinCirculable(X,Y,X2,Y2),
    ( ( dessus(X3,Y3,X,Y), voisinCirculable(X,Y,X3,Y3) ) ;
      ( dessous(X4,Y4,X,Y), voisinCirculable(X,Y,X4,Y4) ) ).

carrefour(X,Y) :-
    caseCirculable(X,Y),
    dessus(X1,Y1,X,Y), voisinCirculable(X,Y,X1,Y1),
    dessous(X2,Y2,X,Y), voisinCirculable(X,Y,X2,Y2),
    ( ( gauche(X3,Y3,X,Y), voisinCirculable(X,Y,X3,Y3) ) ;
      ( droite(X4,Y4,X,Y), voisinCirculable(X,Y,X4,Y4) ) ).

% _____________________________
% Points cardinaux (directions)
% Nord=0, Est=1, Sud=2, Ouest=3

dirIncrement(0,0,-1).
dirIncrement(1,1,0).
dirIncrement(2,0,1).
dirIncrement(3,-1,0).

positionSuivante(X3,Y3,X,Y,Xincr,Yincr) :-
    dims(Larg,Haut),                         % recuperation des dimensions du labyrinthe
    X2 is X+Xincr+Larg,                      % incrémentation horizontale
    X3 is X2 mod Larg,                       % modulo la largeur du labyrinthe (wrapping)
    Y2 is Y+Yincr+Haut,                      % incrémentation verticale
    Y3 is Y2 mod Haut.                       % modulo la hauteur


positionSuivanteVersCible(X1,Y1,X,Y,Xc,Yc) :-
    Xdiff is Xc-X, ( ( Xincr is sign(Xdiff), Xincr \== 0) ; Xincr is 1 ),
    Ydiff is Yc-Y, ( ( Yincr is sign(Ydiff), Yincr \== 0) ; Yincr is 1 ),
    ( ( abs(Xdiff) > abs(Ydiff), % si pacman est plus loin horizontalement que verticalement
        ( ( positionSuivante(X1,Y1,X,Y,Xincr,0), caseCirculable(X1,Y1)) ; % increment majeur
          ( positionSuivante(X1,Y1,X,Y,0,Yincr), caseCirculable(X1,Y1)) ; % increment mineur
          ( positionSuivante(X1,Y1,X,Y,0,-Yincr), caseCirculable(X1,Y1)) ; % opposé de l'increment mineur
          ( positionSuivante(X1,Y1,X,Y,-Xincr,0), caseCirculable(X1,Y1)))   % opposé de l'increment majeur
      ) ;                        % si pacman est plus loin verticalement que horizontalement
      ( ( positionSuivante(X1,Y1,X,Y,0,Yincr), caseCirculable(X1,Y1)) ; % increment majeur
        ( positionSuivante(X1,Y1,X,Y,Xincr,0), caseCirculable(X1,Y1)) ; % increment mineur
        ( positionSuivante(X1,Y1,X,Y,-Xincr,0), caseCirculable(X1,Y1)) ; % opposé de l'increment mineur
        ( positionSuivante(X1,Y1,X,Y,0,-Yincr), caseCirculable(X1,Y1))   % opposé de l'increment majeur
      ) ;
      ( X1 is X, Y1 is Y, afficher('[IMMOBILISME positionSuivanteVersCible]') ) ).
          
% ___________________
% Voisinnage de cases

% X1,Y1 est en dessus/dessous/gauche/droite de X2,Y2
gauche(X1,Y1,X2,Y2) :-  case(X2,Y2), Y1 is Y2,   X1 is X2-1, case(X1,Y1).
droite(X1,Y1,X2,Y2) :-  case(X2,Y2), Y1 is Y2,   X1 is X2+1, case(X1,Y1).
dessus(X1,Y1,X2,Y2) :-  case(X2,Y2), Y1 is Y2-1, X1 is X2,   case(X1,Y1).
dessous(X1,Y1,X2,Y2) :- case(X2,Y2), Y1 is Y2+1, X1 is X2,   case(X1,Y1).       

% Voisin : la case [X2,Y2] est voisine directe (sans wrapping) de [X1,Y1]
% => a sa gauche, a sa droite, en dessus ou en dessous

voisin(X1,Y1,X2,Y2) :-
    %case(X1,Y1),
    ( ( Y2 is Y1, ( X2 is X1-1 ; X2 is X1+1 ), case(X2,Y2) ) ;
      ( X2 is X1, ( Y2 is Y1-1 ; Y2 is Y1+1 ), case(X2,Y2) ) ).

% la case [X2,Y2] est directement accessible depuis la case [X1,Y1]

voisinCirculable(X1,Y1,X2,Y2) :-
    %case(X1,Y1),
    ( ( Y2 is Y1, ( X2 is X1-1 ; X2 is X1+1 ), caseCirculable(X2,Y2) ) ;
      ( X2 is X1, ( Y2 is Y1-1 ; Y2 is Y1+1 ), caseCirculable(X2,Y2) ) ).


% __________________________________________________
% DEFINITION DU NIVEAU ET POSITIONNEMENT DES ENTITES

mur(X,0) :- colonne(X). % murs haut
mur(X,Y) :- Y \= 9, ligne(Y), largeur(Xmax), X is Xmax-1. % murs droite
mur(X,Y) :- hauteur(Ymax), Y is Ymax-1, colonne(X). % murs bas
mur(0,Y) :- Y \= 9, ligne(Y). % murs gauche

mur(X,1) :- X=9.
mur(X,2) :- X=2;X=3;X=5;X=6;X=7;X=9.
mur(X,4) :- X=2;X=3;X=5;X=7;X=8;X=9.
mur(X,5) :- X=5;X=9.
mur(X,6) :- X=1;X=2;X=3;X=5;X=6;X=7;X=9.
mur(X,7) :- X=1;X=2;X=3;X=5.
mur(X,8) :- X=1;X=2;X=3;X=5;X=7;X=9.
mur(X,9) :- X=7.
mur(X,10) :- X=1;X=2;X=3;X=5;X=7;X=8;X=9.
mur(X,11) :- X=1;X=2;X=3;X=5.
mur(X,12) :- X=1;X=2;X=3;X=5;X=7;X=8;X=9.
mur(X,13) :- X=9.
mur(X,14) :- X=2;X=3;X=5;X=6;X=7;X=9.
mur(X,15) :- X=3.
mur(X,16) :- X=1;X=3;X=5;X=7;X=8;X=9.
mur(X,17) :- X=5;X=9.
mur(X,18) :- X=2;X=3;X=4;X=5;X=6;X=7;X=9.

mur(R,Y) :- mur(X,Y), symetrieHoriz(X,R), !.

% posPotion(X,Y) : potion aux coordonnees X,Y
posPotion(1,15).
posPotion(1,2).
posPotion(17,15).
posPotion(17,2).

% initPosFantome(N,X,Y) : coordonnees initiales du fantome N : [X,Y]
initPosFantome(0,8,8).
initPosFantome(1,10,8).
initPosFantome(2,10,9).
initPosFantome(3,8,9).

initialisation :-
    assert(posPacman(9,15)),
    assert(dirPacman(1)),

    initPosFantome(0,X0,Y0), assert(posFantome(0,X0,Y0)),
    initPosFantome(1,X1,Y1), assert(posFantome(1,X1,Y1)),
    initPosFantome(2,X2,Y2), assert(posFantome(2,X2,Y2)),
    initPosFantome(3,X3,Y3), assert(posFantome(3,X3,Y3)),

    assert(dirFantome(0,0)),
    assert(dirFantome(1,0)),
    assert(dirFantome(2,0)),
    assert(dirFantome(3,0)),

    assert(score(0)),

    posPacman(X,Y), % la case où se trouve Pacman ne contient pas de pastille
    assert(visite(X,Y)),

    assert(visite(8,8)),   % pas de pastille dans la "maison" des fantomes
    assert(visite(10,8)),
    assert(visite(8,9)),
    assert(visite(9,9)),
    assert(visite(10,9)).


destruction :-
    retractall(fin(_X)),            % pour pouvoir relancer le jeu
    retractall(score(_X)),
    retractall(visite(_X,_Y)),
    retractall(posPacman(_X,_Y)),
    retractall(dirPacman(_Dir)),
    retractall(posFantome(_N,_X,_Y)),
    retractall(dirFantome(_N,_Dir)),
    retractall(decomptePotion(_T)).


% ______________________________________
% Symboles textuels associés aux entités

schema(X,Y,'RiP') :- posFantome(_,X,Y), posPacman(X,Y), !.
schema(X,Y,' V ') :- dirPacman(0), posPacman(X,Y), !.
schema(X,Y,' < ') :- dirPacman(1), posPacman(X,Y), !.
schema(X,Y,' ^ ') :- dirPacman(2), posPacman(X,Y), !.
schema(X,Y,' > ') :- dirPacman(3), posPacman(X,Y), !.
schema(X,Y,' 1 ') :- decomptePotion(_T), posFantome(0,X,Y),!.
schema(X,Y,' 2 ') :- decomptePotion(_T), posFantome(1,X,Y),!.
schema(X,Y,' 3 ') :- decomptePotion(_T), posFantome(2,X,Y),!.
schema(X,Y,' 4 ') :- decomptePotion(_T), posFantome(3,X,Y),!.
schema(X,Y,' $ ') :- posFantome(0,X,Y),!.
schema(X,Y,' % ') :- posFantome(1,X,Y),!.
schema(X,Y,' § ') :- posFantome(2,X,Y),!.
schema(X,Y,' @ ') :- posFantome(3,X,Y),!.
schema(X,Y,'[X]') :- debugMode, cibleFantome(_,X,Y),!.
schema(X,Y,' + ') :- debugMode, carrefour(X,Y),!.
schema(X,Y,' L ') :- debugMode, angle(X,Y),!.
schema(X,Y,'###') :- mur(X,Y),!.
schema(X,Y,'   ') :- visite(X,Y),!.  % pastille mangée
schema(X,Y,' O ') :- posPotion(X,Y),!.
schema(_X,_Y,' . ').                  % pastille

% ______________________
% GESTION DE L'AFFICHAGE

clearScreen.                                   % pour vider l'ecran
/*
clearScreen :- clearScreen(0).
clearScreen(Y) :- Y >= 20, !.
clearScreen(Y) :- nl, Y2 is Y+1, clearScreen(Y2).
*/

afficher(M) :- debugMode, write(M).       % alias de write utilisé pour afficher des traces
afficher(_).

nl2 :- debugMode, nl.                     % alias de nl utilisé pour afficher des traces
nl2.

afficherCoord(Nom1,Coord1,Nom2,Coord2) :-      % affiche des coordonnées (traces)
    afficher(Nom1),afficher('='),afficher(Coord1),afficher(' '),
    afficher(Nom2),afficher('='),afficher(Coord2),afficher(' ').

dessinercarac(X,Y) :- schema(X,Y,Signe), write(Signe).

dessinerligne(Y) :- dessinerligne(0,Y).
dessinerligne(X,_Y) :- largeur(Xmax), X >= Xmax, !.
dessinerligne(X,Y) :- dessinercarac(X,Y), X2 is X+1, dessinerligne(X2,Y).

ttDessiner(Y) :- hauteur(Ymax), Y >= Ymax, !.
ttDessiner(Y) :- dessinerligne(Y), nl, Y2 is Y+1, ttDessiner(Y2).

affichage :- clearScreen, nl, afficherScore, ttDessiner(0).   % dessine le jeu


% ______________
% GESTION DU JEU

jeu :-          % jeu = construction du niveau puis passage a la vie
    write('** jeu : initialisation'), nl,
    destruction,
    initialisation,
    vie.

jeu :- write('Fin de jeu'), nl.

vie :-           % vie = plusieurs tours jusqu'à la mort
    write('** vie'), nl,
    deplaceFantomes,
    affichage,!,
    not(mort),
    not(fin),
    deplacePacman,!,
    not(mort),
    not(fin),
    not(gagne),
    vie,
    !.

vie :- write('Fin de vie'), nl.

fin :-        %dans le cas où l'utilisateur appuie sur a. 
    X is 0, 
    fin(X),
    write('Fin du jeu').

restePastille :- caseCirculable(X,Y), not(visite(X,Y)).

gagne :- 
    not(restePastille),
    write('Vous avez gagné !'), nl.

mort :-          % collision entre pacman et un fantome
    not(decomptePotion(_T)),
    posPacman(XP,YP),
    posFantome(_N,XP,YP),
    write('LE FANTOME VOUS A ATTRAPE'), nl.

% ________________
% GESTION DU SCORE

point(X,Y) :- posPacman(X,Y), not(visite(X,Y)).

ajoutPoint(X,Y) :- 
    point(X,Y), 
    score(S1), 
    S is S1+1,
    retractall(score(_S)),
    assert(score(S)),
    assert(visite(X,Y)).
    
afficherScore :-
    write('votre score est de : '),
    score(X),
    write(X),
    write(' points'),
    nl.

afficheDureePotion(T) :-
    write('Duree d''effet de la potion : '),
    write(T), nl.

trouvePotion(X,Y) :-
    posPotion(X,Y),
    not(visite(X,Y)),
    write('Potion trouvee ! Les fantomes sont vulnerables !'), nl,
    T is 30,
    assert(decomptePotion(T)),
    afficheDureePotion(T).

testPotion(X,Y) :-
    decomptePotion(_T),
    posFantome(N,X,Y),
    write('Vous avez mange un fantome ! Score + 10'), nl,
    score(S0),
    S is S0+10,
    score(S),
    initPosFantome(N,XF,YF),
    changerPosFantome(N,XF,YF),
    fail, !.

testPotion(X,Y) :- trouvePotion(X,Y).

testPotion(_X,_Y) :-
    decomptePotion(T),
    ( ( T is 0,
        write('Effet de la potion termine'),
        retractall(decomptePotion(_T)) ) ;
      ( retractall(decomptePotion(_T)),
        T2 is T-1,
        assert(decomptePotion(T2)),
        afficheDureePotion(T2) ) ).
        
testPotion(_X,_Y).
        


% _________________
% GESTION DE PACMAN

deplacePacman :-
    write('** deplacePacman : attenteTouche...'), nl,
    attenteTouche(K),              % attend l'appui sur une touche
    nl, write('** commandeClavier : '), write(K), nl,
    commandeClavier(K),            % reagit a la touche pressee
    posPacman(X,Y),
    dirPacman(Dir),
    dirIncrement(Dir,Xincr,Yincr),
    changerPosPacman(X,Y,Xincr,Yincr),

    afficher('Position pacman : '),
    afficher(X), afficher('+'), afficher(Xincr), afficher(', '),
    afficher(Y), afficher('+'), afficher(Yincr), nl2.


changerPosPacman(X,Y,Xi,Yi) :-
    positionSuivante(X2,Y2,X,Y,Xi,Yi),    % calcul de la nouvelle position
    ( mur(X2,Y2) ;                        % si il y a un mur on ne fait rien, sinon :
      ( retractall(posPacman(_X,_Y)),     % - suppression de la position en cours
        assert(posPacman(X2,Y2)),         % - enregistrement de la nouvelle position
        testPotion(X2,Y2),                % - gestion des potions
        ( ajoutPoint(X2,Y2) ; true ) ) ). % - gestion du score

changerDirPacman(Dir) :-
    retractall(dirPacman(_Dir)),   % supprimer la direction en cours
    assert(dirPacman(Dir)).        % enregistre la nouvelle direction


% _____________________________
% GESTION DES COMMANDES CLAVIER

commandeClavier(K) :- K == 113, changerDirPacman(3), /*write('gauche'),*/ !.
commandeClavier(K) :- K == 100, changerDirPacman(1), /*write('droite'),*/ !.
commandeClavier(K) :- K == 122, changerDirPacman(0), /*write('haut'),*/ !.
commandeClavier(K) :- K == 115, changerDirPacman(2), /*write('bas'),*/ !.
commandeClavier(K) :- K ==  97, retractall(fin(_X)), assert(fin(0)), !.    % touche a
commandeClavier(_K).

% __________________
% GESTION DU CLAVIER

attenteTouche(K) :- get_single_char(K).  % commande SWI Prolog

% ____________________
% GESTION DES FANTOMES

deplaceFantomes :-
    deplaceFantome(0), deplaceFantome(1), deplaceFantome(2), deplaceFantome(3).

deplaceFantome(N) :-
    write('** deplaceFantome : '), write(N), nl,
    posFantome(N,X,Y),                   % récupération de la position du fantome
    retractall(posFantome(N,_X,_Y)),     % supprimer ce Fantome pour eviter qu'il ne soit un obstacle
    dirFantome(N,Dir),                   % récupération de la direction du fantome
    dirIncrement(Dir,Xi,Yi),             % récupération de l'increment a partir de la direction
    positionSuivante(X1,Y1,X,Y,Xi,Yi),   % calcul de la position prévue pour ce tour
    ( ( caseCirculable(X1,Y1), X2 is X1, Y2 is Y1 ) ; % si elle est circulable, OK
      positionSuivanteVersCible(X2,Y2,X,Y,X1,Y1) ),   % sinon, trouver une autre case
    ( ( angle(X,Y),                      % si on peut changer de direction (angle)
        redirigerFantome(N,X,Y,XF,YF), ! ) ;          % alors on cherche une autre direction
      ( XF is X2, YF is Y2 ) ),          % sinon on continue d'avancer dans la meme direction
    assert(posFantome(N,XF,YF)).         % dans tous les cas on enregistre la nouvelle position

redirigerFantome(N,X,Y,X2,Y2) :-
    write('** redirigerFantome'), nl,
    cibleFantome(N,Xc,Yc),                  % on identifie la cible de ce fantome
    dirFantome(N,ODir),                     % on recupere la direction du fantome
    ( ( trouverItineraire([X3,Y3],[X,Y],[Xc,Yc],ODir),  % [X3,Y3] est l'angle cible
        positionSuivanteVersCible(X2,Y2,X,Y,X3,Y3),     % on se déplace vers cet angle => [X2,Y2]
        Xi is X2-X, Yi is Y2-Y,             % on déduit l'incrément de ce mouvement
        dirIncrement(Dir,Xi,Yi),            % on déduit la direction de cet incrément
        changerDirFantome(N,Dir) ) /*;      % on modifie la direction du fantome
      ( X2 is X, Y2 is Y )*/ ).             % MAIS si il n'y a pas d'itinéraire, on ne bouge pas 
        
changerPosFantome(N,X,Y) :-
%   write('fantom'),write(N),write(':'),write(X),write(','),write(Y),
    retractall(posFantome(N,_X,_Y)),        % supprimer la position en cours
    assert(posFantome(N,X,Y)).              % enregistre la nouvelle position

changerDirFantome(N,Dir) :-
    retractall(dirFantome(N,_Dir)), % supprimer la direction en cours
    assert(dirFantome(N,Dir)).      % enregistre la nouvelle direction

cibleFantome(_N,X,Y) :-         % cible des fantomes,
    decomptePotion(_T),         % dans le cas ou une potion a ete bue par pacman
    posPacman(Xp,Yp),
    dims(Xm,Ym),
    X is Xm-Xp,                 % = la direction opposee de pacman
    Y is Ym-Yp.
%   initPosFantome(N,X,Y).      % = position initiale du fantome, dans sa maison

cibleFantome(_N,X,Y) :-         % cible par défaut des fantomes => pacman
    posPacman(X,Y).
/*  posPacman(Xp,Yp), dirPacman(Dir),
    ( chercherAngle(X,Y,Xp,Yp,Dir) ;
      ( Dir2 is Dir+1 mod 4, chercherAngle(X,Y,Xp,Yp,Dir2) ) ;
      ( Dir2 is Dir+2 mod 4, chercherAngle(X,Y,Xp,Yp,Dir2) ) ;
      ( Dir2 is Dir+3 mod 4, chercherAngle(X,Y,Xp,Yp,Dir2) ) ).
*/


trouverItineraire([X2,Y2],[X1,Y1],[XA,YA], OldDir) :-
    Xdiff is XA-X1,                                         % vecteur horizontal
    Ydiff is YA-Y1,                                         % vecteur vertical
    ( ( Xincr is sign(Xdiff), Xincr \== 0 ) ; Xincr is 1 ), % increment horizontal non nul
    ( ( Yincr is sign(Ydiff), Yincr \== 0 ) ; Yincr is 1 ), % increment vertical non nul

    ( ( abs(Xdiff) > abs(Ydiff), % si pacman est plus loin horizontalement que verticalement
        dirIncrement(Dir,Xincr,0),            % alors increment majeur = horizontal
        dirIncrement(DirMin,0,Yincr) ) ;      %    et increment mineur = vertical
      ( dirIncrement(Dir,0,Yincr),            % sinon increment majeur = vertical
        dirIncrement(DirMin,Xincr,0) ) ),     %    et increment mineur = horizontal

    OppDir is (Dir + 2) mod 4,                % direction opposée de l'increment majeur
    OppDirMin is (DirMin + 2) mod 4,          % direction opposée de l'increment mineur

    % on cherche un angle dans la direction ...
    ( ( OppDir    \== OldDir, chercherAngle(X2,Y2,X1,Y1,Dir) ) ;       % majeure
      ( OppDirMin \== OldDir, chercherAngle(X2,Y2,X1,Y1,DirMin) ) ;    % mineure
      ( DirMin    \== OldDir, chercherAngle(X2,Y2,X1,Y1,OppDirMin) ) ; % mineure opposée
      ( Dir       \== OldDir, chercherAngle(X2,Y2,X1,Y1,OppDir) ) ;    % majeure opposée
    afficher('[IMMOBILISME trouverItineraire]'), fail ). % cas où rien n'est possible

chercherAngle(XA,YA,X,Y,Dir) :-
    dirIncrement(Dir,Xi,Yi),                  % on recupère les increments de direction
    positionSuivante(X2,Y2,X,Y,Xi,Yi),        % on cherche la position suivante
    caseCirculable(X2,Y2),                    % si ce n'est pas circulable on s'arrete
    ( ( angle(X2,Y2), XA is X2, YA is Y2 ) ;  % si c'est un angle, on a notre solution
      ( posPacman(X2,Y2), XA is X2, YA is Y2 ) ;  % encore mieux si c'est pacman !
      ( chercherAngle(XA,YA,X2,Y2,Dir) ) ).   % sinon on continue de chercher

:- jeu.
