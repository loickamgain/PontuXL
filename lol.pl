:- use_module(library(lists)).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*        PRODUIRE_REPONSE(L_Mots,L_strings) :                           */
/*                                                                       */
/*        Input : une liste de mots L_Mots representant la question      */
/*                de l'utilisateur                                       */
/*        Output : une liste de strings donnant la                       */
/*                 reponse fournie par le bot                            */
/*                                                                       */
/*        NB Par défaut le predicat retourne dans tous les cas           */
/*            [  "Je ne sais pas.", "Les étudiants",                     */
/*               "vont m'aider, vous le verrez !" ]                      */
/*                                                                       */
/*        Je ne doute pas que ce sera le cas ! Et vous souhaite autant   */
/*        d'amusement a coder le predicat que j'ai eu a ecrire           */
/*        cet enonce et ce squelette de solution !                       */
/*                                                                       */
/* --------------------------------------------------------------------- */

% Réagit à 'fin' partout dans la phrase (avant toute autre règle)
produire_reponse(L, [ "Merci de m'avoir consulté." ]) :-
    member(fin, L), !.

% Mécanisme normal : mot-clé -> regle_rep/4 qui matche un pattern
produire_reponse(L,Rep) :-
    mclef(M,_), member(M,L),
    clause(regle_rep(M,_,Pattern,Rep),Body),
    match_pattern(Pattern,L),
    call(Body), !.

% Réponse par défaut
produire_reponse(_,[S1,S2]) :-
    S1 = "Je ne sais pas.",
    S2 = "Les étudiants vont m'aider, vous le verrez !".

/* ---------------------- Moteur de patterns ---------------------------- */

match_pattern(Pattern,Lmots) :-
    sublist(Pattern,Lmots).

match_pattern(LPatterns,Lmots) :-
    match_pattern_dist([100|LPatterns],Lmots).

match_pattern_dist([],_).
match_pattern_dist([N,Pattern|Lpatterns],Lmots) :-
    within_dist(N,Pattern,Lmots,Lmots_rem),
    match_pattern_dist(Lpatterns,Lmots_rem).

within_dist(_,Pattern,Lmots,Lmots_rem) :-
    prefixrem(Pattern,Lmots,Lmots_rem).
within_dist(N,Pattern,[_|Lmots],Lmots_rem) :-
    N > 1, Naux is N-1,
    within_dist(Naux,Pattern,Lmots,Lmots_rem).

sublist(SL,L) :-
    prefix(SL,L), !.
sublist(SL,[_|T]) :-
    sublist(SL,T).

% manquait : utilisé par sublist/2
prefix([], _).
prefix([H|T], [H|R]) :- prefix(T, R).

sublistrem(SL,L,Lr) :-
    prefixrem(SL,L,Lr), !.
sublistrem(SL,[_|T],Lr) :-
    sublistrem(SL,T,Lr).

prefixrem([],L,L).
prefixrem([H|T],[H|L],Lr) :- prefixrem(T,L,Lr).

/* ---------------------------- Base ----------------------------------- */

nb_lutins(4).
nb_equipes(4).

mclef(commence,10).
mclef(equipe,5).
mclef(quipe,5).
mclef(deplacer,8).
mclef(deplac,8).
mclef(retirer,7).
mclef(pont,6).
mclef(regle,9).
mclef(gagner,9).
mclef(plateau,7).
mclef(ordre,8).
mclef(lutin,5).
mclef(bonjour,5).
mclef(merci,5).
mclef(ia,7).
mclef(intelligence,7).
mclef(artificielle,7).
mclef(jouer,8).
mclef(elimine,8).
mclef(couleur,6).
mclef(fin,100).  % garder une seule clé fin

/* -------------------------- Règles QA -------------------------------- */

regle_rep(commence,1,
 [ qui, commence, le, jeu ],
 [ "Par convention, c'est au joueur en charge des lutins verts de commencer la partie." ] ).

regle_rep(equipe,5,
  [ [ combien ], 3, [ lutins ], 5, [ equipe ] ],
  [ chaque, equipe, compte, X, lutins ]) :-
       nb_lutins(X).

regle_rep(quipe,5,
  [ [ combien ], 3, [ lutin ], 5, [ quipe ] ],
  [ "Chaque équipe compte ", X_in_chars, "lutins." ]) :-
       nb_lutins(X),
       write_to_chars(X,X_in_chars).

% version générique de write_to_chars
write_to_chars(N,S) :-
    number_chars(N, Chars),
    atom_chars(A, Chars),
    atom_concat(A, ' ', A2),
    atom_string(A2, S).

/* ------------------ Déplacer : variantes pratiques ------------------- */

% "Comment déplacer un lutin ?"
regle_rep(deplacer,1,
 [ [ comment ], 3, [ deplacer ], 3, [ lutin ] ],
 [ "Pour déplacer un lutin, faites-le passer d'une case à une autre en empruntant un pont.",
   "Deux lutins ne peuvent pas se trouver sur la même case." ] ).

% "Quel lutin ... déplacer ?" (singulier)
regle_rep(deplacer,2,
 [ [ quel ], 8, [ lutin ], 8, [ deplacer ] ],
 [ "Conseil déplacement : privilégiez un lutin qui garde au moins deux ponts accessibles après le coup.",
   "Évitez de déplacer un lutin qui se mettrait en impasse immédiate." ] ).

% "Quels/quel ... lutins ... déplacer ?" (pluriel)
regle_rep(deplacer,3,
 [ [ quel ], 8, [ lutins ], 8, [ deplacer ] ],
 [ "Choisissez un lutin qui conserve plusieurs sorties et qui rapproche un adversaire d'une impasse.",
   "Regardez aussi quel pont vous retirerez ensuite pour menacer un isolement." ] ).

% "… et vers quelle case ?"
regle_rep(deplacer,4,
 [ [ vers ], 4, [ quelle ], 3, [ case ] ],
 [ "Visez une case qui : (1) garde au moins deux ponts accessibles, (2) met la pression sur un adversaire, (3) n'affaiblit pas vos propres options." ] ).

/* ---------------------- Déplacer / Occupation ------------------------ */

regle_rep(deplac,1,
 [ [ puis ], 3, [ deplacer ], 3, [ lutin ], 3, [ occupe ] ],
 [ "Non, vous ne pouvez pas déplacer un lutin sur une case occupée par un autre lutin." ] ).

/* ------------------------- Ponts ------------------------------------- */

regle_rep(retirer,1,
 [ [ quel ], 3, [ pont ], 3, [ retirer ] ],
 [ "Il est permis de retirer le pont emprunté ou tout autre pont.",
   "Vous pouvez aussi choisir de tourner un pont d'un quart de tour au lieu de le retirer." ] ).

regle_rep(pont,1,
 [ [ comment ], 3, [ retirer ], 3, [ pont ] ],
 [ "Après avoir déplacé un lutin, vous devez retirer un pont ou tourner un pont d'un quart de tour.",
   "Cela fait partie de votre tour de jeu." ] ).

/* ---------------------- Règles générales ----------------------------- */

regle_rep(regle,1,
 [ [ quelles ], 3, [ regles ], 3, [ jeu ] ],
 [ "Pontu est un jeu où vous déplacez vos lutins et retirez des ponts pour isoler les lutins adverses.",
   "À votre tour, vous déplacez un lutin en empruntant un pont, puis vous retirez ou tournez un pont.",
   "Un joueur est éliminé quand tous ses lutins sont isolés sans pont autour d'eux.",
   "Le dernier joueur non éliminé gagne la partie." ] ).

regle_rep(gagner,1,
 [ [ comment ], 3, [ gagner ] ],
 [ "Le dernier joueur non éliminé gagne la partie.",
   "Pour cela, isolez les lutins adverses en retirant les ponts de manière stratégique." ] ).

regle_rep(plateau,1,
 [ [ taille ], 3, [ plateau ] ],
 [ "Dans PontuXL, le plateau est de taille 6x6, contrairement à la version originale en 5x5." ] ).

regle_rep(ordre,1,
 [ [ ordre ], 3, [ joueurs ] ],
 [ "Les joueurs jouent dans l'ordre suivant : vert, bleu, jaune, rouge.",
   "Contrairement à la version originale, les bleus et rouges sont contrôlés par une intelligence artificielle." ] ).

regle_rep(lutin,1,
 [ [ combien ], 3, [ lutins ], 3, [ joueur ] ],
 [ "Chaque joueur dispose de 4 lutins de sa couleur." ] ).

regle_rep(fin,1,
 [ [ quand ], 3, [ fin ], 3, [ partie ] ],
 [ "La partie se termine lorsqu'il ne reste plus qu'un seul joueur dont les lutins ne sont pas tous isolés.",
   "Ce joueur est déclaré vainqueur." ] ).

/* ------------------------- Salutations etc. -------------------------- */

regle_rep(bonjour,1,
 [ bonjour ],
 [ "Bonjour ! Je suis PBot, le bot explicateur du jeu PontuXL.",
   "En quoi puis-je vous aider ?" ] ).

regle_rep(merci,1,
 [ merci ],
 [ "Je vous en prie ! N'hésitez pas si vous avez d'autres questions." ] ).

regle_rep(ia,1,
 [ [ intelligence ], 3, [ artificielle ] ],
 [ "Ici, les lutins bleus et rouges sont contrôlés par une intelligence artificielle basée sur des algorithmes de recherche.",
   "Les lutins verts et jaunes sont contrôlés par des joueurs humains." ] ).

regle_rep(jouer,1,
 [ [ comment ], 3, [ jouer ] ],
 [ "À votre tour : (1) déplacer un lutin en empruntant un pont, (2) retirer un pont ou en tourner un d'un quart de tour.",
   "Le but est d'isoler les lutins adverses." ] ).

regle_rep(elimine,1,
 [ [ quand ], 3, [ joueur ], 3, [ elimine ] ],
 [ "Un joueur est éliminé lorsque tous ses lutins sont sans pont autour d'eux : ils sont isolés et ne peuvent plus se déplacer." ] ).

regle_rep(couleur,1,
 [ [ quelles ], 3, [ couleurs ], 3, [ lutins ] ],
 [ "Dans PontuXL, il y a quatre couleurs de lutins : vert, bleu, rouge et jaune.",
   "Les verts et jaunes sont humains ; les bleus et rouges sont contrôlés par l'IA." ] ).

% doublon volontaire côté pattern (inutile mais inoffensif vu le cut plus haut)
regle_rep(fin, 1, [fin], [ "Merci de m'avoir consulté." ]).

/* ----------------- Utilitaires d'E/S (non utilisés en web) ----------- */

% Types de caractères
my_char_type(46,period) :- !.
my_char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
my_char_type(X,alphanumeric) :- X >= 97, X =< 122, !.  % 'z' = 122
my_char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
my_char_type(X,whitespace) :- X =< 32, !.
my_char_type(X,punctuation) :- X >= 33, X =< 47, !.
my_char_type(X,punctuation) :- X >= 58, X =< 64, !.
my_char_type(X,punctuation) :- X >= 91, X =< 96, !.
my_char_type(X,punctuation) :- X >= 123, X =< 126, !.
my_char_type(_,special).

/*****************************************************************************/
% lower_case(+C,?L)
%   If ASCII code C is an upper-case letter, then L is the
%   corresponding lower-case letter. Otherwise L=C.
lower_case(X,Y) :-
    X >= 65, X =< 90, Y is X + 32, !.
lower_case(X,X).

/*****************************************************************************/
% read_lc_string(-String)
%  Reads a line of input into String as a list of ASCII codes,
%  with all capital letters changed to lower case.
% Lecture console SWI (non utilisée dans le navigateur)
read_lc_string(String) :-
    get0(FirstChar),
    lower_case(FirstChar,LChar),
    read_lc_string_aux(LChar,String).
read_lc_string_aux(10,[]) :- !.      % end of line
read_lc_string_aux(-1,[]) :- !.      % end of file
read_lc_string_aux(LChar,[LChar|Rest]) :- read_lc_string(Rest).

/*****************************************************************************/
% extract_word(+String,-Rest,-Word) (final version)
%  Extracts the first Word from String; Rest is rest of String.
%  A word is a series of contiguous letters, or a series
%  of contiguous digits, or a single special character.
%  Assumes String does not begin with whitespace.
extract_word([C|Chars],Rest,[C|RestOfWord]) :-
    my_char_type(C,Type),
    extract_word_aux(Type,Chars,Rest,RestOfWord).
extract_word_aux(special,Rest,Rest,[]) :- !.
extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
    my_char_type(C,Type), !,
    extract_word_aux(Type,Chars,Rest,RestOfWord).
extract_word_aux(_,Rest,Rest,[]).

/*****************************************************************************/
% remove_initial_blanks(+X,?Y)
%   Removes whitespace characters from the
%   beginning of string X, giving string Y.
remove_initial_blanks([C|Chars],Result) :-
    my_char_type(C,whitespace), !,
    remove_initial_blanks(Chars,Result).
remove_initial_blanks(X,X). % if previous clause did not succeed.

/*****************************************************************************/
% digit_value(?D,?V)
%  Where D is the ASCII code of a digit,
%  V is the corresponding number.
digit_value(48,0). 
digit_value(49,1). 
digit_value(50,2).
digit_value(51,3). 
digit_value(52,4). 
digit_value(53,5).
digit_value(54,6). 
digit_value(55,7). 
digit_value(56,8).
digit_value(57,9).

/*****************************************************************************/
% string_to_number(+S,-N)
%  Converts string S to the number that it
%  represents, e.g., "234" to 234.
%  Fails if S does not represent a nonnegative integer.
string_to_number(S,N) :- string_to_number_aux(S,0,N).
string_to_number_aux([D|Digits],ValueSoFar,Result) :-
    digit_value(D,V),
    NewValueSoFar is 10*ValueSoFar + V,
    string_to_number_aux(Digits,NewValueSoFar,Result).
string_to_number_aux([],Result,Result).

/*****************************************************************************/
% string_to_atomic(+String,-Atomic)
%  Converts String into the atom or number of
%  which it is the written representation.
string_to_atomic([C|Chars],Number) :-
    string_to_number([C|Chars],Number), !.
string_to_atomic(String,Atom) :- atom_codes(Atom,String).
% assuming previous clause failed.

/*****************************************************************************/
% extract_atomics(+String,-ListOfAtomics) (second version)
%  Breaks String up into ListOfAtomics
%  e.g., " abc def  123 " into [abc,def,123].
extract_atomics(String,ListOfAtomics) :-
    remove_initial_blanks(String,NewString),
    extract_atomics_aux(NewString,ListOfAtomics).
extract_atomics_aux([C|Chars],[A|Atomics]) :-
    extract_word([C|Chars],Rest,Word),
    string_to_atomic(Word,A),
    extract_atomics(Rest,Atomics).
extract_atomics_aux([],[]).

/*****************************************************************************/
% clean_string(+String,-Cleanstring)
%  removes all punctuation characters from String and return Cleanstring
clean_string([C|Chars],L) :-
    my_char_type(C,punctuation),
    clean_string(Chars,L), !.
clean_string([C|Chars],[C|L]) :-
    clean_string(Chars,L), !.
clean_string([C|[]],[]) :-
    my_char_type(C,punctuation), !.
clean_string([C|[]],[C]).

/*****************************************************************************/
% read_atomics(-ListOfAtomics)
%  Reads a line of input, removes all punctuation characters, and converts
%  it into a list of atomic terms, e.g., [this,is,an,example].
read_atomics(ListOfAtomics) :-
    read_lc_string(String),
    clean_string(String,Cleanstring),
    extract_atomics(Cleanstring,ListOfAtomics).

/* ------------------ Formatage (version Web) --------------------------- */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*        PRODUIRE_REPONSE : ecrit la liste de strings                   */
/*                                                                       */
/* --------------------------------------------------------------------- */

transformer_reponse_en_string(Li,Lo) :- flatten_strings_in_sentences(Li,Lo).
transformer_reponse_en_string(Li,Lo) :- flatten_strings_in_sentences(Li,Lo).
flatten_strings_in_sentences([],[]).
flatten_strings_in_sentences([W|T],S) :-
    string_as_list(W,L1),
    flatten_strings_in_sentences(T,L2),
    append(L1,L2,S).

% --- Tau Prolog (Web) ---
string_as_list(W,W).
% --- SWI (console) : commenter la ligne au-dessus et décommenter ci-dessous ---
string_as_list(W,L) :- string_codes(W,L).

/* ------------------ Console SWI (non utilisée en Web) ---------------- */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*        ECRIRE_REPONSE : ecrit une suite de lignes de texte            */
/*                                                                       */
/* --------------------------------------------------------------------- */


ecrire_reponse(L) :-
   nl, write('PBot :'),
   ecrire_ligne(L,1,1,_Mf).

% ecrire_ligne(Li,Mi,Ei,Mf)
% input : Li, liste de mots a ecrire
%         Mi, indique si le premier caractere du premier mot 
%            doit etre mis en majuscule (1 si oui, 0 si non)
%         Ei, indique le nombre d'espaces avant ce premier mot 
% output : Mf, booleen tel que decrit ci-dessus a appliquer 
%          a la ligne suivante, si elle existe

ecrire_ligne([],M,_,M) :- nl.
ecrire_ligne([M|L],Mi,Ei,Mf) :-
   ecrire_mot(M,Mi,Maux,Ei,Eaux),
   ecrire_ligne(L,Maux,Eaux,Mf).

% ecrire_mot(M,B1,B2,E1,E2)
% input : M, le mot a ecrire
%         B1, indique s'il faut une majuscule (1 si oui, 0 si non)
%         E1, indique s'il faut un espace avant le mot (1 si oui, 0 si non)
% output : B2, indique si le mot suivant prend une majuscule
%          E2, indique si le mot suivant doit etre precede d'un espace

ecrire_mot('.',_,1,_,1) :- write('. '), !.
ecrire_mot('\'',X,X,_,0) :- write('\''), !.
ecrire_mot(',',X,X,E,1) :- espace(E), write(','), !.
ecrire_mot(M,0,0,E,1) :- espace(E), write(M).
ecrire_mot(M,1,0,E,1) :-
   name(M,[C|L]), D is C - 32, name(N,[D|L]),
   espace(E), write(N).

espace(0).
espace(N) :- N>0, Nn is N-1, write(' '), espace(Nn).

/* ------------------ Boucle console (non Web) ------------------------- */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                            TEST DE FIN                                */
/*                                                                       */
/* --------------------------------------------------------------------- */

fin(L) :- member(fin,L).

/* --------------------------------------------------------------------- */
/*                     LIRE_QUESTION                                     */
/*  Lit la saisie utilisateur, retire la ponctuation,                    */
/*  et renvoie la liste de mots en minuscules.                           */
/* --------------------------------------------------------------------- */
lire_question(L_Mots) :- read_atomics(L_Mots).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                         BOUCLE PRINCIPALE                             */
/*                                                                       */
/* --------------------------------------------------------------------- */
pontuXL :-
   nl, nl, nl,
   write('Bonjour, je suis PBot, le bot explicateur du jeu PontuXL.'), nl,
   write('En quoi puis-je vous etre utile ?'), nl, nl,
   repeat,
      write('Vous : '), ttyflush,
      lire_question(L_Mots),
      produire_reponse(L_Mots,L_reponse),
      ecrire_reponse(L_reponse), nl,
   fin(L_Mots), !.

/* --------------------------------------------------------------------- */
/*                                                                       */
/*             ACTIVATION DU PROGRAMME APRES COMPILATION                 */
/*                                                                       */
/* --------------------------------------------------------------------- */
% :- pontuXL.
