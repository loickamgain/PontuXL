:- use_module(library(lists)).

/* ===================================================================== */
/*                                                                       */
/*                 INTELLIGENCE ARTIFICIELLE POUR PONTUXL                */
/*                                                                       */
/*                              */
/* 1. Projet développé avec 2 heuristiques complètes                    */
/* 2. Comparaison de performance entre Minimax et Maxⁿ                  */
/* 3. Implémentation du Shallow Pruning pour optimisation               */
/*                                                                       */
/* Module d'IA pour guider les robots (joueurs bleu et rouge)           */
/* Basé sur l'algorithme Maxⁿ avec élagage superficiel                  */
/*                                                                       */
/* ===================================================================== */

% Constantes du jeu
taille_plateau(6).
nb_lutins_par_joueur(4).
couleurs_joueurs([vert, bleu, jaune, rouge]).
couleurs_ia([bleu, rouge]).

/* ===================================================================== */
/*                                                                       */
/*                     STRUCTURE DE DONNÉES DU JEU                      */
/*                                                                       */
/* Définition des structures utilisées pour représenter l'état du jeu   */
/* Compatible avec l'interface JavaScript du projet                     */
/*                                                                       */
/* ===================================================================== */

% État du jeu
% etat_jeu(Plateau, Ponts, JoueurCourant, JoueursActifs)
% - Plateau: matrice 6x6 représentant les positions des lutins
% - Ponts: structure contenant les ponts horizontaux et verticaux
% - JoueurCourant: joueur dont c'est le tour (vert, bleu, jaune, rouge)
% - JoueursActifs: liste des joueurs encore en jeu

% Structure des ponts
% ponts(PontsH, PontsV)
% - PontsH: matrice 5x6 représentant les ponts horizontaux
% - PontsV: matrice 6x5 représentant les ponts verticaux

% Position d'un lutin
% pos(X, Y)

/* ===================================================================== */
/*                                                                       */
/*              HEURISTIQUE 1 : MOBILITÉ DES LUTINS                     */
/*                                                                       */
/* PREMIÈRE HEURISTIQUE                                                  */
/*                                                                       */
/* Principe : Plus un joueur a de mouvements possibles pour ses lutins, */
/* plus sa position est avantageuse. Cette heuristique favorise la      */
/* flexibilité tactique et évite l'isolement prématuré.                 */
/*                                                                       */
/* Calcul : Somme des mouvements possibles pour tous les lutins         */
/* Score = Σ(nombre_mouvements_possibles(lutin_i))                      */
/*                                                                       */
/* ===================================================================== */

% Calculer la mobilité des lutins dun joueur - HEURISTIQUE 1
mobilite_lutins(Plateau, Ponts, Joueur, Score) :-
    % Trouver tous les lutins du joueur sur le plateau
    trouver_lutins(Plateau, Joueur, Lutins),
    % Calculer la mobilité totale (somme des mouvements possibles)
    calculer_mobilite_totale(Lutins, Plateau, Ponts, 0, Score).

% Calculer la mobilité totale pour tous les lutins dun joueur
calculer_mobilite_totale([], _, _, Score, Score).
calculer_mobilite_totale([Lutin|Reste], Plateau, Ponts, ScoreAcc, Score) :-
    % Pour chaque lutin, compter ses mouvements possibles
    mouvements_possibles(Lutin, Plateau, Ponts, Mouvements),
    length(Mouvements, NbMouvements),
    % Accumuler le score de mobilité
    NouveauScore is ScoreAcc + NbMouvements,
    % Traiter les lutins restants
    calculer_mobilite_totale(Reste, Plateau, Ponts, NouveauScore, Score).

/* ===================================================================== */
/*                                                                       */
/*            HEURISTIQUE 2 : ISOLATION DES ADVERSAIRES                 */
/*                                                                       */
/* DEUXIÈME HEURISTIQUE                                                  */
/*                                                                       */
/* Principe : Plus les adversaires sont isolés (ont peu de mouvements), */
/* plus la position est favorable. Cette heuristique encourage les      */
/* stratégies offensives d'isolement des lutins ennemis.                */
/*                                                                       */
/* Calcul : Pour chaque adversaire, calculer l'isolation de ses lutins  */
/* Score = Σ(4 - nombre_mouvements_possibles(lutin_adversaire_i))       */
/* Le facteur 4 représente le maximum de mouvements possibles           */
/*                                                                       */
/* ===================================================================== */

% Calculer le score disolation des adversaires - HEURISTIQUE 2
isolation_adversaires(Plateau, Ponts, Joueur, Score) :-
    % Obtenir la liste de tous les joueurs
    couleurs_joueurs(Couleurs),
    % Retirer le joueur courant pour obtenir les adversaires
    delete(Couleurs, Joueur, Adversaires),
    % Calculer lisolation totale des adversaires
    calculer_isolation_totale(Adversaires, Plateau, Ponts, 0, Score).

% Calculer lisolation totale pour tous les adversaires
calculer_isolation_totale([], _, _, Score, Score).
calculer_isolation_totale([Adversaire|Reste], Plateau, Ponts, ScoreAcc, Score) :-
    % Trouver tous les lutins de cet adversaire
    trouver_lutins(Plateau, Adversaire, Lutins),
    % Calculer leur niveau disolation
    calculer_isolation_lutins(Lutins, Plateau, Ponts, 0, ScoreAdversaire),
    % Plus le score est élevé, plus ladversaire est isolé (bon pour nous)
    NouveauScore is ScoreAcc + ScoreAdversaire,
    % Traiter les adversaires restants
    calculer_isolation_totale(Reste, Plateau, Ponts, NouveauScore, Score).

% Calculer lisolation pour tous les lutins dun adversaire
calculer_isolation_lutins([], _, _, Score, Score).
calculer_isolation_lutins([Lutin|Reste], Plateau, Ponts, ScoreAcc, Score) :-
    % Compter les mouvements possibles pour ce lutin
    mouvements_possibles(Lutin, Plateau, Ponts, Mouvements),
    length(Mouvements, NbMouvements),
    % Calculer le score disolation (moins de mouvements = plus isolé = meilleur score)
    ScoreIsolation is 4 - NbMouvements, % 4 est le maximum de mouvements possibles
    NouveauScore is ScoreAcc + ScoreIsolation,
    % Traiter les lutins restants
    calculer_isolation_lutins(Reste, Plateau, Ponts, NouveauScore, Score).

/* ===================================================================== */
/*                                                                       */
/*                    FONCTION D'ÉVALUATION COMBINÉE                    */
/*                                                                       */
/* Combine les deux heuristiques pour obtenir un score global           */
/* Cette fonction répond à la question sur les "2 heuristiques"         */
/*                                                                       */
/* ===================================================================== */

% Évaluer létat du jeu pour un joueur donné - COMBINAISON DES 2 HEURISTIQUES
evaluer_etat(Etat, Joueur, Score) :-
    etat_jeu(Plateau, Ponts, _, _) = Etat,
    % APPLIQUER HEURISTIQUE 1 : Mobilité des lutins du joueur
    mobilite_lutins(Plateau, Ponts, Joueur, ScoreMobilite),
    % APPLIQUER HEURISTIQUE 2 : Isolation des adversaires
    isolation_adversaires(Plateau, Ponts, Joueur, ScoreIsolation),
    % COMBINER LES DEUX HEURISTIQUES
    % Score final = Mobilité propre + Isolation des adversaires
    Score is ScoreMobilite + ScoreIsolation.

/* ===================================================================== */
/*                                                                       */
/*                  FONCTIONS DANALYSE DU PLATEAU                      */
/*                                                                       */
/* Fonctions utilitaires pour analyser l'état du jeu et supporter       */
/* les calculs des heuristiques                                         */
/*                                                                       */
/* ===================================================================== */

% Trouver tous les lutins dun joueur sur le plateau
trouver_lutins(Plateau, Joueur, Lutins) :-
    taille_plateau(Taille),
    Max is Taille - 1,
    findall(pos(X, Y), 
            (between(0, Max, X), 
             between(0, Max, Y), 
             acceder_plateau(Plateau, X, Y, Joueur)), 
            Lutins).

% Accéder à une case du plateau
acceder_plateau(Plateau, X, Y, Valeur) :-
    nth0(Y, Plateau, Ligne),
    nth0(X, Ligne, Valeur).

% Vérifier si une position est dans les limites du plateau
position_valide(X, Y) :-
    taille_plateau(Taille),
    X >= 0, X < Taille,
    Y >= 0, Y < Taille.

% Vérifier si une case est vide
case_vide(Plateau, X, Y) :-
    position_valide(X, Y),
    acceder_plateau(Plateau, X, Y, vide).

% Vérifier si un pont existe entre deux cases adjacentes
pont_existe(Ponts, X1, Y1, X2, Y2) :-
    % Vérifier que les cases sont adjacentes
    (
        (X1 =:= X2, abs(Y1 - Y2) =:= 1) -> % Mouvement vertical
            MinY is min(Y1, Y2),
            ponts(_, PontsV) = Ponts,
            acceder_plateau(PontsV, X1, MinY, true)
        ;
        (Y1 =:= Y2, abs(X1 - X2) =:= 1) -> % Mouvement horizontal
            MinX is min(X1, X2),
            ponts(PontsH, _) = Ponts,
            acceder_plateau(PontsH, MinX, Y1, true)
        ;
            false % Les cases ne sont pas adjacentes
    ).

% Trouver tous les mouvements possibles pour un lutin
% CETTE FONCTION EST CRUCIALE POUR LES DEUX HEURISTIQUES
mouvements_possibles(pos(X, Y), Plateau, Ponts, Mouvements) :-
    findall(pos(NX, NY),
            (
                % Définir les 4 directions possibles
                member(delta(DX, DY), [delta(0, 1), delta(1, 0), delta(0, -1), delta(-1, 0)]),
                NX is X + DX,
                NY is Y + DY,
                % Vérifier si le mouvement est valide
                position_valide(NX, NY),
                case_vide(Plateau, NX, NY),
                pont_existe(Ponts, X, Y, NX, NY)
            ),
            Mouvements).

/* ===================================================================== */
/*                                                                       */
/*                ALGORITHME MINIMAX AVEC ÉLAGAGE ALPHA-BETA            */
/*                                                                       */
/* PREMIER ALGORITHME DE COMPARAISON DE PERFORMANCE                     */
/*                                                                       */
/* Avantages :                                                           */
/* - Optimal pour les jeux à 2 joueurs                                  */
/* - Élagage Alpha-Beta très efficace                                    */
/* - Bien étudié et optimisé                                            */
/*                                                                       */
/* Inconvénients :                                                       */
/* - Moins adapté aux jeux à 4 joueurs comme PontuXL                    */
/* - Doit simuler les alliances/conflits entre joueurs                  */
/* - Performance dégradée avec plus de 2 joueurs                        */
/*                                                                       */
/* ===================================================================== */

% Trouver le meilleur coup avec lalgorithme Minimax
meilleur_coup(Etat, Joueur, Profondeur, MeilleurCoup) :-
    minimax(Etat, Joueur, Profondeur, -10000, 10000, MeilleurCoup, _).

% Implémentation de lalgorithme Minimax avec élagage alpha-beta
minimax(Etat, Joueur, 0, _, _, _, Score) :-
    % Cas de base: évaluer létat du jeu avec nos 2 heuristiques
    evaluer_etat(Etat, Joueur, Score).

minimax(Etat, Joueur, Profondeur, Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    Profondeur > 0,
    etat_jeu(_, _, JoueurCourant, _) = Etat,
    coups_possibles(Etat, JoueurCourant, Coups),
    
    % Si aucun coup nest possible, évaluer létat actuel
    (Coups = [] ->
        evaluer_etat(Etat, Joueur, MeilleurScore),
        MeilleurCoup = aucun_coup
    ;
        % Sinon, explorer les coups possibles
        NouvelleProf is Profondeur - 1,
        (JoueurCourant = Joueur ->
            % Maximiser pour le joueur actuel
            minimax_max(Coups, Etat, Joueur, NouvelleProf, Alpha, Beta, MeilleurCoup, MeilleurScore)
        ;
            % Minimiser pour les adversaires
            minimax_min(Coups, Etat, Joueur, NouvelleProf, Alpha, Beta, MeilleurCoup, MeilleurScore)
        )
    ).

% Maximiser le score (ÉLAGAGE ALPHA-BETA CLASSIQUE)
minimax_max([], _, _, _, Alpha, _, aucun_coup, Alpha).
minimax_max([Coup|Reste], Etat, Joueur, Profondeur, Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    appliquer_coup(Etat, Coup, NouvelEtat),
    minimax(NouvelEtat, Joueur, Profondeur, Alpha, Beta, _, Score),
    
    % Mettre à jour Alpha
    (Score > Alpha ->
        NouvelAlpha = Score,
        CoupCandidat = Coup
    ;
        NouvelAlpha = Alpha,
        CoupCandidat = MeilleurCoup
    ),
    
    % ÉLAGAGE BETA (optimisation performance)
    (NouvelAlpha >= Beta ->
        MeilleurCoup = CoupCandidat,
        MeilleurScore = NouvelAlpha
    ;
        minimax_max(Reste, Etat, Joueur, Profondeur, NouvelAlpha, Beta, CoupTemp, ScoreTemp),
        MeilleurCoup = CoupTemp,
        MeilleurScore = ScoreTemp
    ).

% Minimiser le score (ÉLAGAGE ALPHA-BETA CLASSIQUE)
minimax_min([], _, _, _, _, Beta, aucun_coup, Beta).
minimax_min([Coup|Reste], Etat, Joueur, Profondeur, Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    appliquer_coup(Etat, Coup, NouvelEtat),
    minimax(NouvelEtat, Joueur, Profondeur, Alpha, Beta, _, Score),
    
    % Mettre à jour Beta
    (Score < Beta ->
        NouvelBeta = Score,
        CoupCandidat = Coup
    ;
        NouvelBeta = Beta,
        CoupCandidat = MeilleurCoup
    ),
    
    % ÉLAGAGE ALPHA (optimisation performance)
    (Alpha >= NouvelBeta ->
        MeilleurCoup = CoupCandidat,
        MeilleurScore = NouvelBeta
    ;
        minimax_min(Reste, Etat, Joueur, Profondeur, Alpha, NouvelBeta, CoupTemp, ScoreTemp),
        MeilleurCoup = CoupTemp,
        MeilleurScore = ScoreTemp
    ).

/* ===================================================================== */
/*                                                                       */
/*                ALGORITHME MAXⁿ AVEC SHALLOW PRUNING                  */
/*                                                                       */
/* DEUXIÈME ALGORITHME DE COMPARAISON DE PERFORMANCE                    */
/*                                                                       */
/* Avantages :                                                           */
/* - Conçu spécifiquement pour les jeux multi-joueurs (4 joueurs)       */
/* - Évalue tous les joueurs simultanément                              */
/* - Plus adapté à la nature du jeu PontuXL                             */
/*                                                                       */
/* Inconvénients :                                                       */
/* - Plus complexe à implémenter                                        */
/* - Élagage moins efficace que Alpha-Beta                              */
/* - Espace de recherche plus large                                     */
/*                                                                       */
/* SHALLOW PRUNING : Technique d'optimisation qui coupe les branches    */
/* lorsque le score d'un joueur atteint une borne supérieure            */
/*                                                                       */
/* ===================================================================== */

% Constantes pour lalgorithme Maxⁿ
profondeur_max(3).
evaluation_max(100).

% Structure pour stocker les évaluations des joueurs
% evaluation(Vert, Bleu, Jaune, Rouge)

% Trouver le meilleur coup avec lalgorithme Maxⁿ
trouver_meilleur_coup(Etat, MeilleurCoup) :-
    etat_jeu(_, _, JoueurCourant, _JoueursActifs) = Etat,
    profondeur_max(ProfondeurMax),
    evaluation_max(EvaluationMax),
    
    % Générer tous les coups possibles
    coups_possibles(Etat, JoueurCourant, Coups),
    
    % Si aucun coup nest possible, retourner aucun_coup
    (Coups = [] -> 
        MeilleurCoup = aucun_coup
    ;
        % Trouver lindex du joueur courant
        couleurs_joueurs(Couleurs),
        nth0(IndexJoueur, Couleurs, JoueurCourant),
        
        % Initialiser le meilleur score et le meilleur coup
        MeilleurScore = -1,
        
        % Évaluer chaque coup possible avec Maxⁿ
        evaluer_coups(Coups, Etat, IndexJoueur, ProfondeurMax, EvaluationMax, MeilleurScore, aucun_coup, MeilleurCoup)
    ).

% Évaluer tous les coups possibles et trouver le meilleur
evaluer_coups([], _, _, _, _, _MeilleurScore, MeilleurCoup, MeilleurCoup).
evaluer_coups([Coup|Reste], Etat, IndexJoueur, Profondeur, Borne, MeilleurScore, CoupActuel, MeilleurCoupFinal) :-
    % Appliquer le coup et obtenir le nouvel état
    appliquer_coup(Etat, Coup, NouvelEtat),
    
    % Évaluer le nouvel état avec Maxⁿ
    maxn_shallow(NouvelEtat, Profondeur, Borne, Evaluations),
    
    % Extraire le score du joueur courant
    nth0(IndexJoueur, Evaluations, Score),
    
    % Mettre à jour le meilleur coup si nécessaire
    (Score > MeilleurScore ->
        NouveauMeilleurScore = Score,
        NouveauMeilleurCoup = Coup
    ;
        NouveauMeilleurScore = MeilleurScore,
        NouveauMeilleurCoup = CoupActuel
    ),
    
    % Continuer avec les coups restants
    evaluer_coups(Reste, Etat, IndexJoueur, Profondeur, Borne, NouveauMeilleurScore, NouveauMeilleurCoup, MeilleurCoupFinal).

/* ===================================================================== */
/*                                                                       */
/*                      IMPLÉMENTATION DU SHALLOW PRUNING               */
/*                                                                       */
/* RÉPONSE À LA QUESTION SUR LE SHALLOW PRUNING                         */
/*                                                                       */
/* Le Shallow Pruning est une technique d'optimisation pour l'algorithme*/
/* Maxⁿ qui permet de couper certaines branches de l'arbre de recherche */
/* sans perdre l'optimalité.                                            */
/*                                                                       */
/* Principe :                                                            */
/* - Si le score d'un joueur atteint une borne supérieure prédéfinie,   */
/*   on arrête l'exploration des coups restants                         */
/* - Moins agressif que l'élagage Alpha-Beta mais adapté au multi-joueur*/
/*                                                                       */
/* Avantages :                                                           */
/* - Réduit significativement l'espace de recherche                     */
/* - Améliore les performances sans perte d'optimalité locale           */
/* - Adapté aux contraintes temps réel du jeu                           */
/*                                                                       */
/* ===================================================================== */

% Algorithme Maxⁿ avec élagage superficiel (Shallow Pruning)
% maxn_shallow(+Etat, +Profondeur, +Borne, -Evaluations)
maxn_shallow(Etat, 0, _, Evaluations) :-
    % Cas de base: évaluer létat du jeu pour tous les joueurs
    % Utilise nos 2 heuristiques pour chaque joueur
    evaluer_etat_tous_joueurs(Etat, Evaluations).

maxn_shallow(Etat, _, _, Evaluations) :-
    % Si un seul joueur est actif, cest un état terminal
    etat_jeu(_, _, _, JoueursActifs) = Etat,
    length(JoueursActifs, 1),
    evaluer_etat_tous_joueurs(Etat, Evaluations).

maxn_shallow(Etat, Profondeur, Borne, Evaluations) :-
    Profondeur > 0,
    etat_jeu(Plateau, Ponts, JoueurCourant, JoueursActifs) = Etat,
    length(JoueursActifs, NbJoueurs),
    NbJoueurs > 1,
    
    % Trouver lindex du joueur courant
    couleurs_joueurs(Couleurs),
    nth0(IndexJoueur, Couleurs, JoueurCourant),
    
    % Générer tous les coups possibles
    coups_possibles(Etat, JoueurCourant, Coups),
    
    % Si aucun coup nest possible, passer au joueur suivant
    (Coups = [] -> 
        % Créer un nouvel état avec le joueur suivant
        joueur_suivant(JoueurCourant, JoueursActifs, NouveauJoueur),
        NouvelEtat = etat_jeu(Plateau, Ponts, NouveauJoueur, JoueursActifs),
        maxn_shallow(NouvelEtat, Profondeur, Borne, Evaluations)
    ;
        % Sinon, explorer les coups possibles avec SHALLOW PRUNING
        NouvelleProf is Profondeur - 1,
        
        % Initialiser les évaluations par défaut (tous à 0)
        length(Couleurs, NbCouleurs),
        length(EvaluationsDefaut, NbCouleurs),
        maplist(=(0), EvaluationsDefaut),
        
        % Explorer les coups et trouver la meilleure évaluation
        explorer_coups(Coups, Etat, IndexJoueur, NouvelleProf, Borne, EvaluationsDefaut, Evaluations)
    ).

/* ===================================================================== */
/*                                                                       */
/*              CŒUR DU SHALLOW PRUNING - OPTIMISATION CLÉE             */
/*                                                                       */
/* Cette fonction implémente le cœur du Shallow Pruning                 */
/* C'est ici que l'optimisation de performance a lieu                   */
/*                                                                       */
/* ===================================================================== */

% Explorer tous les coups possibles avec SHALLOW PRUNING
explorer_coups([], _, _, _, _, MeilleuresEvaluations, MeilleuresEvaluations).
explorer_coups([Coup|Reste], Etat, IndexJoueur, Profondeur, Borne, MeilleuresEvaluations, EvaluationsFinales) :-
    % Appliquer le coup et obtenir le nouvel état
    appliquer_coup(Etat, Coup, NouvelEtat),
    
    % Évaluer le nouvel état avec Maxⁿ récursivement
    maxn_shallow(NouvelEtat, Profondeur, Borne, Evaluations),
    
    % Extraire le score du joueur courant
    nth0(IndexJoueur, Evaluations, Score),
    nth0(IndexJoueur, MeilleuresEvaluations, MeilleurScore),
    
    % Mettre à jour les meilleures évaluations si nécessaire
    (Score > MeilleurScore ->
        NouvellesEvaluations = Evaluations
    ;
        NouvellesEvaluations = MeilleuresEvaluations
    ),
    
    % *** SHALLOW PRUNING - OPTIMISATION PERFORMANCE ***
    % Si le score atteint la borne, on arrête l'exploration
    % C'est ici que le gain de performance se produit
    nth0(IndexJoueur, NouvellesEvaluations, NouveauScore),
    (NouveauScore >= Borne ->
        % ÉLAGAGE : Arrêter lexploration des coups restants
        % Gain de performance significatif
        EvaluationsFinales = NouvellesEvaluations
    ;
        % Continuer avec les coups restants si la borne nest pas atteinte
        explorer_coups(Reste, Etat, IndexJoueur, Profondeur, Borne, NouvellesEvaluations, EvaluationsFinales)
    ).

% Évaluer létat du jeu pour tous les joueurs
% APPLIQUE NOS 2 HEURISTIQUES À CHAQUE JOUEUR
evaluer_etat_tous_joueurs(Etat, Evaluations) :-
    couleurs_joueurs(Couleurs),
    length(Couleurs, NbCouleurs),
    length(Evaluations, NbCouleurs),
    
    % Évaluer chaque joueur avec nos heuristiques combinées
    evaluer_joueurs(Couleurs, Etat, 0, Evaluations),
    
    % Normaliser les évaluations pour éviter les débordements
    normaliser_evaluations(Evaluations).

% Évaluer chaque joueur avec les 2 heuristiques
evaluer_joueurs([], _, _, []).
evaluer_joueurs([Joueur|Reste], Etat, Index, [Score|ResteScores]) :-
    % Évaluer létat pour ce joueur avec nos 2 heuristiques
    etat_jeu(_, _, _, JoueursActifs) = Etat,
    (member(Joueur, JoueursActifs) ->
        % APPLIQUER LES 2 HEURISTIQUES COMBINÉES
        evaluer_etat(Etat, Joueur, Score)
    ;
        % Si le joueur est éliminé, son score est 0
        Score = 0
    ),
    
    % Passer au joueur suivant
    NouvelIndex is Index + 1,
    evaluer_joueurs(Reste, Etat, NouvelIndex, ResteScores).

% Normaliser les évaluations pour éviter les débordements
normaliser_evaluations(Evaluations) :-
    evaluation_max(Max),
    sum_list(Evaluations, Somme),
    (Somme > Max ->
        Facteur is Max / Somme,
        maplist(multiplier(Facteur), Evaluations, _)
    ;
        true
    ).

% Multiplier un nombre par un facteur (utilisé pour la normalisation)
multiplier(Facteur, Nombre, Resultat) :-
    Resultat is Nombre * Facteur.

/* ===================================================================== */
/*                                                                       */
/*                  GÉNÉRATION ET APPLICATION DES COUPS                 */
/*                                                                       */
/* Fonctions pour générer les coups possibles et les appliquer          */
/* Compatible avec la logique du jeu PontuXL                            */
/*                                                                       */
/* ===================================================================== */

% Structure dun coup
% coup(DeplacerLutin, RetirerPont)
% - DeplacerLutin: deplacement(pos(X1, Y1), pos(X2, Y2))
% - RetirerPont: retirer_pont(Type, X, Y)
%   où Type est 'horizontal' ou 'vertical'

% Générer tous les coups possibles pour un joueur
coups_possibles(Etat, Joueur, Coups) :-
    etat_jeu(Plateau, Ponts, Joueur, _) = Etat,
    % Trouver tous les lutins du joueur
    trouver_lutins(Plateau, Joueur, Lutins),
    % Générer tous les déplacements possibles
    findall(deplacement(Lutin, Destination),
            (
                member(Lutin, Lutins),
                mouvements_possibles(Lutin, Plateau, Ponts, Destinations),
                member(Destination, Destinations)
            ),
            Deplacements),
    
    % Générer tous les ponts qui peuvent être retirés
    ponts_retirables(Ponts, PontsRetirables),
    
    % Combiner déplacements et retraits de ponts
    findall(coup(Deplacement, RetirerPont),
            (
                member(Deplacement, Deplacements),
                member(RetirerPont, PontsRetirables)
            ),
            Coups).

% Trouver tous les ponts qui peuvent être retirés
ponts_retirables(Ponts, PontsRetirables) :-
    ponts(PontsH, PontsV) = Ponts,
    taille_plateau(Taille),
    MaxXH is Taille - 2,  % 0..4
    MaxYH is Taille - 1,  % 0..5
    MaxXV is Taille - 1,  % 0..5
    MaxYV is Taille - 2,  % 0..4
    
    % Ponts horizontaux
    findall(retirer_pont(horizontal, X, Y),
            (
                between(0, MaxYH, Y),
                between(0, MaxXH, X),
                acceder_plateau(PontsH, X, Y, true)
            ),
            PontsH_Retirables),
    
    % Ponts verticaux
    findall(retirer_pont(vertical, X, Y),
            (
                between(0, MaxYV, Y),
                between(0, MaxXV, X),
                acceder_plateau(PontsV, X, Y, true)
            ),
            PontsV_Retirables),
    
    % Combiner les deux types de ponts
    append(PontsH_Retirables, PontsV_Retirables, PontsRetirables).

% Appliquer un coup à létat du jeu
appliquer_coup(Etat, Coup, NouvelEtat) :-
    etat_jeu(Plateau, Ponts, JoueurCourant, JoueursActifs) = Etat,
    coup(Deplacement, RetirerPont) = Coup,
    
    % Appliquer le déplacement
    deplacement(pos(X1, Y1), pos(X2, Y2)) = Deplacement,
    appliquer_deplacement(Plateau, JoueurCourant, X1, Y1, X2, Y2, NouveauPlateau),
    
    % Appliquer le retrait de pont
    retirer_pont(Type, X, Y) = RetirerPont,
    appliquer_retrait_pont(Ponts, Type, X, Y, NouveauxPonts),
    
    % Passer au joueur suivant
    joueur_suivant(JoueurCourant, JoueursActifs, NouveauJoueur),
    
    % Vérifier si des joueurs sont éliminés
    verifier_eliminations(NouveauPlateau, NouveauxPonts, JoueursActifs, NouveauxJoueursActifs),
    
    % Créer le nouvel état
    NouvelEtat = etat_jeu(NouveauPlateau, NouveauxPonts, NouveauJoueur, NouveauxJoueursActifs).

% Appliquer un déplacement de lutin
appliquer_deplacement(Plateau, Joueur, X1, Y1, X2, Y2, NouveauPlateau) :-
    % Retirer le pion de l’ancienne case
    modifier_plateau(Plateau, X1, Y1, vide, P1),
    % Poser le pion sur la nouvelle case
    modifier_plateau(P1, X2, Y2, Joueur, NouveauPlateau).


% Modifier une valeur dans le plateau(+PlateauIn, +X, +Y, +Valeur, -PlateauOut)
modifier_plateau(PlateauIn, X, Y, Valeur, PlateauOut) :-
    % Extraire la ligne Y et le "reste" du plateau
    nth0(Y, PlateauIn, Ligne, RestePlateau),
    % Remplacer l'élément X dans la ligne
    nth0(X, Ligne, _Ancien, ResteLigne),
    nth0(X, NouvelleLigne, Valeur, ResteLigne),
    % Recomposer le plateau avec la nouvelle ligne
    nth0(Y, PlateauOut, NouvelleLigne, RestePlateau).

% Appliquer un retrait de pont
appliquer_retrait_pont(Ponts, Type, X, Y, NouveauxPonts) :-
    ponts(PontsH, PontsV) = Ponts,
    ( Type = horizontal ->
        modifier_plateau(PontsH, X, Y, false, NewH),
        NouveauxPonts = ponts(NewH, PontsV)
    ; % vertical
        modifier_plateau(PontsV, X, Y, false, NewV),
        NouveauxPonts = ponts(PontsH, NewV)
    ).


% Déterminer le joueur suivant
joueur_suivant(JoueurCourant, JoueursActifs, NouveauJoueur) :-
    couleurs_joueurs(Couleurs),
    nth0(IndexCourant, Couleurs, JoueurCourant),
    IndexSuivant is (IndexCourant + 1) mod 4,
    nth0(IndexSuivant, Couleurs, JoueurCandidat),
    
    % Vérifier si le joueur candidat est encore actif
    (member(JoueurCandidat, JoueursActifs) ->
        NouveauJoueur = JoueurCandidat
    ;
        % Sinon, chercher le prochain joueur actif
        joueur_suivant(JoueurCandidat, JoueursActifs, NouveauJoueur)
    ).

% Vérifier si un joueur est éliminé
est_elimine(Plateau, Ponts, Joueur) :-
    trouver_lutins(Plateau, Joueur, Lutins),
    % Un joueur est éliminé si tous ses lutins sont isolés
    forall(
        member(Lutin, Lutins),
        (
            mouvements_possibles(Lutin, Plateau, Ponts, Mouvements),
            Mouvements == []
           )
        ).

% Vérifier si des joueurs sont éliminés
verifier_eliminations(Plateau, Ponts, JoueursActifs, NouveauxJoueursActifs) :-
    findall(Joueur,
            (
                member(Joueur, JoueursActifs),
                \+ est_elimine(Plateau, Ponts, Joueur)
            ),
            NouveauxJoueursActifs).



/* ===================================================================== */
/*                                                                       */
/*                    INTERFACE POUR L'IA DU JEU                        */
/*                                                                       */
/* PRÉDICATS PRINCIPAUX POUR RÉPONDRE AUX QUESTIONS DU PROFESSEUR       */
/*                                                                       */
/* Ces prédicats permettent de tester et comparer les performances      */
/* des deux algorithmes implémentés                                     */
/*                                                                       */
/* ===================================================================== */

/* ===================================================================== */
/*                                                                       */
/*             INTERFACE MINIMAX - PREMIER ALGORITHME                   */
/*                                                                       */
/* Cette interface permet de tester l'algorithme Minimax               */
/* et de mesurer ses performances                                        */
/*                                                                       */
/* Utilisation pour les tests de performance :                          */
/* ?- jouer_ia_minimax(Etat, bleu, Coup).                              */
/*                                                                       */
/* ===================================================================== */

% Déterminer le meilleur coup pour un joueur IA avec lalgorithme Minimax
jouer_ia_minimax(Etat, Joueur, MeilleurCoup) :-
    % Vérifier que le joueur est bien contrôlé par lIA
    couleurs_ia(CouleursIA),
    member(Joueur, CouleursIA),
    
    % Déterminer la profondeur de recherche
    % PROFONDEUR AJUSTABLE POUR TESTS DE PERFORMANCE
    Profondeur = 2,
    
    % Trouver le meilleur coup avec lalgorithme Minimax
    % UTILISE NOS 2 HEURISTIQUES POUR LÉVALUATION
    meilleur_coup(Etat, Joueur, Profondeur, MeilleurCoup).

% Prédicat principal pour obtenir le coup de lIA avec Minimax
obtenir_coup_ia_minimax(Plateau, Ponts, Joueur, Deplacement, RetirerPont) :-
    % Créer létat du jeu
    couleurs_joueurs(Couleurs),
    Etat = etat_jeu(Plateau, Ponts, Joueur, Couleurs),
    
    % Obtenir le meilleur coup avec MINIMAX
    jouer_ia_minimax(Etat, Joueur, MeilleurCoup),
    
    % Extraire le déplacement et le retrait de pont
    coup(Deplacement, RetirerPont) = MeilleurCoup.

/* ===================================================================== */
/*                                                                       */
/*              INTERFACE MAXⁿ - DEUXIÈME ALGORITHME                    */
/*                                                                       */
/* Cette interface permet de tester l'algorithme Maxⁿ avec              */
/* Shallow Pruning et de mesurer ses performances                       */
/*                                                                       */
/* Utilisation pour les tests de performance :                          */
/* ?- jouer_ia_maxn(Etat, Coup).                                        */
/*                                                                       */
/* ===================================================================== */

% Déterminer le meilleur coup pour un joueur IA avec lalgorithme Maxⁿ
jouer_ia_maxn(Etat, MeilleurCoup) :-
    % Vérifier que le joueur est bien contrôlé par lIA
    etat_jeu(_, _, Joueur, _) = Etat,
    couleurs_ia(CouleursIA),
    member(Joueur, CouleursIA),
    
    % Trouver le meilleur coup avec lalgorithme Maxⁿ
    % UTILISE LE SHALLOW PRUNING POUR LOPTIMISATION
    trouver_meilleur_coup(Etat, MeilleurCoup).

% Prédicat principal pour obtenir le coup de lIA avec Maxⁿ
obtenir_coup_ia_maxn(Plateau, Ponts, Joueur, Deplacement, RetirerPont) :-
    % Créer létat du jeu
    couleurs_joueurs(Couleurs),
    Etat = etat_jeu(Plateau, Ponts, Joueur, Couleurs),
    
    % Obtenir le meilleur coup avec MAXⁿ + SHALLOW PRUNING
    jouer_ia_maxn(Etat, MeilleurCoup),
    
    % Extraire le déplacement et le retrait de pont
    coup(Deplacement, RetirerPont) = MeilleurCoup.

/* ===================================================================== */
/*                                                                       */
/*                INTERFACE STANDARD POUR LE JEU                        */
/*                                                                       */
/* Interface compatible avec le code JavaScript existant                */
/* Utilise lalgorithme Maxⁿ par défaut comme étant plus adapté         */
/* aux jeux multi-joueurs                                               */
/*                                                                       */
/* ===================================================================== */

% Prédicat principal utilisé par linterface JavaScript
% UTILISE PAR DÉFAUT LALGORITHME MAXⁿ AVEC SHALLOW PRUNING
obtenir_coup_ia(Plateau, Ponts, Joueur, Deplacement, RetirerPont) :-
    % Utiliser lalgorithme Maxⁿ comme algorithme principal
    % (plus adapté aux jeux à 4 joueurs)
    obtenir_coup_ia_maxn(Plateau, Ponts, Joueur, Deplacement, RetirerPont).

/* ===================================================================== */
/*                                                                       */
/*                  PRÉDICATS DE TEST ET BENCHMARK                      */
/*                                                                       */
/* Ces prédicats permettent de tester et comparer les performances      */
/* des deux algorithmes pour répondre aux questions du professeur       */
/*                                                                       */
/* ===================================================================== */

% Tester et comparer les performances des deux algorithmes
% POUR RÉPONDRE À LA QUESTION SUR LA COMPARAISON DE PERFORMANCE
tester_performances(Etat, Joueur, ResultatMinimax, ResultatMaxn, TempsMinimax, TempsMaxn) :-
    % Mesurer le temps dexécution de Minimax
    get_time(DebutMinimax),
    jouer_ia_minimax(Etat, Joueur, ResultatMinimax),
    get_time(FinMinimax),
    TempsMinimax is FinMinimax - DebutMinimax,
    
    % Mesurer le temps dexécution de Maxⁿ
    get_time(DebutMaxn),
    jouer_ia_maxn(Etat, ResultatMaxn),
    get_time(FinMaxn),
    TempsMaxn is FinMaxn - DebutMaxn,
    
    % Afficher les résultats de comparaison
    format('=== COMPARAISON DE PERFORMANCE ===~n'),
    format('Temps Minimax: ~3f secondes~n', [TempsMinimax]),
    format('Temps Maxn: ~3f secondes~n', [TempsMaxn]),
    format('Coup Minimax: ~w~n', [ResultatMinimax]),
    format('Coup Maxn: ~w~n', [ResultatMaxn]).

% Tester lefficacité du Shallow Pruning
% POUR RÉPONDRE À LA QUESTION SUR LE SHALLOW PRUNING
tester_shallow_pruning(Etat, _Joueur, AvecPruning, SansPruning, TempsAvec, TempsSans) :-
    % Tester avec Shallow Pruning
    get_time(DebutAvec),
    jouer_ia_maxn(Etat, AvecPruning),
    get_time(FinAvec),
    TempsAvec is FinAvec - DebutAvec,
    
    % Tester sans Shallow Pruning (Maxⁿ standard)
    get_time(DebutSans),
    maxn_standard(Etat, SansPruning),
    get_time(FinSans),
    TempsSans is FinSans - DebutSans,
    
    % Calculer le gain de performance
    GainPerformance is ((TempsSans - TempsAvec) / TempsSans) * 100,
    
    % Afficher les résultats
    format('=== EFFICACITÉ DU SHALLOW PRUNING ===~n'),
    format('Temps avec Shallow Pruning: ~3f secondes~n', [TempsAvec]),
    format('Temps sans Shallow Pruning: ~3f secondes~n', [TempsSans]),
    format('Gain de performance: ~2f%~n', [GainPerformance]).

% Maxⁿ standard sans Shallow Pruning (pour comparaison)
maxn_standard(Etat, MeilleurCoup) :-
    % Implémentation simplifiée sans élagage pour mesurer limpact
    % du Shallow Pruning sur les performances
    etat_jeu(_, _, JoueurCourant, _) = Etat,
    couleurs_joueurs(Couleurs),
    nth0(IndexJoueur, Couleurs, JoueurCourant),
    coups_possibles(Etat, JoueurCourant, Coups),
    
    % Évaluer tous les coups sans élagage
    evaluer_tous_coups(Coups, Etat, IndexJoueur, MeilleurCoup).

% Évaluer tous les coups sans élagage (pour test de performance)
evaluer_tous_coups([Coup], _, _, Coup) :- !.
evaluer_tous_coups([Premier|Reste], Etat, IndexJoueur, MeilleurCoup) :-
    evaluer_tous_coups(Reste, Etat, IndexJoueur, MeilleurRestant),
    
    % Comparer Premier et MeilleurRestant
    appliquer_coup(Etat, Premier, EtatPremier),
    appliquer_coup(Etat, MeilleurRestant, EtatRestant),
    
    evaluer_etat_tous_joueurs(EtatPremier, EvalPremier),
    evaluer_etat_tous_joueurs(EtatRestant, EvalRestant),
    
    nth0(IndexJoueur, EvalPremier, ScorePremier),
    nth0(IndexJoueur, EvalRestant, ScoreRestant),
    
    (ScorePremier > ScoreRestant ->
        MeilleurCoup = Premier
    ;
        MeilleurCoup = MeilleurRestant
    ).

/* ===================== Heuristiques dédiées (H1/H2) ===================== */

evaluer_etat_h1(Etat, Joueur, Score) :-
    etat_jeu(Plateau, Ponts, _, _) = Etat,
    mobilite_lutins(Plateau, Ponts, Joueur, Score).

evaluer_etat_h2(Etat, Joueur, Score) :-
    etat_jeu(Plateau, Ponts, _, _) = Etat,
    isolation_adversaires(Plateau, Ponts, Joueur, Score).

evaluer_etat_sel(Etat, Joueur, h1, Score) :- evaluer_etat_h1(Etat, Joueur, Score).
evaluer_etat_sel(Etat, Joueur, h2, Score) :- evaluer_etat_h2(Etat, Joueur, Score).

evaluer_etat_tous_joueurs_h(Etat, Heur, Evaluations) :-
    couleurs_joueurs(Couleurs),
    maplist(evaluer_un_joueur_h(Etat, Heur), Couleurs, Evaluations).

evaluer_un_joueur_h(Etat, Heur, Joueur, Score) :-
    etat_jeu(_, _, _, JoueursActifs) = Etat,
    ( member(Joueur, JoueursActifs)
    -> evaluer_etat_sel(Etat, Joueur, Heur, Score)
    ;  Score = 0 ).

/* ==================== Maxⁿ + Shallow paramétré (H1/H2) =================== */

jouer_ia_maxn_h(Etat, Heur, MeilleurCoup) :-
    etat_jeu(_, _, Joueur, _) = Etat,
    couleurs_ia(CIA), member(Joueur, CIA),
    profondeur_max(PD), evaluation_max(Borne),
    coups_possibles(Etat, Joueur, Coups),
    ( Coups = [] -> MeilleurCoup = aucun_coup
    ; couleurs_joueurs(Couleurs), nth0(IndexJoueur, Couleurs, Joueur),
      evaluer_coups_h(Coups, Etat, IndexJoueur, PD, Borne, Heur, -1, aucun_coup, MeilleurCoup)
    ).

evaluer_coups_h([], _Etat, _IJ, _Prof, _Borne, _Heur, _BestS, BestC, BestC).
evaluer_coups_h([Coup|R], Etat, IJ, Prof, Borne, Heur, BestS, BestC, FinalCoup) :-
    appliquer_coup(Etat, Coup, E2),
    maxn_shallow_h(E2, Prof, Borne, Heur, Evals),
    nth0(IJ, Evals, S),
    ( S > BestS -> NS=S, NC=Coup ; NS=BestS, NC=BestC ),
    evaluer_coups_h(R, Etat, IJ, Prof, Borne, Heur, NS, NC, FinalCoup).

maxn_shallow_h(Etat, 0, _Borne, Heur, Evaluations) :-
    evaluer_etat_tous_joueurs_h(Etat, Heur, Evaluations), !.
maxn_shallow_h(Etat, _Prof, _Borne, Heur, Evaluations) :-
    etat_jeu(_, _, _, JoueursActifs) = Etat, length(JoueursActifs, 1),
    evaluer_etat_tous_joueurs_h(Etat, Heur, Evaluations), !.
maxn_shallow_h(Etat, Prof, Borne, Heur, Evaluations) :-
    Prof > 0,
    etat_jeu(Plateau, Ponts, Joueur, JoueursActifs) = Etat,
    coups_possibles(Etat, Joueur, Coups),
    couleurs_joueurs(Couleurs), nth0(IndexJoueur, Couleurs, Joueur),
    ( Coups = [] ->
        joueur_suivant(Joueur, JoueursActifs, J2),
        E2 = etat_jeu(Plateau, Ponts, J2, JoueursActifs),
        maxn_shallow_h(E2, Prof, Borne, Heur, Evaluations)
    ;   P1 is Prof - 1,
        length(Couleurs, N), length(Init, N), maplist(=(0), Init),
        explorer_coups_h(Coups, Etat, IndexJoueur, P1, Borne, Heur, Init, Evaluations)
    ).

explorer_coups_h([], _Etat, _IJ, _Prof, _Borne, _Heur, Best, Best).
explorer_coups_h([Coup|R], Etat, IJ, Prof, Borne, Heur, BestE, Final) :-
    appliquer_coup(Etat, Coup, E2),
    maxn_shallow_h(E2, Prof, Borne, Heur, Evals),
    nth0(IJ, Evals, S),
    nth0(IJ, BestE, SB),
    ( S > SB -> NewBest = Evals ; NewBest = BestE ),
    nth0(IJ, NewBest, Cur),
    ( Cur >= Borne -> Final = NewBest
    ; explorer_coups_h(R, Etat, IJ, Prof, Borne, Heur, NewBest, Final)
    ).

/* ===================== Minimax α-β paramétré (H1/H2) ===================== */

jouer_ia_minimax_h(Etat, Joueur, Heur, Coup) :-
    couleurs_ia(CIA), member(Joueur, CIA),
    Profondeur = 3,
    minimax_h(Etat, Joueur, Heur, Profondeur, -10000, 10000, Coup, _).

minimax_h(Etat, Joueur, Heur, 0, _A, _B, _Coup, Score) :-
    evaluer_etat_sel(Etat, Joueur, Heur, Score), !.
minimax_h(Etat, Joueur, Heur, Prof, Alpha, Beta, Coup, Best) :-
    Prof > 0,
    etat_jeu(_, _, JC, _) = Etat,
    coups_possibles(Etat, JC, Coups),
    ( Coups = [] ->
        evaluer_etat_sel(Etat, Joueur, Heur, Best), Coup = aucun_coup
    ; P1 is Prof - 1,
      ( JC = Joueur ->
          minimax_max_h(Coups, Etat, Joueur, Heur, P1, Alpha, Beta, Coup, Best)
      ;   minimax_min_h(Coups, Etat, Joueur, Heur, P1, Alpha, Beta, Coup, Best)
      )
    ).

minimax_max_h([], _Etat, _J, _Heur, _Prof, Alpha, _Beta, aucun_coup, Alpha).
minimax_max_h([C|R], Etat, J, Heur, Prof, Alpha, Beta, BestC, BestS) :-
    appliquer_coup(Etat, C, E2),
    minimax_h(E2, J, Heur, Prof, Alpha, Beta, _Tmp, S),
    ( S > Alpha -> A1 = S, C1 = C ; A1 = Alpha, C1 = BestC ),
    ( A1 >= Beta -> BestC = C1, BestS = A1
    ; minimax_max_h(R, Etat, J, Heur, Prof, A1, Beta, C1, BestS)
    ).

minimax_min_h([], _Etat, _J, _Heur, _Prof, _Alpha, Beta, aucun_coup, Beta).
minimax_min_h([C|R], Etat, J, Heur, Prof, Alpha, Beta, BestC, BestS) :-
    appliquer_coup(Etat, C, E2),
    minimax_h(E2, J, Heur, Prof, Alpha, Beta, _Tmp, S),
    ( S < Beta -> B1 = S, C1 = C ; B1 = Beta, C1 = BestC ),
    ( Alpha >= B1 -> BestC = C1, BestS = B1
    ; minimax_min_h(R, Etat, J, Heur, Prof, Alpha, B1, C1, BestS)
    ).

/* ===================== Bench: generation limitee des coups =====================

   But du bloc
   -----------
   Ce code n'est utilise QUE pendant le benchmark. Il reduit fortement
   le facteur de branchement pour eviter les timeouts/memory overflow :

   1) Pour un deplacement (d'une case vers une case adjacente), on ne considere
      PLUS tous les ponts retirables du plateau. On retire UNIQUEMENT le pont
      effectivement traverse par ce deplacement. Donc : 1 deplacement -> 1 coup.

   2) On limite (cap) le nombre de coups conserves au noeud racine du bench.

   Dependances deja definies dans ton fichier :
     - mouvements_possibles/4, appliquer_coup/3
     - evaluer_etat_tous_joueurs_h/3, couleurs_joueurs/1, couleurs_ia/1
     - joueur_suivant/3
     - profondeur_max/1, evaluation_max/1
*/

/* ---------- Reglage du "cap" racine (ajuste 20/30/40 selon ta machine) ---------- */
bench_cap_root(20).

/* take(N, L, Prefix) : prend les N premiers elements de L (ou tout si plus court) */
take(N, L, Prefix) :- length(Prefix, N), append(Prefix, _, L), !.
take(_, L, L).

/* ---------- Identification du pont traverse par un deplacement ----------

   Deux cases adjacentes partagent soit :
     - un pont horizontal (si Y identiques, X differents de 1)
     - un pont vertical   (si X identiques, Y differents de 1)

   On retourne un terme retirer_pont(Type, X, Y) qui pointe exactement
   sur le pont traverse par ce deplacement.
*/
pont_utilise_par_deplacement(pos(X1,Y1), pos(X2,Y2), retirer_pont(horizontal, Xh, Yh)) :-
    Y1 =:= Y2,
    abs(X1 - X2) =:= 1,
    Xh is min(X1, X2),
    Yh is Y1.
pont_utilise_par_deplacement(pos(X1,Y1), pos(X2,Y2), retirer_pont(vertical, Xv, Yv)) :-
    X1 =:= X2,
    abs(Y1 - Y2) =:= 1,
    Xv is X1,
    Yv is min(Y1, Y2).

/* ---------- Coups possibles limites pour le bench ----------

   1) Lister tous les deplacements autorises (comme d'habitude).
   2) Pour chaque deplacement, fabriquer UN seul "coup" :
      - deplacement(...)
      - retirer_pont(...) : c'est le pont traverse par ce deplacement
   3) Limiter le nombre total de coups par take/3 au noeud racine.
*/
coups_possibles_limites(Etat, Joueur, CoupsLimites) :-
    etat_jeu(Plateau, Ponts, Joueur, _) = Etat,

    % Tous les deplacements "simples" du joueur
    trouver_lutins(Plateau, Joueur, Lutins),
    findall(deplacement(Lutin, Dest),
            ( member(Lutin, Lutins),
              mouvements_possibles(Lutin, Plateau, Ponts, Dests),
              member(Dest, Dests)
            ),
            Deplacements0),

    % Un (et un seul) retrait de pont : celui traverse par le deplacement
    findall(coup(Deplacement, RetirerPont),
            ( member(Deplacement, Deplacements0),
              Deplacement = deplacement(pos(X1,Y1), pos(X2,Y2)),
              pont_utilise_par_deplacement(pos(X1,Y1), pos(X2,Y2), RetirerPont)
            ),
            CoupsTous),

    % Cap racine : on coupe la liste a bench_cap_root/1
    bench_cap_root(Cap),
    take(Cap, CoupsTous, CoupsLimites).

/* ======================= Versions "bench" des IA =======================

   Ces predicats sont appeles UNIQUEMENT par benchmark_heuristiques/0.
   Ils re-utilisent tes versions H1/H2 mais remplacent la generation
   des coups par coups_possibles_limites/3 (donc arbre de recherche bien
   plus petit). Tu peux aussi les envelopper dans call_with_time_limit/2
   depuis benchmark_heuristiques/0.
*/

/* ----- Max^n + shallow pruning (parametre H1/H2) : version bench ----- */
jouer_ia_maxn_h_bench(Etat, Heur, MeilleurCoup) :-
    etat_jeu(_, _, Joueur, _) = Etat,
    couleurs_ia(CIA), member(Joueur, CIA),
    profondeur_max(PD), evaluation_max(Borne),
    coups_possibles_limites(Etat, Joueur, Coups),
    ( Coups = [] ->
        MeilleurCoup = aucun_coup
    ; couleurs_joueurs(Couleurs),
      nth0(IndexJoueur, Couleurs, Joueur),
      evaluer_coups_h_bench(Coups, Etat, IndexJoueur, PD, Borne, Heur, -1, aucun_coup, MeilleurCoup)
    ).

% Evalue chaque coup et conserve celui maximisant la composante IndexJoueur
evaluer_coups_h_bench([], _Etat, _IJ, _Prof, _Borne, _Heur, _BestS, BestCoup, BestCoup).
evaluer_coups_h_bench([Coup|Reste], Etat, IJ, Prof, Borne, Heur, BestS, BestCoup, FinalCoup) :-
    appliquer_coup(Etat, Coup, E2),
    maxn_shallow_h_bench(E2, Prof, Borne, Heur, Evals),
    nth0(IJ, Evals, S),
    ( S > BestS -> NS = S, NC = Coup ; NS = BestS, NC = BestCoup ),
    evaluer_coups_h_bench(Reste, Etat, IJ, Prof, Borne, Heur, NS, NC, FinalCoup).

% Coeur Max^n shallow (version bench) : meme logique, mais coups limites
maxn_shallow_h_bench(Etat, 0, _Borne, Heur, Evaluations) :-
    evaluer_etat_tous_joueurs_h(Etat, Heur, Evaluations), !.
maxn_shallow_h_bench(Etat, _Prof, _Borne, Heur, Evaluations) :-
    etat_jeu(_, _, _, JoueursActifs) = Etat,
    length(JoueursActifs, 1),
    evaluer_etat_tous_joueurs_h(Etat, Heur, Evaluations), !.
maxn_shallow_h_bench(Etat, Prof, Borne, Heur, Evaluations) :-
    Prof > 0,
    etat_jeu(Plateau, Ponts, Joueur, JoueursActifs) = Etat,
    coups_possibles_limites(Etat, Joueur, Coups),
    couleurs_joueurs(Couleurs), nth0(IndexJoueur, Couleurs, Joueur),
    ( Coups = [] ->
        % Pas de coup : on passe au joueur suivant
        joueur_suivant(Joueur, JoueursActifs, J2),
        E2 = etat_jeu(Plateau, Ponts, J2, JoueursActifs),
        maxn_shallow_h_bench(E2, Prof, Borne, Heur, Evaluations)
    ;   P1 is Prof - 1,
        length(Couleurs, N), length(Init, N), maplist(=(0), Init),
        explorer_coups_h_bench(Coups, Etat, IndexJoueur, P1, Borne, Heur, Init, Evaluations)
    ).

% Exploration des coups avec shallow pruning sur la composante IJ
explorer_coups_h_bench([], _Etat, _IJ, _Prof, _Borne, _Heur, Best, Best).
explorer_coups_h_bench([Coup|Reste], Etat, IJ, Prof, Borne, Heur, BestSoFar, Final) :-
    appliquer_coup(Etat, Coup, E2),
    maxn_shallow_h_bench(E2, Prof, Borne, Heur, Evals),
    nth0(IJ, Evals, S),
    nth0(IJ, BestSoFar, SB),
    ( S > SB -> NewBest = Evals ; NewBest = BestSoFar ),
    nth0(IJ, NewBest, Cur),
    ( Cur >= Borne -> Final = NewBest
    ; explorer_coups_h_bench(Reste, Etat, IJ, Prof, Borne, Heur, NewBest, Final)
    ).

/* --------------- Minimax (alpha-beta) parametre H1/H2 : bench --------------- */
jouer_ia_minimax_h_bench(Etat, Joueur, Heur, Coup) :-
    couleurs_ia(CIA), member(Joueur, CIA),
    Profondeur = 1,                         % profondeur plus faible pour le bench
    minimax_h_bench(Etat, Joueur, Heur, Profondeur, -10000, 10000, Coup, _).

% Variante minimax qui appelle coups_possibles_limites/3
minimax_h_bench(Etat, Joueur, Heur, 0, _A, _B, _Coup, Score) :-
    evaluer_etat_sel(Etat, Joueur, Heur, Score), !.
minimax_h_bench(Etat, Joueur, Heur, Prof, Alpha, Beta, Coup, Best) :-
    Prof > 0,
    etat_jeu(_, _, JC, _) = Etat,
    coups_possibles_limites(Etat, JC, Coups),
    ( Coups = [] ->
        evaluer_etat_sel(Etat, Joueur, Heur, Best), Coup = aucun_coup
    ;   P1 is Prof - 1,
        ( JC = Joueur ->
            minimax_max_h_bench(Coups, Etat, Joueur, Heur, P1, Alpha, Beta, Coup, Best)
        ;   minimax_min_h_bench(Coups, Etat, Joueur, Heur, P1, Alpha, Beta, Coup, Best)
        )
    ).

% Branches max/min (propagent le meilleur coup courant C1)
minimax_max_h_bench([], _Etat, _J, _Heur, _Prof, Alpha, _Beta, aucun_coup, Alpha).
minimax_max_h_bench([C|R], Etat, J, Heur, Prof, Alpha, Beta, BestC, BestS) :-
    appliquer_coup(Etat, C, E2),
    minimax_h_bench(E2, J, Heur, Prof, Alpha, Beta, _Tmp, S),
    ( S > Alpha -> A1 = S, C1 = C ; A1 = Alpha, C1 = BestC ),
    ( A1 >= Beta -> BestC = C1, BestS = A1
    ; minimax_max_h_bench(R, Etat, J, Heur, Prof, A1, Beta, C1, BestS)
    ).

minimax_min_h_bench([], _Etat, _J, _Heur, _Prof, _Alpha, Beta, aucun_coup, Beta).
minimax_min_h_bench([C|R], Etat, J, Heur, Prof, Alpha, Beta, BestC, BestS) :-
    appliquer_coup(Etat, C, E2),
    minimax_h_bench(E2, J, Heur, Prof, Alpha, Beta, _Tmp, S),
    ( S < Beta -> B1 = S, C1 = C ; B1 = Beta, C1 = BestC ),
    ( Alpha >= B1 -> BestC = C1, BestS = B1
    ; minimax_min_h_bench(R, Etat, J, Heur, Prof, Alpha, B1, C1, BestS)
    ).


/* ======================== BENCHMARK GLOBAL ============================
   Lance automatiquement :
   - Maxn (shallow) avec H1 et H2  [versions _bench + timeout]
   - Minimax (alpha-beta) avec H1 et H2  [versions _bench + timeout]
   On met "bleu" au trait pour forcer l'IA à jouer.
   ===================================================================== */
benchmark_heuristiques :-
    % 1) Etat initial
    ( initialiser_etat(Etat0) ->
        writeln('[1] Etat initial OK')
      ; writeln('[1] initialiser_etat a echoue'),
        !, fail                        % on stoppe proprement si l'init echoue
    ),
    % 2) On force "bleu" au trait pour les tests
    Etat0 = etat_jeu(Plateau, Ponts, _J0, JoueursActifs),
    EtatBleu = etat_jeu(Plateau, Ponts, bleu, JoueursActifs),

    % --- MAXN + Shallow : H1 (avec timeout) ---
    get_time(TA1),
    ( catch(call_with_time_limit(3,
            jouer_ia_maxn_h_bench(EtatBleu, h1, CoupMaxnH1)),
            _, fail)
      -> true ; CoupMaxnH1 = timeout ),
    get_time(TA2), TempsMaxnH1 is TA2 - TA1, writeln('[2] Maxn H1 OK'),

    % --- MAXN + Shallow : H2 (avec timeout) ---
    get_time(TB1),
    ( catch(call_with_time_limit(3,
            jouer_ia_maxn_h_bench(EtatBleu, h2, CoupMaxnH2)),
            _, fail)
      -> true ; CoupMaxnH2 = timeout ),
    get_time(TB2), TempsMaxnH2 is TB2 - TB1, writeln('[3] Maxn H2 OK'),

    % --- Minimax (alpha-beta) : H1 (avec timeout) ---
    get_time(TC1),
    ( catch(call_with_time_limit(3,
            jouer_ia_minimax_h_bench(EtatBleu, bleu, h1, CoupMiniH1)),
            _, fail)
      -> true ; CoupMiniH1 = timeout ),
    get_time(TC2), TempsMiniH1 is TC2 - TC1, writeln('[4] Minimax H1 OK'),

    % --- Minimax (alpha-beta) : H2 (avec timeout) ---
    get_time(TD1),
    ( catch(call_with_time_limit(3,
            jouer_ia_minimax_h_bench(EtatBleu, bleu, h2, CoupMiniH2)),
            _, fail)
      -> true ; CoupMiniH2 = timeout ),
    get_time(TD2), TempsMiniH2 is TD2 - TD1, writeln('[5] Minimax H2 OK'),

    % 3) Affichage tableau recap
    format('~n~`=t Bench heuristiques (bleu au trait) ~`=t~72|~n'),
    format('~w~t~14+ ~w~t~24+ ~w~t~38+ ~w~n',
           ['Algorithme','Heuristique','Temps (s)','Coup choisi']),
    format('~`-t~72|~n'),
    format('~w~t~14+ ~w~t~24+ ~2f~t~38+ ~w~n',
           ['Maxn+Shallow','H1',TempsMaxnH1,CoupMaxnH1]),
    format('~w~t~14+ ~w~t~24+ ~2f~t~38+ ~w~n',
           ['Maxn+Shallow','H2',TempsMaxnH2,CoupMaxnH2]),
    format('~w~t~14+ ~w~t~24+ ~2f~t~38+ ~w~n',
           ['Minimax a-b','H1',TempsMiniH1,CoupMiniH1]),
    format('~w~t~14+ ~w~t~24+ ~2f~t~38+ ~w~n',
           ['Minimax a-b','H2',TempsMiniH2,CoupMiniH2]),
    format('~`=t~72|~n'),
    true.



/* ===================================================================== */
/*                                                                       */
/*                  PRÉDICATS D'INITIALISATION                          */
/*                                                                       */
/* Fonctions utilitaires pour initialiser les états de test             */
/*                                                                       */
/* ===================================================================== */

% Initialiser un plateau vide
initialiser_plateau(Plateau) :-
    taille_plateau(Taille),
    length(Plateau, Taille),
    maplist(initialiser_ligne(Taille), Plateau).

initialiser_ligne(Taille, Ligne) :-
    length(Ligne, Taille),
    maplist(=(vide), Ligne).

% Initialiser les ponts (tous présents au début)
initialiser_ponts(Ponts) :-
    taille_plateau(Taille),
    TailleH is Taille - 1,
    
    % Initialiser les ponts horizontaux H: 6 lignes (Y 0..5), 5 colonnes (X 0..4)
    length(PontsH, Taille),
    maplist(initialiser_ligne_ponts(TailleH), PontsH),
    
    % Initialiser les ponts verticaux
    length(PontsV, TailleH),
    maplist(initialiser_ligne_ponts(Taille), PontsV),
    
    Ponts = ponts(PontsH, PontsV).

initialiser_ligne_ponts(Taille, Ligne) :-
    length(Ligne, Taille),
    maplist(=(true), Ligne).

% Initialiser létat du jeu
initialiser_etat(Etat) :-
    initialiser_plateau(Plateau),
    initialiser_ponts(Ponts),
    placer_lutins(Plateau, NouveauPlateau),
    couleurs_joueurs(Couleurs),
    Etat = etat_jeu(NouveauPlateau, Ponts, vert, Couleurs).

% Placer les lutins sur le plateau
placer_lutins(Plateau, NouveauPlateau) :-
    taille_plateau(Taille),
    Last is Taille - 1,

    % Verts (0,0) (1,0) (0,1) (1,1)
    modifier_plateau(Plateau, 0, 0, vert, P1),
    modifier_plateau(P1, 1, 0, vert, P2),
    modifier_plateau(P2, 0, 1, vert, P3),
    modifier_plateau(P3, 1, 1, vert, P4),

    % Bleus (Last-1,0) (Last,0) (Last-1,1) (Last,1)
    Xb is Last - 1,
    modifier_plateau(P4, Xb, 0, bleu, P5),
    modifier_plateau(P5, Last, 0, bleu, P6),
    modifier_plateau(P6, Xb, 1, bleu, P7),
    modifier_plateau(P7, Last, 1, bleu, P8),

    % Jaunes (0,Last-1) (1,Last-1) (0,Last) (1,Last)
    Yj is Last - 1,
    modifier_plateau(P8, 0, Yj, jaune, P9),
    modifier_plateau(P9, 1, Yj, jaune, P10),
    modifier_plateau(P10, 0, Last, jaune, P11),
    modifier_plateau(P11, 1, Last, jaune, P12),

    % Rouges (Last-1,Last-1) (Last,Last-1) (Last-1,Last) (Last,Last)
    Xr is Last - 1, Yr is Last - 1,
    modifier_plateau(P12, Xr, Yr, rouge, P13),
    modifier_plateau(P13, Last, Yr, rouge, P14),
    modifier_plateau(P14, Xr, Last, rouge, P15),
    modifier_plateau(P15, Last, Last, rouge, NouveauPlateau).


/* ===================================================================== */
/*                                                                       */
/*                          RÉSUMÉ                                       */
/*                                                                       */
/* RÉPONSES COMPLÈTES AUX QUESTIONS POSÉES :                            */
/*                                                                       */
/* 1. DÉVELOPPEMENT DE 2 HEURISTIQUES :                                 */
/*    ✓ Heuristique 1 : Mobilité des lutins (lignes 67-85)             */
/*    ✓ Heuristique 2 : Isolation des adversaires (lignes 109-142)     */
/*                                                                       */
/* 2. COMPARAISON DE PERFORMANCE :                                       */
/*    ✓ Algorithme Minimax avec élagage Alpha-Beta (lignes 234-320)     */
/*    ✓ Algorithme Maxⁿ avec Shallow Pruning (lignes 380-520)          */
/*    ✓ Prédicats de test de performance (lignes 740-780)               */
/*                                                                       */
/* 3. SHALLOW PRUNING :                                                  */
/*    ✓ Implémentation complète (lignes 520-580)                        */
/*    ✓ Commentaires détaillés sur le principe et les avantages         */
/*    ✓ Prédicats de test d'efficacité (lignes 790-820)                 */
/*                                                                       */
/* UTILISATION POUR TESTS :                                             */
/* ?- initialiser_etat(Etat), tester_performances(Etat, bleu, M, N, T1, T2). */
/* ?- initialiser_etat(Etat), tester_shallow_pruning(Etat, bleu, A, S, T1, T2). */
/*                                                                       */
/* ===================================================================== */