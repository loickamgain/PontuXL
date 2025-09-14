# Jeu PontuXL

PontuXL est une implémentation web du jeu de société Pontu, améliorée avec une interface utilisateur moderne, des animations fluides et une IA puissante. Ce projet a été développé dans le cadre du cours INFO B317 - IA et programmation symbolique.

## Aperçu du jeu

Pontu est un jeu de stratégie dont l'objectif est d'isoler les pièces de l'adversaire ("lutins") en déplaçant les siennes et en modifiant stratégiquement le plateau de jeu. Le dernier joueur avec des lutins connectés gagne.

Le jeu se déroule sur une grille de 6x6 cases reliées par des ponts. Quatre joueurs (Vert, Bleu, Jaune, Rouge) déplacent à tour de rôle leurs lutins et manipulent les ponts.

-   **Joueurs humains :** Vert et Jaune
-   **Joueurs IA :** Bleu et Rouge

### Phases du jeu

1.  **Phase de placement :** Les joueurs placent à tour de rôle leurs deux lutins sur des cases vides.
2.  **Phase principale :** Une fois tous les lutins placés, les joueurs jouent à tour de rôle dans un cycle (Vert -> Bleu -> Jaune -> Rouge).

### Tour d'un joueur

Un tour se compose de deux actions :

1.  **Déplacer un lutin :** Déplacez l'un de vos lutins sur un pont adjacent vers une case vide.
2.  **Action sur un pont :** Après vous être déplacé, vous devez effectuer l'une des actions suivantes :
    *   **Retirer un pont :** Cliquez sur n'importe quel pont du plateau pour le retirer définitivement.
    *   **Pivoter un pont :** Cliquez sur n'importe quel pont pour le faire pivoter de 90 degrés, changeant son orientation (horizontale à verticale ou vice-versa).

Un joueur est éliminé lorsque tous ses lutins sont isolés (c'est-à-dire qu'ils n'ont plus de ponts connectés à leurs cases).

## Fonctionnalités Clés

-   **Interface utilisateur moderne et réactive :** Une interface propre et intuitive qui fonctionne sur différentes tailles d'écran.
-   **Animations riches :** Des animations fluides pour les mouvements des lutins, les suppressions et les rotations de ponts améliorent l'expérience utilisateur.
-   **Plateau de jeu interactif :** Les cases et les ponts se surlignent au survol, et les éléments sélectionnables sont clairement indiqués.
-   **Mécanique de rotation des ponts :** Ajoute une nouvelle dimension stratégique au jeu, permettant aux joueurs de remodeler la connectivité du plateau.
-   **Modale d'action :** Une modale claire et accessible apparaît lorsqu'on clique sur un pont, invitant le joueur à choisir entre le retirer ou le faire pivoter.
-   **Retour visuel de l'IA :** Lorsque l'IA retire un pont, celui-ci clignote brièvement en rouge pour indiquer clairement son action.
-   **Écran de victoire animé :** Un écran de victoire soigné et animé annonce le gagnant et permet une revanche rapide.

## Comment jouer

1.  **Ouvrir le jeu :** Lancez `index.html` dans un navigateur web moderne (par ex. Chrome, Firefox).
2.  **Phase de placement :** Cliquez sur n'importe quelle case vide pour placer vos lutins. Le jeu vous guidera à travers l'ordre des joueurs.
3.  **Phase principale (Votre tour) :**
    *   **Sélectionner un lutin :** Cliquez sur l'un de vos lutins. Les cases de destination valides seront mises en surbrillance.
    *   **Déplacer :** Cliquez sur une case en surbrillance pour déplacer votre lutin.
    *   **Action sur un pont :** Après le déplacement, cliquez sur n'importe quel pont. Une modale vous demandera si vous voulez **Retirer** ou **Pivoter**. L'IA effectuera son action automatiquement.
4.  **Condition de victoire :** La partie se termine lorsqu'il ne reste qu'un seul joueur avec des lutins connectés.

## Intelligence Artificielle

Les joueurs IA (Bleu et Rouge) sont alimentés par un moteur Prolog sophistiqué fonctionnant directement dans le navigateur via Tau Prolog.

### Algorithme Maxⁿ

Au lieu de l'algorithme Minimax traditionnel pour les jeux à two joueurs, PontuXL utilise l'**algorithme Maxⁿ**, conçu pour les scénarios multi-joueurs. Dans ce modèle, chaque joueur vise à maximiser son propre score, en supposant que les autres joueurs feront de même. Cette approche est idéale pour un jeu à 4 joueurs où chacun joue pour soi.

La recherche est optimisée à l'aide de l'**élagage superficiel (Shallow Pruning)** pour réduire l'espace de recherche et améliorer les performances.

### Intégration Prolog

L'interface JavaScript communique avec le moteur Prolog (`pontu_ai.pl`) via une interface dans `prolog_interface.js`.

1.  L'état actuel du jeu (positions des lutins, disposition des ponts) est converti en termes Prolog.
2.  Le moteur Prolog exécute l'algorithme Maxⁿ pour déterminer le meilleur coup possible pour le joueur IA.
3.  Le coup choisi est renvoyé au code JavaScript, qui l'exécute ensuite sur le plateau de jeu avec les animations correspondantes.

## Fichiers

-   `index.html`: Le point d'entrée principal et la structure du jeu.
-   `styles.css`: Contient tous les styles pour le plateau de jeu, les éléments de l'interface utilisateur et les animations.
-   `game.js`: La logique de base du jeu, y compris la gestion de l'état, les tours des joueurs et la manipulation du DOM.
-   `prolog_interface.js`: Le pont entre le jeu JavaScript et l'IA Prolog.
-   `pontu_ai.pl`: Le code Prolog définissant la logique de l'IA et l'algorithme Maxⁿ.

## Exécution

Pour exécuter le jeu, vous pouvez simplement ouvrir le fichier `index.html` dans un navigateur, ou utiliser un serveur web local :

```bash
python3 -m http.server 8000
```

Puis accéder à `http://localhost:8000` dans votre navigateur.

