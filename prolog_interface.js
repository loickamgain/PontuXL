/**
 * Interface entre JavaScript et Prolog pour l'IA du jeu PontuXL
 * Utilise Tau Prolog pour exécuter le code Prolog directement dans le navigateur
 */

// Fonction pour charger le code Prolog depuis un fichier
async function loadPrologCode(url) {
    try {
        const response = await fetch(url);
        if (!response.ok) {
            throw new Error(`Erreur lors du chargement du fichier Prolog: ${response.status}`);
        }
        return await response.text();
    } catch (error) {
        console.error("Erreur lors du chargement du code Prolog:", error);
        return null;
    }
}

// Classe pour l'interface Prolog
class PrologInterface {
    constructor() {
        this.session = null;
        this.initialized = false;
    }

    // Initialiser l'interface Prolog
    async initialize() {
        if (this.initialized) return true;

        try {
            // Vérifier si pl est disponible (Tau Prolog)
            if (typeof pl === 'undefined') {
                console.error("Tau Prolog n'est pas chargé");
                return false;
            }

            // Créer une nouvelle session Prolog
            this.session = pl.create();

            // Charger le code Prolog
            const prologCode = await loadPrologCode('pontu_ai.pl');
            if (!prologCode) return false;

            // Charger le code dans la session
            await new Promise((resolve, reject) => {
                this.session.consult(prologCode, {
                    success: resolve,
                    error: (err) => {
                        console.error("Erreur lors de la consultation du code Prolog:", err);
                        reject(err);
                    }
                });
            });

            this.initialized = true;
            console.log("Interface Prolog initialisée avec succès");
            return true;
        } catch (error) {
            console.error("Erreur lors de l'initialisation de l'interface Prolog:", error);
            return false;
        }
    }

    // Convertir l'état du jeu JavaScript en format Prolog
    convertGameStateToPrologFormat(gameState) {
        // Créer une matrice pour le plateau
        const boardSize = 6;
        let board = Array(boardSize).fill().map(() => Array(boardSize).fill('vide'));

        // Remplir le plateau avec les lutins
        for (const player of ['green', 'blue', 'yellow', 'red']) {
            const prologPlayer = this.convertPlayerColorToProlog(player);
            for (const lutin of gameState.lutins[player]) {
                board[lutin.row][lutin.col] = prologPlayer;
            }
        }

        // Convertir les ponts
        const horizontalBridges = JSON.parse(JSON.stringify(gameState.bridges.horizontal));
        const verticalBridges = JSON.parse(JSON.stringify(gameState.bridges.vertical));

        // Convertir les valeurs booléennes en 'true' ou 'false' pour Prolog
        for (let i = 0; i < horizontalBridges.length; i++) {
            for (let j = 0; j < horizontalBridges[i].length; j++) {
                horizontalBridges[i][j] = horizontalBridges[i][j] ? 'true' : 'false';
            }
        }

        for (let i = 0; i < verticalBridges.length; i++) {
            for (let j = 0; j < verticalBridges[i].length; j++) {
                verticalBridges[i][j] = verticalBridges[i][j] ? 'true' : 'false';
            }
        }

        // Convertir le joueur courant
        const currentPlayer = this.convertPlayerColorToProlog(gameState.PLAYERS[gameState.currentPlayer]);

        // Convertir les joueurs actifs
        const activePlayers = gameState.activePlayers.map(player => this.convertPlayerColorToProlog(player));

        return {
            board,
            bridges: {
                horizontal: horizontalBridges,
                vertical: verticalBridges
            },
            currentPlayer,
            activePlayers
        };
    }

    // Convertir les couleurs des joueurs de JavaScript à Prolog
    convertPlayerColorToProlog(color) {
        const colorMap = {
            'green': 'vert',
            'blue': 'bleu',
            'yellow': 'jaune',
            'red': 'rouge'
        };
        return colorMap[color] || color;
    }

    // Convertir les couleurs des joueurs de Prolog à JavaScript
    convertPlayerColorToJS(color) {
        const colorMap = {
            'vert': 'green',
            'bleu': 'blue',
            'jaune': 'yellow',
            'rouge': 'red'
        };
        return colorMap[color] || color;
    }

    // Créer une requête Prolog pour obtenir le meilleur coup
    createPrologQuery(prologState) {
        // Construire les termes Prolog pour le plateau
        let boardTerm = '[';
        for (let i = 0; i < prologState.board.length; i++) {
            if (i > 0) boardTerm += ',';
            boardTerm += '[';
            for (let j = 0; j < prologState.board[i].length; j++) {
                if (j > 0) boardTerm += ',';
                boardTerm += prologState.board[i][j];
            }
            boardTerm += ']';
        }
        boardTerm += ']';

        // Construire les termes Prolog pour les ponts horizontaux
        let horizontalBridgesTerm = '[';
        for (let i = 0; i < prologState.bridges.horizontal.length; i++) {
            if (i > 0) horizontalBridgesTerm += ',';
            horizontalBridgesTerm += '[';
            for (let j = 0; j < prologState.bridges.horizontal[i].length; j++) {
                if (j > 0) horizontalBridgesTerm += ',';
                horizontalBridgesTerm += prologState.bridges.horizontal[i][j];
            }
            horizontalBridgesTerm += ']';
        }
        horizontalBridgesTerm += ']';

        // Construire les termes Prolog pour les ponts verticaux
        let verticalBridgesTerm = '[';
        for (let i = 0; i < prologState.bridges.vertical.length; i++) {
            if (i > 0) verticalBridgesTerm += ',';
            verticalBridgesTerm += '[';
            for (let j = 0; j < prologState.bridges.vertical[i].length; j++) {
                if (j > 0) verticalBridgesTerm += ',';
                verticalBridgesTerm += prologState.bridges.vertical[i][j];
            }
            verticalBridgesTerm += ']';
        }
        verticalBridgesTerm += ']';

        // Construire le terme Prolog pour les ponts
        const bridgesTerm = `ponts(${horizontalBridgesTerm},${verticalBridgesTerm})`;

        // Construire le terme Prolog pour les joueurs actifs
        let activePlayersTerm = '[';
        for (let i = 0; i < prologState.activePlayers.length; i++) {
            if (i > 0) activePlayersTerm += ',';
            activePlayersTerm += prologState.activePlayers[i];
        }
        activePlayersTerm += ']';

        // Construire la requête Prolog
        // Utiliser le prédicat standard qui est plus stable
        return `obtenir_coup_ia(${boardTerm}, ${bridgesTerm}, ${prologState.currentPlayer}, Deplacement, RetirerPont)`;
    }

    // Convertir le résultat Prolog en format JavaScript
    convertPrologResultToJSFormat(result) {
        // Analyser le résultat de la requête Prolog
        try {
            // Extraire le déplacement
            const deplacement = result.links.Deplacement;
            const deplMatch = deplacement.toString().match(/deplacement\(pos\((\d+),(\d+)\),pos\((\d+),(\d+)\)\)/);
            if (!deplMatch) throw new Error("Format de déplacement invalide");

            const fromRow = parseInt(deplMatch[1]);
            const fromCol = parseInt(deplMatch[2]);
            const toRow = parseInt(deplMatch[3]);
            const toCol = parseInt(deplMatch[4]);

            // Extraire le retrait de pont
            const retirerPont = result.links.RetirerPont;
            const pontMatch = retirerPont.toString().match(/retirer_pont\((horizontal|vertical),(\d+),(\d+)\)/);
            if (!pontMatch) throw new Error("Format de retrait de pont invalide");

            const bridgeType = pontMatch[1];
            const bridgeRow = parseInt(pontMatch[2]);
            const bridgeCol = parseInt(pontMatch[3]);

            return {
                lutin: { row: fromRow, col: fromCol },
                targetCell: { row: toRow, col: toCol },
                bridge: { type: bridgeType, row: bridgeRow, col: bridgeCol }
            };
        } catch (error) {
            console.error("Erreur lors de la conversion du résultat Prolog:", error);
            return null;
        }
    }

    // Trouver le meilleur coup avec l'algorithme Maxⁿ en Prolog
    async findBestMove(gameState) {
        if (!this.initialized) {
            const success = await this.initialize();
            if (!success) return null;
        }

        try {
            // Convertir l'état du jeu en format Prolog
            const prologState = this.convertGameStateToPrologFormat(gameState);

            // Créer la requête Prolog
            const query = this.createPrologQuery(prologState);

            // Exécuter la requête
            const result = await new Promise((resolve, reject) => {
                this.session.query(query, {
                    success: (goal) => {
                        this.session.answer({
                            success: (answer) => resolve(answer),
                            error: (err) => reject(err),
                            fail: () => resolve(null)
                        });
                    },
                    error: (err) => reject(err)
                });
            });

            if (!result) {
                console.log("Aucun coup trouvé par Prolog");
                return null;
            }

            // Convertir le résultat en format JavaScript
            return this.convertPrologResultToJSFormat(result);
        } catch (error) {
            console.error("Erreur lors de la recherche du meilleur coup:", error);
            return null;
        }
    }
}

// Exporter l'interface Prolog
const prologInterface = new PrologInterface();

// Fonction globale pour trouver le meilleur coup
async function findBestMove(gameState) {
    return await prologInterface.findBestMove(gameState);
}
