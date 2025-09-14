/**
 * PontuXL Game Logic - Working Version with Bridge Animations
 * This file contains the complete enhanced game logic with working bridge animations
 */

// Game constants
const BOARD_SIZE = 6;
const PLAYERS = ['green', 'blue', 'yellow', 'red'];
const LUTINS_PER_PLAYER = 4;
const AI_PLAYERS = ['blue', 'red']; // Players controlled by AI
const HUMAN_PLAYERS = ['green', 'yellow']; // Players controlled by humans

// Game state
let gameState = {
    currentPlayer: 0,
    selectedLutin: null,
    gamePhase: 'placement',
    gameOver: false,
    activePlayers: [...PLAYERS],
    board: [],
    bridges: {
        horizontal: [],
        vertical: []
    },
    lutins: {},
    placementCount: 0,
    currentPlayerLutinCount: {},
    lastMovedBridge: null
};

// DOM elements
const gameBoard = document.getElementById('game-board');
const currentPlayerDisplay = document.getElementById('current-player');
const gameMessage = document.getElementById('game-message');
const resetButton = document.getElementById('reset-game');
const helpButton = document.getElementById('help-button');
const helpModal = document.getElementById('help-modal');
const closeModalButton = document.querySelector('.close');

// Initialize the game
function initGame() {
    try {
        createBoard();
        resetGameState();
        renderBoard();
        updateGameInfo();
        startPlacementPhase();

        if (gameMessage) {
            gameMessage.textContent = '';
            gameMessage.style.color = '';
            gameMessage.style.fontWeight = '';
            gameMessage.style.fontSize = '';
        }
        
        console.log('Game initialized successfully');
    } catch (error) {
        console.error('Error initializing game:', error);
        if (gameMessage) {
            gameMessage.textContent = 'Loading error. Please refresh the page.';
            gameMessage.style.color = 'red';
        }
    }
}

// Create the board structure
function createBoard() {
    if (!gameBoard) {
        throw new Error('game-board element not found in DOM');
    }

    try {
        gameBoard.innerHTML = '';
        
        for (let row = 0; row < BOARD_SIZE; row++) {
            for (let col = 0; col < BOARD_SIZE; col++) {
                const cell = document.createElement('div');
                cell.className = 'cell';
                cell.dataset.row = row;
                cell.dataset.col = col;
                cell.addEventListener('click', handleCellClick);
                gameBoard.appendChild(cell);
                
                // Create horizontal bridges
                if (row > 0) {
                    const hBridge = document.createElement('div');
                    hBridge.className = 'bridge bridge-horizontal top';
                    hBridge.dataset.row = row;
                    hBridge.dataset.col = col;
                    hBridge.dataset.type = 'horizontal';
                    hBridge.dataset.position = 'top';
                    hBridge.addEventListener('click', handleBridgeClick);
                    cell.appendChild(hBridge);
                }
                
                if (row < BOARD_SIZE - 1) {
                    const hBridge = document.createElement('div');
                    hBridge.className = 'bridge bridge-horizontal bottom';
                    hBridge.dataset.row = row;
                    hBridge.dataset.col = col;
                    hBridge.dataset.type = 'horizontal';
                    hBridge.dataset.position = 'bottom';
                    hBridge.addEventListener('click', handleBridgeClick);
                    cell.appendChild(hBridge);
                }
                
                // Create vertical bridges
                if (col > 0) {
                    const vBridge = document.createElement('div');
                    vBridge.className = 'bridge bridge-vertical left';
                    vBridge.dataset.row = row;
                    vBridge.dataset.col = col;
                    vBridge.dataset.type = 'vertical';
                    vBridge.dataset.position = 'left';
                    vBridge.addEventListener('click', handleBridgeClick);
                    cell.appendChild(vBridge);
                }
                
                if (col < BOARD_SIZE - 1) {
                    const vBridge = document.createElement('div');
                    vBridge.className = 'bridge bridge-vertical right';
                    vBridge.dataset.row = row;
                    vBridge.dataset.col = col;
                    vBridge.dataset.type = 'vertical';
                    vBridge.dataset.position = 'right';
                    vBridge.addEventListener('click', handleBridgeClick);
                    cell.appendChild(vBridge);
                }
            }
        }
        console.log('Board created successfully');
    } catch (error) {
        console.error('Error creating board:', error);
        throw error;
    }
}

// Reset game state
function resetGameState() {
    gameState.board = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(null));
    gameState.bridges.horizontal = Array(BOARD_SIZE - 1).fill().map(() => Array(BOARD_SIZE).fill(true));
    gameState.bridges.vertical = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE - 1).fill(true));
    
    gameState.currentPlayer = 0;
    gameState.selectedLutin = null;
    gameState.gamePhase = 'placement';
    gameState.gameOver = false;
    gameState.activePlayers = [...PLAYERS];
    gameState.placementCount = 0;
    gameState.lutins = {};
    gameState.currentPlayerLutinCount = {};
    gameState.lastMovedBridge = null;
    
    PLAYERS.forEach(player => {
        gameState.lutins[player] = [];
        gameState.currentPlayerLutinCount[player] = 0;
    });
    
    console.log('Game state reset');
}

// Start placement phase
function startPlacementPhase() {
    gameState.gamePhase = 'placement';
    gameState.currentPlayer = 0;
    gameState.placementCount = 0;
    
    console.log("Starting placement phase");
    updateGameInfo();
    
    if (AI_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
        setTimeout(placeAILutin, 1000);
    }
}

// Place AI lutin during placement phase
function placeAILutin() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    const emptyCells = [];
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            if (gameState.board[row][col] === null) {
                emptyCells.push({ row, col });
            }
        }
    }
    
    if (emptyCells.length > 0) {
        const randomCell = emptyCells[Math.floor(Math.random() * emptyCells.length)];
        placeLutin(randomCell.row, randomCell.col, currentPlayer);
    }
}

// Place a lutin on the board
function placeLutin(row, col, player) {
    if (gameState.board[row][col] !== null) {
        console.log("Cell already occupied");
        return false;
    }
    
    if (gameState.currentPlayerLutinCount[player] >= LUTINS_PER_PLAYER) {
        console.log("Player has already placed all lutins");
        return false;
    }
    
    gameState.board[row][col] = player;
    gameState.lutins[player].push({ row, col });
    gameState.currentPlayerLutinCount[player]++;
    gameState.placementCount++;
    
    console.log(`Lutin ${player} placed at [${row}, ${col}]. Total: ${gameState.placementCount}/16`);
    
    renderBoard();
    
    if (gameState.placementCount >= 16) {
        console.log("Placement phase completed, starting normal game");
        startNormalGame();
        return true;
    }
    
    nextPlayerPlacement();
    return true;
}

// Move to next player during placement
function nextPlayerPlacement() {
    gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    updateGameInfo();
    
    if (AI_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
        setTimeout(placeAILutin, 1000);
    }
}

// Start normal game after placement
function startNormalGame() {
    gameState.gamePhase = 'select';
    gameState.currentPlayer = 0;
    gameState.selectedLutin = null;
    gameState.lastMovedBridge = null;
    
    updateGameInfo();
    handlePlayerTurn();
}

// Handle cell click
function handleCellClick(event) {
    if (gameState.gameOver) return;
    
    const cell = event.target.closest('.cell');
    if (!cell) return;
    
    const row = parseInt(cell.dataset.row);
    const col = parseInt(cell.dataset.col);
    
    if (gameState.gamePhase === 'placement') {
        const currentPlayer = PLAYERS[gameState.currentPlayer];
        
        if (!HUMAN_PLAYERS.includes(currentPlayer)) {
            return;
        }
        
        placeLutin(row, col, currentPlayer);
    } else if (gameState.gamePhase === 'move' && gameState.selectedLutin) {
        const selectedLutin = gameState.selectedLutin;
        
        if (isValidMove(selectedLutin.row, selectedLutin.col, row, col)) {
            gameState.lastMovedBridge = getBridgeUsedForMove(selectedLutin.row, selectedLutin.col, row, col);
            
            moveLutin(selectedLutin.row, selectedLutin.col, row, col);
            gameState.gamePhase = 'bridge_action';
            renderBoard();
            updateGameInfo();
        }
    }
}

// Get the bridge used for a move
function getBridgeUsedForMove(fromRow, fromCol, toRow, toCol) {
    if (fromRow === toRow) {
        const bridgeCol = Math.min(fromCol, toCol);
        return { type: 'vertical', row: fromRow, col: bridgeCol };
    } else {
        const bridgeRow = Math.min(fromRow, toRow);
        return { type: 'horizontal', row: bridgeRow, col: fromCol };
    }
}

// Check if bridge connection exists
function bridgeConnectionExists(fromRow, fromCol, toRow, toCol) {
    const rowDiff = toRow - fromRow;
    const colDiff = toCol - fromCol;
    
    if (Math.abs(rowDiff) + Math.abs(colDiff) !== 1) {
        return false;
    }
    
    if (rowDiff === 1) {
        const bridgeRow = fromRow;
        const bridgeCol = fromCol;
        return gameState.bridges.horizontal[bridgeRow][bridgeCol];
    } else if (rowDiff === -1) {
        const bridgeRow = toRow;
        const bridgeCol = fromCol;
        return gameState.bridges.horizontal[bridgeRow][bridgeCol];
    } else if (colDiff === 1) {
        const bridgeRow = fromRow;
        const bridgeCol = fromCol;
        return gameState.bridges.vertical[bridgeRow][bridgeCol];
    } else if (colDiff === -1) {
        const bridgeRow = fromRow;
        const bridgeCol = toCol;
        return gameState.bridges.vertical[bridgeRow][bridgeCol];
    }
    
    return false;
}

// Show bridge action dialog
async function showBridgeActionDialog(bridgeType, bridgeRow, bridgeCol) {
    return new Promise((resolve) => {
        const modal = document.createElement('div');
        modal.className = 'bridge-action-modal';
        modal.style.position = 'fixed';
        modal.style.top = '0';
        modal.style.left = '0';
        modal.style.width = '100vw';
        modal.style.height = '100vh';
        modal.style.backgroundColor = 'rgba(0, 0, 0, 0.7)';
        modal.style.display = 'flex';
        modal.style.justifyContent = 'center';
        modal.style.alignItems = 'center';
        modal.style.zIndex = '10000';
        
        const content = document.createElement('div');
        content.style.backgroundColor = 'white';
        content.style.padding = '30px';
        content.style.borderRadius = '15px';
        content.style.textAlign = 'center';
        content.style.boxShadow = '0 10px 30px rgba(0, 0, 0, 0.3)';
        content.style.maxWidth = '400px';
        
        const title = document.createElement('h3');
        title.textContent = 'Action sur le pont';
        title.style.marginBottom = '20px';
        title.style.color = '#2c3e50';
        
        const message = document.createElement('p');
        message.textContent = `Choisissez une action pour le pont ${bridgeType === 'horizontal' ? 'horizontal' : 'vertical'} :`;
        message.style.marginBottom = '25px';
        message.style.color = '#555';
        
        const buttonContainer = document.createElement('div');
        buttonContainer.style.display = 'flex';
        buttonContainer.style.gap = '15px';
        buttonContainer.style.justifyContent = 'center';
        
        const removeButton = document.createElement('button');
        removeButton.textContent = '🗑️ Retirer';
        removeButton.style.padding = '12px 20px';
        removeButton.style.backgroundColor = '#e74c3c';
        removeButton.style.color = 'white';
        removeButton.style.border = 'none';
        removeButton.style.borderRadius = '8px';
        removeButton.style.cursor = 'pointer';
        removeButton.style.fontSize = '1rem';
        removeButton.style.fontWeight = 'bold';
        
        const rotateButton = document.createElement('button');
        rotateButton.textContent = '🔄 Tourner';
        rotateButton.style.padding = '12px 20px';
        rotateButton.style.backgroundColor = '#3498db';
        rotateButton.style.color = 'white';
        rotateButton.style.border = 'none';
        rotateButton.style.borderRadius = '8px';
        rotateButton.style.cursor = 'pointer';
        rotateButton.style.fontSize = '1rem';
        rotateButton.style.fontWeight = 'bold';
        
        const rotateDescription = document.createElement('small');
        rotateDescription.textContent = `(Tourner d'un quart de tour : ${bridgeType === 'horizontal' ? 'horizontal → vertical' : 'vertical → horizontal'})`;
        rotateDescription.style.display = 'block';
        rotateDescription.style.marginTop = '10px';
        rotateDescription.style.color = '#7f8c8d';
        rotateDescription.style.fontStyle = 'italic';
        
        removeButton.addEventListener('click', () => {
            document.body.removeChild(modal);
            resolve('remove');
        });
        
        rotateButton.addEventListener('click', () => {
            document.body.removeChild(modal);
            resolve('rotate');
        });
        
        buttonContainer.appendChild(removeButton);
        buttonContainer.appendChild(rotateButton);
        content.appendChild(title);
        content.appendChild(message);
        content.appendChild(buttonContainer);
        content.appendChild(rotateDescription);
        modal.appendChild(content);
        document.body.appendChild(modal);
        
        removeButton.focus();
    });
}

// Handle bridge click with animations
async function handleBridgeClick(event) {
    if (gameState.gameOver || gameState.gamePhase !== 'bridge_action') return;
    
    console.log("Bridge click detected!");
    
    const bridge = event.currentTarget;
    const row = parseInt(bridge.dataset.row);
    const col = parseInt(bridge.dataset.col);
    const type = bridge.dataset.type;
    const position = bridge.dataset.position;
    
    let bridgeRow = row;
    let bridgeCol = col;
    
    if (type === 'horizontal') {
        if (position === 'top') {
            bridgeRow = row - 1;
        }
    } else if (type === 'vertical') {
        if (position === 'left') {
            bridgeCol = col - 1;
        }
    }
    
    const isValidHorizontal = type === 'horizontal' && bridgeRow >= 0 && bridgeRow < BOARD_SIZE - 1 && 
                             bridgeCol >= 0 && bridgeCol < BOARD_SIZE && gameState.bridges.horizontal[bridgeRow][bridgeCol];
    const isValidVertical = type === 'vertical' && bridgeRow >= 0 && bridgeRow < BOARD_SIZE && 
                           bridgeCol >= 0 && bridgeCol < BOARD_SIZE - 1 && gameState.bridges.vertical[bridgeRow][bridgeCol];
    
    if (!isValidHorizontal && !isValidVertical) {
        console.log("Invalid bridge coordinates or bridge doesn't exist");
        return;
    }
    
    // Add visual feedback
    bridge.classList.add('selected');
    
    if (HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
        const action = await showBridgeActionDialog(type, bridgeRow, bridgeCol);
        
        bridge.classList.remove('selected');
        
        if (action === 'remove') {
            animateBridgeRemoval(bridge);
            setTimeout(() => {
                removeBridge(type, bridgeRow, bridgeCol);
                renderBoard();
            }, 600);
        } else if (action === 'rotate') {
            animateBridgeRotation(bridge);
            setTimeout(() => {
                rotateBridge(type, bridgeRow, bridgeCol);
                renderBoard();
            }, 800);
        }
    } else {
        const action = Math.random() < 0.7 ? 'remove' : 'rotate';
        
        bridge.classList.remove('selected');
        
        if (action === 'remove') {
            animateBridgeRemoval(bridge);
            setTimeout(() => {
                removeBridge(type, bridgeRow, bridgeCol);
                renderBoard();
            }, 600);
        } else {
            animateBridgeRotation(bridge);
            setTimeout(() => {
                rotateBridge(type, bridgeRow, bridgeCol);
                renderBoard();
            }, 800);
        }
    }
    
    setTimeout(() => {
        checkPlayerElimination();
        nextTurn();
    }, 1200);
}

// Simple bridge removal animation
function animateBridgeRemoval(bridgeElement) {
    bridgeElement.style.transition = 'all 0.6s ease-out';
    bridgeElement.style.backgroundColor = '#e74c3c';
    bridgeElement.style.transform = bridgeElement.style.transform + ' scale(1.5)';
    bridgeElement.style.opacity = '0.8';
    
    setTimeout(() => {
        bridgeElement.style.opacity = '0';
        bridgeElement.style.transform = bridgeElement.style.transform.replace('scale(1.5)', 'scale(0)');
    }, 300);
}

// Simple bridge rotation animation
function animateBridgeRotation(bridgeElement) {
    bridgeElement.style.transition = 'all 0.8s ease-in-out';
    bridgeElement.style.backgroundColor = '#f39c12';
    bridgeElement.style.transform = bridgeElement.style.transform + ' rotate(90deg) scale(1.2)';
    
    setTimeout(() => {
        bridgeElement.style.opacity = '0';
        bridgeElement.style.transform = bridgeElement.style.transform + ' scale(0)';
    }, 400);
}

// Rotate a bridge (quarter turn: horizontal becomes vertical and vice versa)
function rotateBridge(bridgeType, bridgeRow, bridgeCol) {
    if (bridgeType === 'horizontal') {
        gameState.bridges.horizontal[bridgeRow][bridgeCol] = false;
        
        const possibleVerticalPositions = [
            { row: bridgeRow, col: bridgeCol },
            { row: bridgeRow + 1, col: bridgeCol },
            { row: bridgeRow, col: bridgeCol - 1 },
            { row: bridgeRow + 1, col: bridgeCol - 1 }
        ];
        
        for (const pos of possibleVerticalPositions) {
            if (pos.row >= 0 && pos.row < BOARD_SIZE && 
                pos.col >= 0 && pos.col < BOARD_SIZE - 1 && 
                !gameState.bridges.vertical[pos.row][pos.col]) {
                gameState.bridges.vertical[pos.row][pos.col] = true;
                console.log(`Horizontal bridge rotated to vertical bridge at [${pos.row}, ${pos.col}]`);
                return;
            }
        }
        
        if (bridgeRow < BOARD_SIZE && bridgeCol < BOARD_SIZE - 1) {
            gameState.bridges.vertical[bridgeRow][bridgeCol] = true;
        }
        
    } else if (bridgeType === 'vertical') {
        gameState.bridges.vertical[bridgeRow][bridgeCol] = false;
        
        const possibleHorizontalPositions = [
            { row: bridgeRow, col: bridgeCol },
            { row: bridgeRow, col: bridgeCol + 1 },
            { row: bridgeRow - 1, col: bridgeCol },
            { row: bridgeRow - 1, col: bridgeCol + 1 }
        ];
        
        for (const pos of possibleHorizontalPositions) {
            if (pos.row >= 0 && pos.row < BOARD_SIZE - 1 && 
                pos.col >= 0 && pos.col < BOARD_SIZE && 
                !gameState.bridges.horizontal[pos.row][pos.col]) {
                gameState.bridges.horizontal[pos.row][pos.col] = true;
                console.log(`Vertical bridge rotated to horizontal bridge at [${pos.row}, ${pos.col}]`);
                return;
            }
        }
        
        if (bridgeRow < BOARD_SIZE - 1 && bridgeCol < BOARD_SIZE) {
            gameState.bridges.horizontal[bridgeRow][bridgeCol] = true;
        }
    }
}

// Remove a bridge
function removeBridge(bridgeType, bridgeRow, bridgeCol) {
    if (bridgeType === 'horizontal') {
        gameState.bridges.horizontal[bridgeRow][bridgeCol] = false;
        console.log(`Horizontal bridge removed at [${bridgeRow}, ${bridgeCol}]`);
    } else if (bridgeType === 'vertical') {
        gameState.bridges.vertical[bridgeRow][bridgeCol] = false;
        console.log(`Vertical bridge removed at [${bridgeRow}, ${bridgeCol}]`);
    }
}

// Enhanced render board with proper selection handling
function renderBoard() {
    // Remove all existing lutins
    document.querySelectorAll('.lutin').forEach(lutin => lutin.remove());
    
    Object.entries(gameState.lutins).forEach(([color, positions]) => {
        positions.forEach(pos => {
            const cell = document.querySelector(`.cell[data-row="${pos.row}"][data-col="${pos.col}"]`);
            if (cell) {
                const lutin = document.createElement('div');
                lutin.className = `lutin ${color}`;
                lutin.dataset.row = pos.row;
                lutin.dataset.col = pos.col;
                lutin.dataset.color = color;
                
                const currentPlayer = PLAYERS[gameState.currentPlayer];
                if (color === currentPlayer && HUMAN_PLAYERS.includes(currentPlayer) && 
                    (gameState.gamePhase === 'select' || gameState.gamePhase === 'move')) {
                    lutin.addEventListener('click', handleLutinClick);
                    lutin.style.cursor = 'pointer';
                    
                    // Add visual hint for selectable lutins
                    lutin.classList.add('selectable');
                }
                
                cell.appendChild(lutin);
            }
        });
    });
    
    updateBridgesVisibility();
    
    // Restore selection if there's a selected lutin
    if (gameState.selectedLutin) {
        const selectedLutinElement = document.querySelector(
            `.lutin[data-row="${gameState.selectedLutin.row}"][data-col="${gameState.selectedLutin.col}"]`
        );
        if (selectedLutinElement) {
            selectedLutinElement.classList.add('selected');
        }
    }
}

// Update game information display
function updateGameInfo() {
    if (!currentPlayerDisplay || !gameMessage) {
        return;
    }
    
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    currentPlayerDisplay.textContent = currentPlayer.charAt(0).toUpperCase() + currentPlayer.slice(1);
    currentPlayerDisplay.style.color = getPlayerColor(currentPlayer);
    
    switch (gameState.gamePhase) {
        case 'placement':
            const lutinsLeft = LUTINS_PER_PLAYER - gameState.currentPlayerLutinCount[currentPlayer];
            if (HUMAN_PLAYERS.includes(currentPlayer)) {
                gameMessage.textContent = `Phase de placement : Placez un lutin (${lutinsLeft} restants)`;
            } else {
                gameMessage.textContent = `L'IA place un lutin pour ${currentPlayer}... (${lutinsLeft} restants)`;
            }
            break;
        case 'select':
            if (HUMAN_PLAYERS.includes(currentPlayer)) {
                gameMessage.textContent = 'Sélectionnez un lutin à déplacer.';
            } else {
                gameMessage.textContent = `L'IA réfléchit pour ${currentPlayer}...`;
            }
            break;
        case 'move':
            gameMessage.textContent = 'Déplacez le lutin vers une case adjacente.';
            break;
        case 'bridge_action':
            gameMessage.textContent = 'Retirez ou tournez un pont.';
            break;
    }
}

// Update bridges visibility with wood color
function updateBridgesVisibility() {
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            if (row > 0) {
                const bridge = document.querySelector(`.bridge-horizontal.top[data-row="${row}"][data-col="${col}"]`);
                if (bridge) {
                    const bridgeRow = row - 1;
                    const bridgeCol = col;
                    const isVisible = gameState.bridges.horizontal[bridgeRow][bridgeCol];
                    
                    bridge.style.display = isVisible ? 'block' : 'none';
                    bridge.style.background = 'linear-gradient(135deg, #8B4513, #A0522D, #CD853F)';
                    bridge.style.transform = 'translateX(-50%)';
                    bridge.style.opacity = '1';
                    bridge.classList.remove('selected');
                    
                    if (isVisible && gameState.gamePhase === 'bridge_action' && 
                        HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
                        bridge.style.cursor = 'pointer';
                    } else {
                        bridge.style.cursor = 'default';
                    }
                }
            }
            
            if (row < BOARD_SIZE - 1) {
                const bridge = document.querySelector(`.bridge-horizontal.bottom[data-row="${row}"][data-col="${col}"]`);
                if (bridge) {
                    const bridgeRow = row;
                    const bridgeCol = col;
                    const isVisible = gameState.bridges.horizontal[bridgeRow][bridgeCol];
                    
                    bridge.style.display = isVisible ? 'block' : 'none';
                    bridge.style.background = 'linear-gradient(135deg, #8B4513, #A0522D, #CD853F)';
                    bridge.style.transform = 'translateX(-50%)';
                    bridge.style.opacity = '1';
                    bridge.classList.remove('selected');
                    
                    if (isVisible && gameState.gamePhase === 'bridge_action' && 
                        HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
                        bridge.style.cursor = 'pointer';
                    } else {
                        bridge.style.cursor = 'default';
                    }
                }
            }
            
            if (col > 0) {
                const bridge = document.querySelector(`.bridge-vertical.left[data-row="${row}"][data-col="${col}"]`);
                if (bridge) {
                    const bridgeRow = row;
                    const bridgeCol = col - 1;
                    const isVisible = gameState.bridges.vertical[bridgeRow][bridgeCol];
                    
                    bridge.style.display = isVisible ? 'block' : 'none';
                    bridge.style.background = 'linear-gradient(135deg, #8B4513, #A0522D, #CD853F)';
                    bridge.style.transform = 'translateY(-50%)';
                    bridge.style.opacity = '1';
                    bridge.classList.remove('selected');
                    
                    if (isVisible && gameState.gamePhase === 'bridge_action' && 
                        HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
                        bridge.style.cursor = 'pointer';
                    } else {
                        bridge.style.cursor = 'default';
                    }
                }
            }
            
            if (col < BOARD_SIZE - 1) {
                const bridge = document.querySelector(`.bridge-vertical.right[data-row="${row}"][data-col="${col}"]`);
                if (bridge) {
                    const bridgeRow = row;
                    const bridgeCol = col;
                    const isVisible = gameState.bridges.vertical[bridgeRow][bridgeCol];
                    
                    bridge.style.display = isVisible ? 'block' : 'none';
                    bridge.style.background = 'linear-gradient(135deg, #8B4513, #A0522D, #CD853F)';
                    bridge.style.transform = 'translateY(-50%)';
                    bridge.style.opacity = '1';
                    bridge.classList.remove('selected');
                    
                    if (isVisible && gameState.gamePhase === 'bridge_action' && 
                        HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
                        bridge.style.cursor = 'pointer';
                    } else {
                        bridge.style.cursor = 'default';
                    }
                }
            }
        }
    }
}

// Handle player turn
function handlePlayerTurn() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    if (AI_PLAYERS.includes(currentPlayer) && !gameState.gameOver) {
        setTimeout(playAITurn, 3000);
    } else if (HUMAN_PLAYERS.includes(currentPlayer) && !gameState.gameOver) {
        enableHumanPlayerLutins();
    }
}

// Enhanced enable human player lutins with abandon functionality
function enableHumanPlayerLutins() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    // First, disable all lutins
    document.querySelectorAll('.lutin').forEach(lutin => {
        lutin.style.cursor = 'default';
        lutin.classList.remove('selectable');
        
        // Remove existing event listeners by cloning
        const newLutin = lutin.cloneNode(true);
        lutin.parentNode.replaceChild(newLutin, lutin);
    });
    
    // Then enable only the current player's lutins
    document.querySelectorAll(`.lutin.${currentPlayer}`).forEach(lutin => {
        lutin.addEventListener('click', handleLutinClick);
        lutin.style.cursor = 'pointer';
        lutin.classList.add('selectable');
        
        // Add hover effect for better UX
        lutin.addEventListener('mouseenter', () => {
            if (!lutin.classList.contains('selected')) {
                lutin.style.transform = 'scale(1.1)';
                lutin.style.boxShadow = '0 0 10px rgba(255, 255, 255, 0.5)';
            }
        });
        
        lutin.addEventListener('mouseleave', () => {
            if (!lutin.classList.contains('selected')) {
                lutin.style.transform = '';
                lutin.style.boxShadow = '';
            }
        });
    });
    
    console.log(`Lutins enabled for player ${currentPlayer} with abandon functionality`);
}

// Handle lutin click with abandon functionality
function handleLutinClick(event) {
    if (gameState.gameOver || gameState.gamePhase === 'placement') return;
    
    const lutin = event.target;
    const row = parseInt(lutin.dataset.row);
    const col = parseInt(lutin.dataset.col);
    const color = lutin.dataset.color;
    
    if (color !== PLAYERS[gameState.currentPlayer]) return;
    
    if (gameState.gamePhase === 'select' || gameState.gamePhase === 'move') {
        // Clear previous selection
        clearLutinSelection();
        
        // Select the new lutin
        gameState.selectedLutin = { row, col, color };
        gameState.gamePhase = 'move';
        
        lutin.classList.add('selected');
        updateGameInfo();
        
        console.log(`Lutin selected at [${row}, ${col}]. Previous selection abandoned.`);
    }
}

// Clear lutin selection visual feedback
function clearLutinSelection() {
    // Remove 'selected' class from all lutins
    document.querySelectorAll('.lutin.selected').forEach(selectedLutin => {
        selectedLutin.classList.remove('selected');
    });
    
    // Reset any visual effects
    document.querySelectorAll('.lutin').forEach(lutin => {
        lutin.style.transform = '';
        lutin.style.boxShadow = '';
        lutin.style.zIndex = '';
    });
}

// Check if a move is valid
function isValidMove(fromRow, fromCol, toRow, toCol) {
    if (toRow < 0 || toRow >= BOARD_SIZE || toCol < 0 || toCol >= BOARD_SIZE) {
        return false;
    }
    
    if (gameState.board[toRow][toCol] !== null) {
        return false;
    }
    
    return bridgeConnectionExists(fromRow, fromCol, toRow, toCol);
}

// Move a lutin
function moveLutin(fromRow, fromCol, toRow, toCol) {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    gameState.board[fromRow][fromCol] = null;
    gameState.board[toRow][toCol] = currentPlayer;
    
    const lutinIndex = gameState.lutins[currentPlayer].findIndex(
        lutin => lutin.row === fromRow && lutin.col === fromCol
    );
    
    if (lutinIndex !== -1) {
        gameState.lutins[currentPlayer][lutinIndex] = { row: toRow, col: toCol };
    }
    
    gameState.selectedLutin = null;
}

// Check player elimination
function checkPlayerElimination() {
    let eliminatedPlayers = [];
    
    PLAYERS.forEach(player => {
        if (gameState.activePlayers.includes(player)) {
            const isEliminated = gameState.lutins[player].every(lutin => {
                return !hasAvailableMoves(lutin.row, lutin.col);
            });
            
            if (isEliminated) {
                eliminatedPlayers.push(player);
                console.log(`Player ${player} is eliminated!`);
            }
        }
    });
    
    eliminatedPlayers.forEach(player => {
        gameState.activePlayers = gameState.activePlayers.filter(p => p !== player);
    });
    
    if (gameState.activePlayers.length === 1) {
        const winner = gameState.activePlayers[0];
        gameState.gameOver = true;
        
        if (gameMessage) {
            gameMessage.textContent = `Le joueur ${winner.charAt(0).toUpperCase() + winner.slice(1)} a gagné !`;
            gameMessage.style.color = getPlayerColor(winner);
            gameMessage.style.fontWeight = 'bold';
            gameMessage.style.fontSize = '1.5rem';
        }
        
        disableAllInteractions();
        showVictoryMessage(winner);
        
        console.log(`Game over! Player ${winner} wins!`);
        return true;
    }
    
    return false;
}

// Check if lutin has available moves
function hasAvailableMoves(row, col) {
    const adjacentCells = [
        { row: row - 1, col: col },
        { row: row + 1, col: col },
        { row: row, col: col - 1 },
        { row: row, col: col + 1 }
    ];
    
    return adjacentCells.some(cell => {
        if (cell.row >= 0 && cell.row < BOARD_SIZE && cell.col >= 0 && cell.col < BOARD_SIZE) {
            if (gameState.board[cell.row][cell.col] === null) {
                return bridgeConnectionExists(row, col, cell.row, cell.col);
            }
        }
        return false;
    });
}

// Enhanced move to next turn with proper cleanup
function nextTurn() {
    // Clear any selections and reset visual states
    clearLutinSelection();
    gameState.gamePhase = 'select';
    gameState.selectedLutin = null;
    gameState.lastMovedBridge = null;
    
    gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    
    while (!gameState.activePlayers.includes(PLAYERS[gameState.currentPlayer])) {
        gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    }
    
    renderBoard();
    updateGameInfo();
    handlePlayerTurn();
}

// Enhanced AI turn with bridge animations
async function playAITurn() {
    if (gameState.gameOver) return;
    
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    console.log(`AI playing for player: ${currentPlayer}`);
    
    const aiPromise = new Promise((resolve) => {
        try {
            resolve(useBasicAIWithAnimations());
        } catch (error) {
            console.error('Error with AI:', error);
            resolve(false);
        }
    });
    
    const timeoutPromise = new Promise((resolve) => {
        setTimeout(() => resolve('timeout'), 5000);
    });
    
    const result = await Promise.race([aiPromise, timeoutPromise]);
    
    if (result === 'timeout') {
        console.log('AI took too long, playing random move');
        playRandomMoveWithAnimations();
    }
}

// Enhanced random move for AI with animations
async function playRandomMoveWithAnimations() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    const lutins = gameState.lutins[currentPlayer];
    
    let validLutin = null;
    let validMoves = [];
    
    for (const lutin of lutins) {
        const moves = getValidMoves(lutin.row, lutin.col);
        if (moves.length > 0) {
            validLutin = lutin;
            validMoves = moves;
            break;
        }
    }
    
    if (validLutin && validMoves.length > 0) {
        const randomMove = validMoves[Math.floor(Math.random() * validMoves.length)];
        
        // Move the lutin
        moveLutin(validLutin.row, validLutin.col, randomMove.row, randomMove.col);
        renderBoard();
        
        // Get available bridges
        const bridges = getAvailableBridges();
        if (bridges.length > 0) {
            const randomBridge = bridges[Math.floor(Math.random() * bridges.length)];
            const action = Math.random() < 0.7 ? 'remove' : 'rotate';
            
            // Find the bridge element for animation
            const bridgeElement = findBridgeElementByCoordinates(randomBridge.type, randomBridge.row, randomBridge.col);
            
            if (bridgeElement) {
                if (action === 'remove') {
                    // Show AI removal message
                    if (gameMessage) {
                        gameMessage.textContent = `L'IA supprime un pont ${randomBridge.type === 'horizontal' ? 'horizontal' : 'vertical'} à [${randomBridge.row},${randomBridge.col}]`;
                        gameMessage.style.color = getPlayerColor(currentPlayer);
                        gameMessage.style.fontWeight = 'bold';
                    }
                    
                    // Add AI-specific removal animation
                    animateAIBridgeRemoval(bridgeElement);
                    
                    setTimeout(() => {
                        removeBridge(randomBridge.type, randomBridge.row, randomBridge.col);
                        finishAITurn();
                    }, 800);
                } else {
                    // Show AI rotation message
                    if (gameMessage) {
                        gameMessage.textContent = `L'IA tourne un pont ${randomBridge.type === 'horizontal' ? 'horizontal → vertical' : 'vertical → horizontal'} à [${randomBridge.row},${randomBridge.col}]`;
                        gameMessage.style.color = getPlayerColor(currentPlayer);
                        gameMessage.style.fontWeight = 'bold';
                    }
                    
                    // Add AI-specific rotation animation
                    animateAIBridgeRotation(bridgeElement);
                    
                    setTimeout(() => {
                        rotateBridge(randomBridge.type, randomBridge.row, randomBridge.col);
                        finishAITurn();
                    }, 1000);
                }
            } else {
                // Fallback if bridge element not found
                if (action === 'remove') {
                    removeBridge(randomBridge.type, randomBridge.row, randomBridge.col);
                } else {
                    rotateBridge(randomBridge.type, randomBridge.row, randomBridge.col);
                }
                finishAITurn();
            }
        } else {
            finishAITurn();
        }
    } else {
        finishAITurn();
    }
}

// Enhanced basic AI with animations
async function useBasicAIWithAnimations() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    const movableLutins = gameState.lutins[currentPlayer].filter(lutin => {
        return hasAvailableMoves(lutin.row, lutin.col);
    });
    
    if (movableLutins.length === 0) {
        console.log(`AI ${currentPlayer} has no possible moves, skipping turn`);
        checkPlayerElimination();
        nextTurn();
        return;
    }
    
    const selectedLutin = movableLutins[Math.floor(Math.random() * movableLutins.length)];
    const possibleMoves = getValidMoves(selectedLutin.row, selectedLutin.col);
    const targetCell = possibleMoves[Math.floor(Math.random() * possibleMoves.length)];
    
    const availableBridges = getAvailableBridges();
    const bridgeAction = {
        action: Math.random() < 0.7 ? 'remove' : 'rotate',
        bridge: availableBridges[Math.floor(Math.random() * availableBridges.length)]
    };
    
    executeAIMove(selectedLutin, targetCell, bridgeAction);
}

// Find bridge element by coordinates for AI animations
function findBridgeElementByCoordinates(bridgeType, bridgeRow, bridgeCol) {
    if (bridgeType === 'horizontal') {
        // Look for horizontal bridges
        let bridge = document.querySelector(`.bridge-horizontal.bottom[data-row="${bridgeRow}"][data-col="${bridgeCol}"]`);
        if (bridge && bridge.style.display !== 'none') return bridge;
        
        bridge = document.querySelector(`.bridge-horizontal.top[data-row="${bridgeRow + 1}"][data-col="${bridgeCol}"]`);
        if (bridge && bridge.style.display !== 'none') return bridge;
    } else if (bridgeType === 'vertical') {
        // Look for vertical bridges
        let bridge = document.querySelector(`.bridge-vertical.right[data-row="${bridgeRow}"][data-col="${bridgeCol}"]`);
        if (bridge && bridge.style.display !== 'none') return bridge;
        
        bridge = document.querySelector(`.bridge-vertical.left[data-row="${bridgeRow}"][data-col="${bridgeCol + 1}"]`);
        if (bridge && bridge.style.display !== 'none') return bridge;
    }
    
    return null;
}

// AI-specific bridge removal animation with different colors
function animateAIBridgeRemoval(bridgeElement) {
    // Add AI removal class for different styling
    bridgeElement.classList.add('ai-removing');
    
    // Create visual effect
    bridgeElement.style.transition = 'all 0.8s ease-out';
    bridgeElement.style.backgroundColor = '#e74c3c';
    bridgeElement.style.transform = bridgeElement.style.transform + ' scale(1.8)';
    bridgeElement.style.boxShadow = '0 0 25px rgba(231, 76, 60, 0.9)';
    bridgeElement.style.opacity = '0.9';
    
    // Add pulsing effect
    bridgeElement.style.animation = 'aiRemovalPulse 0.3s ease-in-out 2';
    
    setTimeout(() => {
        bridgeElement.style.opacity = '0';
        bridgeElement.style.transform = bridgeElement.style.transform.replace('scale(1.8)', 'scale(0)');
        bridgeElement.style.boxShadow = 'none';
    }, 400);
    
    // Create sparks effect
    createAISparkEffect(bridgeElement, 'removal');
}

// AI-specific bridge rotation animation with different colors
function animateAIBridgeRotation(bridgeElement) {
    // Add AI rotation class for different styling
    bridgeElement.classList.add('ai-rotating');
    
    // Create visual effect
    bridgeElement.style.transition = 'all 1s ease-in-out';
    bridgeElement.style.backgroundColor = '#f39c12';
    bridgeElement.style.transform = bridgeElement.style.transform + ' rotate(180deg) scale(1.5)';
    bridgeElement.style.boxShadow = '0 0 30px rgba(243, 156, 18, 0.9)';
    bridgeElement.style.opacity = '0.8';
    
    // Add spinning effect
    bridgeElement.style.animation = 'aiRotationSpin 0.5s ease-in-out 2';
    
    setTimeout(() => {
        bridgeElement.style.opacity = '0';
        bridgeElement.style.transform = bridgeElement.style.transform + ' scale(0)';
        bridgeElement.style.boxShadow = 'none';
    }, 600);
    
    // Create rotation sparks effect
    createAISparkEffect(bridgeElement, 'rotation');
}

// Create AI spark effects for bridge actions
function createAISparkEffect(bridgeElement, effectType) {
    const rect = bridgeElement.getBoundingClientRect();
    const sparkContainer = document.createElement('div');
    sparkContainer.className = 'ai-spark-container';
    sparkContainer.style.position = 'fixed';
    sparkContainer.style.left = rect.left + 'px';
    sparkContainer.style.top = rect.top + 'px';
    sparkContainer.style.width = rect.width + 'px';
    sparkContainer.style.height = rect.height + 'px';
    sparkContainer.style.pointerEvents = 'none';
    sparkContainer.style.zIndex = '100';
    
    document.body.appendChild(sparkContainer);
    
    // Create sparks
    const sparkCount = effectType === 'removal' ? 6 : 4;
    
    for (let i = 0; i < sparkCount; i++) {
        setTimeout(() => {
            const spark = document.createElement('div');
            spark.className = `ai-spark ${effectType}`;
            spark.style.position = 'absolute';
            spark.style.width = '6px';
            spark.style.height = '6px';
            spark.style.borderRadius = '50%';
            spark.style.backgroundColor = effectType === 'removal' ? '#e74c3c' : '#f39c12';
            spark.style.boxShadow = `0 0 10px ${effectType === 'removal' ? '#e74c3c' : '#f39c12'}`;
            
            // Random position within bridge bounds
            const x = Math.random() * rect.width;
            const y = Math.random() * rect.height;
            spark.style.left = x + 'px';
            spark.style.top = y + 'px';
            
            // Animate spark
            spark.style.transition = 'all 1s ease-out';
            
            sparkContainer.appendChild(spark);
            
            // Animate spark movement
            setTimeout(() => {
                const angle = Math.random() * 2 * Math.PI;
                const distance = 40 + Math.random() * 30;
                const finalX = x + Math.cos(angle) * distance;
                const finalY = y + Math.sin(angle) * distance;
                
                spark.style.transform = `translate(${finalX - x}px, ${finalY - y}px) scale(0)`;
                spark.style.opacity = '0';
            }, 50);
            
            // Remove spark after animation
            setTimeout(() => {
                if (spark.parentNode) {
                    spark.parentNode.removeChild(spark);
                }
            }, 1100);
        }, i * 80);
    }
    
    // Remove spark container after all animations
    setTimeout(() => {
        if (document.body.contains(sparkContainer)) {
            document.body.removeChild(sparkContainer);
        }
    }, 2000);
}

// Finish AI turn with proper cleanup
function finishAITurn() {
    setTimeout(() => {
        renderBoard();
        checkPlayerElimination();
        
        setTimeout(() => {
            if (gameMessage) {
                gameMessage.style.fontWeight = 'normal';
                gameMessage.style.color = '';
            }
            nextTurn();
        }, 1000);
    }, 500);
}

// Play random move for AI
function playRandomMove() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    const lutins = gameState.lutins[currentPlayer];
    
    let validLutin = null;
    let validMoves = [];
    
    for (const lutin of lutins) {
        const moves = getValidMoves(lutin.row, lutin.col);
        if (moves.length > 0) {
            validLutin = lutin;
            validMoves = moves;
            break;
        }
    }
    
    if (validLutin && validMoves.length > 0) {
        const randomMove = validMoves[Math.floor(Math.random() * validMoves.length)];
        
        moveLutin(validLutin.row, validLutin.col, randomMove.row, randomMove.col);
        
        const bridges = getAvailableBridges();
        if (bridges.length > 0) {
            const randomBridge = bridges[Math.floor(Math.random() * bridges.length)];
            const action = Math.random() < 0.7 ? 'remove' : 'rotate';
            
            if (action === 'remove') {
                removeBridge(randomBridge.type, randomBridge.row, randomBridge.col);
            } else {
                rotateBridge(randomBridge.type, randomBridge.row, randomBridge.col);
            }
        }
        
        checkPlayerElimination();
        renderBoard();
        updateGameInfo();
        nextTurn();
    }
}

// Get available bridges
function getAvailableBridges() {
    const bridges = [];
    
    for (let row = 0; row < BOARD_SIZE - 1; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            if (gameState.bridges.horizontal[row][col]) {
                bridges.push({ type: 'horizontal', row, col });
            }
        }
    }
    
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE - 1; col++) {
            if (gameState.bridges.vertical[row][col]) {
                bridges.push({ type: 'vertical', row, col });
            }
        }
    }
    
    return bridges;
}

// Get valid moves for a lutin
function getValidMoves(row, col) {
    const moves = [];
    const directions = [
        { dr: -1, dc: 0 },
        { dr: 1, dc: 0 },
        { dr: 0, dc: -1 },
        { dr: 0, dc: 1 }
    ];
    
    for (const dir of directions) {
        const newRow = row + dir.dr;
        const newCol = col + dir.dc;
        
        if (isValidMove(row, col, newRow, newCol)) {
            moves.push({ row: newRow, col: newCol });
        }
    }
    
    return moves;
}

// Use basic AI as fallback
function useBasicAI() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    const movableLutins = gameState.lutins[currentPlayer].filter(lutin => {
        return hasAvailableMoves(lutin.row, lutin.col);
    });
    
    if (movableLutins.length === 0) {
        console.log(`AI ${currentPlayer} has no possible moves, skipping turn`);
        checkPlayerElimination();
        nextTurn();
        return;
    }
    
    const selectedLutin = movableLutins[Math.floor(Math.random() * movableLutins.length)];
    const possibleMoves = getValidMoves(selectedLutin.row, selectedLutin.col);
    const targetCell = possibleMoves[Math.floor(Math.random() * possibleMoves.length)];
    
    const availableBridges = getAvailableBridges();
    const bridgeAction = {
        action: Math.random() < 0.7 ? 'remove' : 'rotate',
        bridge: availableBridges[Math.floor(Math.random() * availableBridges.length)]
    };
    
    executeAIMove(selectedLutin, targetCell, bridgeAction);
}

// Enhanced Execute AI move with improved bridge animations
function executeAIMove(selectedLutin, targetCell, bridgeAction) {
    gameState.selectedLutin = selectedLutin;
    gameState.gamePhase = 'move';
    renderBoard();
    updateGameInfo();
    
    if (gameMessage) {
        gameMessage.textContent = `L'IA déplace un lutin de [${selectedLutin.row},${selectedLutin.col}] vers [${targetCell.row},${targetCell.col}]`;
        gameMessage.style.color = getPlayerColor(PLAYERS[gameState.currentPlayer]);
        gameMessage.style.fontWeight = 'bold';
        gameMessage.classList.add('ai-action');
    }
    
    setTimeout(() => {
        const selectedLutinElement = document.querySelector(`.lutin[data-row="${selectedLutin.row}"][data-col="${selectedLutin.col}"]`);
        
        if (selectedLutinElement) {
            selectedLutinElement.style.transition = 'all 0.8s ease-in-out';
            selectedLutinElement.style.transform = 'scale(1.2)';
            selectedLutinElement.style.zIndex = '10';
            selectedLutinElement.style.boxShadow = '0 0 20px rgba(52, 152, 219, 0.8)';
            
            // Add AI indicator
            const aiIndicator = document.createElement('div');
            aiIndicator.className = 'ai-turn-indicator';
            aiIndicator.textContent = 'IA';
            selectedLutinElement.appendChild(aiIndicator);
        }
        
        setTimeout(() => {
            moveLutin(selectedLutin.row, selectedLutin.col, targetCell.row, targetCell.col);
            gameState.gamePhase = 'bridge_action';
            renderBoard();
            
            if (gameMessage) {
                const actionText = bridgeAction.action === 'remove' ? 'supprime' : 'tourne d\'un quart de tour';
                const directionText = bridgeAction.bridge.type === 'horizontal' ? 'horizontal' : 'vertical';
                gameMessage.textContent = `L'IA ${actionText} un pont ${directionText} à [${bridgeAction.bridge.row},${bridgeAction.bridge.col}]`;
                gameMessage.classList.add('ai-action');
            }
            
            setTimeout(async () => {
                try {
                    // Find the bridge element for animation
                    const bridgeElement = findBridgeElementByCoordinates(bridgeAction.bridge.type, bridgeAction.bridge.row, bridgeAction.bridge.col);
                    
                    if (bridgeElement) {
                        // Highlight the bridge first
                        bridgeElement.classList.add('ai-highlighting');
                        
                        setTimeout(async () => {
                            if (bridgeAction.action === 'remove') {
                                animateAIBridgeRemoval(bridgeElement);
                                setTimeout(() => {
                                    removeBridge(bridgeAction.bridge.type, bridgeAction.bridge.row, bridgeAction.bridge.col);
                                    finishAITurn();
                                }, 800);
                            } else {
                                animateAIBridgeRotation(bridgeElement);
                                setTimeout(() => {
                                    rotateBridge(bridgeAction.bridge.type, bridgeAction.bridge.row, bridgeAction.bridge.col);
                                    finishAITurn();
                                }, 1000);
                            }
                        }, 1000);
                    } else {
                        // Fallback if bridge element not found
                        if (bridgeAction.action === 'remove') {
                            removeBridge(bridgeAction.bridge.type, bridgeAction.bridge.row, bridgeAction.bridge.col);
                        } else {
                            rotateBridge(bridgeAction.bridge.type, bridgeAction.bridge.row, bridgeAction.bridge.col);
                        }
                        finishAITurn();
                    }
                    
                } catch (error) {
                    console.error("Error executing AI bridge action:", error);
                    finishAITurn();
                }
            }, 2000);
        }, 800);
    }, 1500);
}

// Event listeners
if (resetButton) {
    resetButton.addEventListener('click', () => {
        initGame();
    });
}

if (helpButton && helpModal) {
    helpButton.addEventListener('click', () => {
        helpModal.style.display = 'block';
    });
}

if (closeModalButton && helpModal) {
    closeModalButton.addEventListener('click', () => {
        helpModal.style.display = 'none';
    });
}

window.addEventListener('click', (event) => {
    if (helpModal && event.target === helpModal) {
        helpModal.style.display = 'none';
    }
});

// Initialize game when page loads
window.addEventListener('DOMContentLoaded', initGame);

// Also handle the case where the script is loaded after DOMContentLoaded
if (document.readyState === 'complete' || document.readyState === 'interactive') {
    console.log('DOM already loaded, initializing game...');
    initGame();
}

// Get CSS color for player
function getPlayerColor(player) {
    switch (player) {
        case 'green': return '#2ecc71';
        case 'blue': return '#3498db';
        case 'yellow': return '#f1c40f';
        case 'red': return '#e74c3c';
        default: return '#333';
    }
}

// Disable all interactions
function disableAllInteractions() {
    document.querySelectorAll('.lutin').forEach(lutin => {
        lutin.style.cursor = 'default';
        const newLutin = lutin.cloneNode(true);
        lutin.parentNode.replaceChild(newLutin, lutin);
    });
    
    document.querySelectorAll('.bridge-horizontal, .bridge-vertical').forEach(bridge => {
        bridge.style.cursor = 'default';
        const newBridge = bridge.cloneNode(true);
        bridge.parentNode.replaceChild(newBridge, bridge);
    });
    
    document.querySelectorAll('.cell').forEach(cell => {
        cell.style.cursor = 'default';
        const newCell = cell.cloneNode(true);
        cell.parentNode.replaceChild(newCell, cell);
    });
}

// Show victory message
function showVictoryMessage(winner) {
    try {
        disableAllInteractions();
        gameState.gameOver = true;
        updateGameInfo();
        
        console.log(`Victory for player ${winner}`);
        
        const victoryOverlay = document.createElement('div');
        victoryOverlay.classList.add('victory-message');
        victoryOverlay.style.position = 'fixed';
        victoryOverlay.style.top = '0';
        victoryOverlay.style.left = '0';
        victoryOverlay.style.width = '100vw';
        victoryOverlay.style.height = '100vh';
        victoryOverlay.style.backgroundColor = 'rgba(0, 0, 0, 0.85)';
        victoryOverlay.style.display = 'flex';
        victoryOverlay.style.justifyContent = 'center';
        victoryOverlay.style.alignItems = 'center';
        victoryOverlay.style.zIndex = '9999';
        victoryOverlay.style.margin = '0';
        victoryOverlay.style.padding = '0';
        
        const victoryContent = document.createElement('div');
        victoryContent.classList.add('victory-content');
        victoryContent.style.backgroundColor = 'white';
        victoryContent.style.padding = '40px 50px';
        victoryContent.style.borderRadius = '15px';
        victoryContent.style.textAlign = 'center';
        victoryContent.style.boxShadow = '0 10px 30px rgba(0, 0, 0, 0.3)';
        victoryContent.style.maxWidth = '90vw';
        victoryContent.style.maxHeight = '90vh';
        victoryContent.style.transform = 'scale(0.8)';
        victoryContent.style.transition = 'transform 0.3s ease-out';
        victoryContent.style.border = `4px solid ${getPlayerColor(winner)}`;
        
        setTimeout(() => {
            victoryContent.style.transform = 'scale(1)';
        }, 10);
        
        const victoryTitle = document.createElement('h2');
        victoryTitle.textContent = '🎉 VICTOIRE ! 🎉';
        victoryTitle.style.margin = '0 0 25px 0';
        victoryTitle.style.fontSize = '2.5em';
        victoryTitle.style.color = getPlayerColor(winner);
        victoryTitle.style.textShadow = '2px 2px 4px rgba(0, 0, 0, 0.2)';
        victoryTitle.style.fontFamily = 'Arial, sans-serif';
        victoryTitle.style.fontWeight = 'bold';
        
        const victoryMessage = document.createElement('p');
        victoryMessage.textContent = `Le joueur ${winner.charAt(0).toUpperCase() + winner.slice(1)} a gagné la partie !`;
        victoryMessage.style.color = getPlayerColor(winner);
        victoryMessage.style.fontSize = '1.8em';
        victoryMessage.style.fontWeight = 'bold';
        victoryMessage.style.margin = '0 0 30px 0';
        victoryMessage.style.fontFamily = 'Arial, sans-serif';
        
        const replayButton = document.createElement('button');
        replayButton.textContent = '🎮 Nouvelle Partie';
        replayButton.style.padding = '15px 30px';
        replayButton.style.fontSize = '1.3em';
        replayButton.style.backgroundColor = getPlayerColor(winner);
        replayButton.style.color = 'white';
        replayButton.style.border = 'none';
        replayButton.style.borderRadius = '8px';
        replayButton.style.cursor = 'pointer';
        replayButton.style.fontWeight = 'bold';
        replayButton.style.fontFamily = 'Arial, sans-serif';
        replayButton.style.transition = 'all 0.3s ease';
        replayButton.style.boxShadow = '0 4px 8px rgba(0, 0, 0, 0.2)';
        
        replayButton.addEventListener('mouseenter', () => {
            replayButton.style.transform = 'translateY(-2px)';
            replayButton.style.boxShadow = '0 6px 12px rgba(0, 0, 0, 0.3)';
        });
        
        replayButton.addEventListener('mouseleave', () => {
            replayButton.style.transform = 'translateY(0)';
            replayButton.style.boxShadow = '0 4px 8px rgba(0, 0, 0, 0.2)';
        });
        
        replayButton.addEventListener('click', () => {
            victoryContent.style.transform = 'scale(0.8)';
            victoryOverlay.style.opacity = '0';
            
            setTimeout(() => {
                if (document.body.contains(victoryOverlay)) {
                    document.body.removeChild(victoryOverlay);
                }
                initGame();
            }, 200);
        });
        
        const closeButton = document.createElement('button');
        closeButton.textContent = '✕';
        closeButton.style.position = 'absolute';
        closeButton.style.top = '10px';
        closeButton.style.right = '15px';
        closeButton.style.background = 'none';
        closeButton.style.border = 'none';
        closeButton.style.fontSize = '1.5em';
        closeButton.style.color = '#666';
        closeButton.style.cursor = 'pointer';
        closeButton.style.width = '30px';
        closeButton.style.height = '30px';
        closeButton.style.borderRadius = '50%';
        closeButton.style.transition = 'all 0.2s ease';
        
        closeButton.addEventListener('mouseenter', () => {
            closeButton.style.backgroundColor = '#f0f0f0';
            closeButton.style.color = '#000';
        });
        
        closeButton.addEventListener('mouseleave', () => {
            closeButton.style.backgroundColor = 'transparent';
            closeButton.style.color = '#666';
        });
        
        closeButton.addEventListener('click', () => {
            victoryContent.style.transform = 'scale(0.8)';
            victoryOverlay.style.opacity = '0';
            
            setTimeout(() => {
                if (document.body.contains(victoryOverlay)) {
                    document.body.removeChild(victoryOverlay);
                }
                initGame();
            }, 200);
        });
        
        const escapeHandler = (event) => {
            if (event.key === 'Escape') {
                closeButton.click();
                document.removeEventListener('keydown', escapeHandler);
            }
        };
        document.addEventListener('keydown', escapeHandler);
        
        victoryContent.style.position = 'relative';
        victoryContent.appendChild(closeButton);
        victoryContent.appendChild(victoryTitle);
        victoryContent.appendChild(victoryMessage);
        victoryContent.appendChild(replayButton);
        victoryOverlay.appendChild(victoryContent);
        
        document.body.appendChild(victoryOverlay);
        
        setTimeout(() => {
            replayButton.focus();
        }, 100);
        
    } catch (error) {
        console.error('Error displaying victory message:', error);
    }
}