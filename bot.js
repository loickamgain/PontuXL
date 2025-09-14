/**
 * PontuXL Bot Interface
 * This file handles the communication between the web interface and the Prolog bot
 */

// DOM elements
const chatMessages = document.getElementById('chat-messages');
const userInput = document.getElementById('user-input');
const sendButton = document.getElementById('send-button');

// Sample responses for the bot (will be replaced with Prolog integration)
// Questions et réponses du chatbot
const gameQuestions = [
    {
        keywords: ['hello', 'bonjour', 'salut', 'coucou', 'bonsoir', 'hey', 'hi', 'hola', 'yo', 'ola', 'slt'],
        response: "Bonjour ! Je suis PBot, le bot explicateur du jeu PontuXL. En quoi puis-je vous être utile ?"
    },
    {
        keywords: ['commence', 'premier', 'debute', 'debut', 'debut partie', 'qui commence', 'qui commence le jeu'],
        response: "Par convention, c'est au joueur en charge des lutins verts de commencer la partie."
    },
    {
        keywords: ['combien', 'nombre', 'lutin', 'pion', 'lutins', 'pions', 'equipe', 'équipe'],
        response: "Chaque équipe compte 4 lutins."
    },
    {
        keywords: ['gagner', 'victoire', 'gagne', 'but', 'but du jeu', 'comment gagner', 'fin partie', 'quand fin partie'],
        response: "La partie se termine lorsqu'il ne reste plus qu'un seul joueur dont les lutins ne sont pas tous isolés. Ce joueur est déclaré vainqueur."
    },
    {
        keywords: ['jouer', 'joue', 'deroulement', 'tour', 'comment', 'comment jouer', 'coment', 'comman', 'commant', 'commente', 'deplacer', 'déplacer'],
        response: "Pour déplacer un lutin, vous devez le faire passer d'une case à une autre en empruntant un pont. Deux lutins ne peuvent pas se trouver sur la même case. Après avoir déplacé un lutin, vous devez retirer un pont ou tourner un pont d'un quart de tour."
    },
    {
        keywords: ['règle', 'règles', 'principe', 'base'],
        response: "Pontu est un jeu où vous déplacez vos lutins et retirez des ponts pour isoler les lutins adverses. Un joueur est éliminé quand tous ses lutins sont isolés."
    },
    {
        keywords: ['déplacer', 'mouvement', 'bouger', 'déplacement'],
        response: "Vous pouvez déplacer un lutin vers une case adjacente en passant par un pont. Deux lutins ne peuvent pas occuper la même case."
    },
    {
        keywords: ['retirer', 'enlever', 'supprimer', 'pont'],
        response: "Après avoir déplacé un lutin, vous devez retirer un pont. Vous pouvez retirer le pont que vous venez d'emprunter ou n'importe quel autre pont."
    },
    {
        keywords: ['tourner', 'rotation', 'pivoter', 'orienter'],
        response: "Au lieu de retirer un pont, vous pouvez choisir de tourner un pont d'un quart de tour."
    },
    {
        keywords: ['taille', 'dimension', 'plateau', 'grille'],
        response: "Dans PontuXL, le plateau est de taille 6x6."
    },
    {
        keywords: ['ordre', 'tour', 'sequence', 'joueur'],
        response: "Les joueurs jouent dans l'ordre suivant : vert, bleu, jaune, rouge."
    },
    {
        keywords: ['ia', 'intelligence', 'artificielle', 'ordinateur', 'robot'],
        response: "Dans cette version du jeu, les lutins bleus et rouges sont contrôlés par une intelligence artificielle."
    },
    {
        keywords: ['isoler', 'éliminer', 'élimination'],
        response: "Un joueur est éliminé quand tous ses lutins sont isolés, c'est-à-dire qu'ils n'ont plus aucun pont accessible autour d'eux."
    },
    {
        keywords: ['couleur', 'équipe', 'camp'],
        response: "Il y a quatre équipes dans le jeu : verte, bleue, jaune et rouge. Les lutins verts et jaunes sont contrôlés par les joueurs humains."
    },
    {
        keywords: ['difficulté', 'niveau', 'ia'],
        response: "L'intelligence artificielle utilise un algorithme sophistiqué (Maxⁿ) pour jouer de manière stratégique, mais elle a un temps limité pour prendre ses décisions."
    },
    {
        keywords: ['stratégie', 'conseil', 'astuce', 'tactique'],
        response: "Quelques conseils stratégiques : gardez vos lutins groupés, essayez d'isoler les lutins adverses un par un, et pensez à plusieurs coups à l'avance."
    }
];

// --- en haut du fichier (ou au tout début de initBot)
if (window.__pbotInitialized) {
  // déjà initialisé, on ne refait rien
} else {
  window.__pbotInitialized = false; // flag posé mais pas encore prêt
}
// Initialize the bot
function initBot() {
    if (window.__pbotInitialized) return; // évite les doubles appels
    window.__pbotInitialized = true;

    // Get DOM elements
    const chatMessages = document.getElementById('chat-messages');
    const userInput = document.getElementById('user-input');
    const sendButton = document.getElementById('send-button');

    if (!chatMessages || !userInput || !sendButton) {
        console.error('Chat elements not found!');
        return;
    }

    // Stocker dans un objet global
    window.chatElements = { chatMessages, userInput, sendButton };

    // Remplacer le bouton par un clone pour nettoyer les anciens listeners
    sendButton.replaceWith(sendButton.cloneNode(true));
    window.chatElements.sendButton = document.getElementById('send-button');

    // Ajout des event listeners
    window.chatElements.sendButton.addEventListener('click', handleUserMessage);
    userInput.addEventListener('keypress', (event) => {
        if (event.key === 'Enter') handleUserMessage();
    });

    // Ajout unique du message d’intro
    if (chatMessages.childElementCount === 0) {
        addMessage(
          "Bonjour ! Je suis le chatbot de Pontu. Je peux vous aider avec les règles du jeu, comment jouer, et répondre à vos questions. Que voulez-vous savoir ?",
          'bot'
        );
        // Optionnel : deuxième message
        // addMessage("Posez-moi une question pour commencer 👇", 'bot');
    }
}

// --- helpers 
function prologListToArray(term) {
  const arr = [];
  let t = term;
  while (t && t.indicator === "./2") {
    arr.push(t.args[0].toString());
    t = t.args[1];
  }
  return arr;
}

function formatTokensToText(tokens) {
  let s = tokens.join(' ');
  s = s.replace(/\s+([.,!?;:])/g, '$1')
       .replace(/\s*'\s*/g, "'")
       .replace(/\(\s+/g, '(').replace(/\s+\)/g, ')')
       .replace(/\s*-\s*/g, '-')
       .replace(/\s{2,}/g, ' ')
       .trim();
  if (s) s = s[0].toUpperCase() + s.slice(1);
  if (s && !/[.!?]$/.test(s)) s += '.';
  return s;
}


// Handle user message
async function handleUserMessage() {
  const { userInput, chatMessages } = window.chatElements || {};
  const message = (userInput?.value || '').trim();
  if (!message) return;

  addMessage(message, 'user');
  if (userInput) userInput.value = '';

    // --- Interception immédiate des salutations et de "fin" (avant Prolog) ---
  const raw = message.toLowerCase().trim();
  const normalized = raw
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')   // enlève accents (bonjour, bonjour!, olá → ola)
    .replace(/[^\p{L}\p{N}\s]/gu, ' ')                  // enlève ponctuation
    .replace(/\s+/g, ' ')
    .trim();

  const tokens = normalized.split(' ').filter(Boolean);

  // Salutations : toujours répondre (à chaque fois), sans bloquer les suivantes
  const greetings = ['hello','bonjour','salut','coucou','bonsoir','hey','hi','hola','yo','ola','slt'];
  if (tokens.some(t => greetings.includes(t))) {
    addMessage("Bonjour ! Je suis PBot, le bot explicateur du jeu PontuXL. En quoi puis-je vous être utile ?", 'bot');
    return; // on ne passe pas par Prolog ni fallback pour ce tour
  }

  // Mot-clé de sortie (filet de sécurité si Prolog indispo)
  if (tokens.includes('fin')) {
    addMessage("Merci de m'avoir consulté.", 'bot');
    return;
  }
  // --- Fin interception ---

  try {
    const lines = await getPrologResponse(message); // <- ne doit rien afficher

    if (Array.isArray(lines) && lines.length > 0) {
      lines.forEach(line => addMessage(line, 'bot'));
    } else {
      // fallback unique (pas un mélange)
      const fb = getBotResponse(message);
      addMessage(fb, 'bot');
    }
  } catch (e) {
    console.error('Error getting response:', e);
    const fb = getBotResponse(message);
    addMessage(fb, 'bot');
  }

  if (chatMessages) chatMessages.scrollTop = chatMessages.scrollHeight;
}


// Add a message to the chat
function addMessage(message, sender, opts = {}) {
  const { chatMessages } = window.chatElements || {};
  if (!chatMessages) return;

  const el = document.createElement('div');
  el.className = sender === 'user' ? 'user-message' : 'bot-message';
  el.textContent = message;

  // tag spécial pour l’intro (utile pour le garde dans initBot)
  if (opts.intro) el.dataset.intro = "1";

  chatMessages.appendChild(el);
  chatMessages.scrollTop = chatMessages.scrollHeight;
}

// Fonction pour calculer la distance de Levenshtein (similarité entre deux chaînes)
function levenshteinDistance(str1, str2) {
    const m = str1.length;
    const n = str2.length;
    const dp = Array.from({ length: m + 1 }, () => Array(n + 1).fill(0));

    for (let i = 0; i <= m; i++) dp[i][0] = i;
    for (let j = 0; j <= n; j++) dp[0][j] = j;

    for (let i = 1; i <= m; i++) {
        for (let j = 1; j <= n; j++) {
            if (str1[i - 1] === str2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1];
            } else {
                dp[i][j] = 1 + Math.min(
                    dp[i - 1][j],     // suppression
                    dp[i][j - 1],     // insertion
                    dp[i - 1][j - 1]   // remplacement
                );
            }
        }
    }
    return dp[m][n];
}

// Liste des salutations courtes qui doivent correspondre exactement
const shortGreetings = new Set(['hi', 'hey', 'yo', 'ola']);

// Fonction pour vérifier si deux mots sont similaires
function areSimilar(word1, word2, threshold = 0.5) {
    // Normaliser les mots
    word1 = word1.toLowerCase()
        .replace(/['']/g, "'")
        .replace(/[\u00e0\u00e2]/g, "a")
        .replace(/[\u00e8\u00e9\u00ea\u00eb]/g, "e")
        .replace(/[\u00ee\u00ef]/g, "i")
        .replace(/[\u00f4]/g, "o")
        .replace(/[\u00f9\u00fb]/g, "u")
        .replace(/[\u00e7]/g, "c")
        .trim();
    
    word2 = word2.toLowerCase()
        .replace(/['']/g, "'")
        .replace(/[\u00e0\u00e2]/g, "a")
        .replace(/[\u00e8\u00e9\u00ea\u00eb]/g, "e")
        .replace(/[\u00ee\u00ef]/g, "i")
        .replace(/[\u00f4]/g, "o")
        .replace(/[\u00f9\u00fb]/g, "u")
        .replace(/[\u00e7]/g, "c")
        .trim();

    // Vérification spéciale pour les salutations courtes
    if (shortGreetings.has(word1) || shortGreetings.has(word2)) {
        return word1 === word2;
    }

    // Si les mots sont identiques après normalisation
    if (word1 === word2) {
        return true;
    }

    // Si un mot contient l'autre
    if (word1.includes(word2) || word2.includes(word1)) {
        return true;
    }

    // Pour les mots très courts (2 caractères ou moins)
    if (word1.length <= 2 || word2.length <= 2) {
        return false;
    }
    
    const distance = levenshteinDistance(word1, word2);
    const maxLength = Math.max(word1.length, word2.length);
    const similarity = 1 - distance / maxLength;
    
    return similarity >= threshold;
}

// Get bot response based on user input
// Get bot response based on user input (scoring robuste + garde "combien/nombre")
function getBotResponse(message) {
  const raw = (message || "").trim().toLowerCase();
  if (raw === "fin") return "Merci de m'avoir consulté.";

  // Normalisation (sans accents, sans ponctuation)
  const normalized = raw
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .replace(/[^a-z0-9\s]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();

  if (!normalized) {
    return "Je ne peux pas répondre à un message vide. Posez-moi une question sur le jeu !";
  }

  const tokens = normalized.split(' ');
  const tokenSet = new Set(tokens);

  // Mots clés importants à privilégier
  const CRITICAL = new Set(['retirer','pont','deplacer','tourner','gagner','commence','ordre','ia','couleur','regle','plateau']);

  // util: normaliser une chaîne pour comparaison
  const norm = s => (s || '')
    .toLowerCase()
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .replace(/\s+/g, ' ')
    .trim();

  let best = null;

  for (const q of gameQuestions) {
    let exact = 0, contains = 0, fuzzy = 0, criticalHits = 0;

    for (const kw of q.keywords) {
      const nkw = norm(kw);
      // exact token match
      if (tokenSet.has(nkw)) {
        exact++;
        if (CRITICAL.has(nkw)) criticalHits++;
        continue;
      }
      // sous-chaîne raisonnable (évite les mini-mots)
      const hitContain = tokens.some(t =>
        t.length >= 3 && nkw.length >= 3 && (t.includes(nkw) || nkw.includes(t))
      );
      if (hitContain) {
        contains++;
        if (CRITICAL.has(nkw)) criticalHits++;
        continue;
      }
      // fuzzy (Levenshtein)
      let bestSim = 0;
      for (const t of tokens) {
        const sim = 1 - (levenshteinDistance(t, nkw) / Math.max(t.length, nkw.length));
        if (sim > bestSim) bestSim = sim;
      }
      if (bestSim >= 0.82) {
        fuzzy++;
        if (CRITICAL.has(nkw)) criticalHits++;
      }
    }

    // Score global
    let score = 3*exact + 2*contains + 1*fuzzy + 1.5*criticalHits;

    // 🔒 Garde anti-faux-positif :
    // La réponse "Chaque équipe compte 4 lutins." n'est autorisée
    // que si la question contient un quantificateur.
    const respNorm = norm(q.response);
    const isTeamCount = /chaque equipe compte 4 lutins/.test(respNorm);
    if (isTeamCount) {
      const hasQuantifier = tokenSet.has('combien') || tokenSet.has('nombre');
      if (!hasQuantifier) {
        score = -999; // bannie si pas "combien"/"nombre"
      }
    }

    if (!best || score > best.score || (score === best.score && exact > best.exact)) {
      best = { response: q.response, score, exact };
    }
  }

  if (!best || best.score <= 0) {
    return "Je ne suis pas sûr de comprendre votre question. Vous pouvez me demander des informations sur :\n" +
      "- Les règles du jeu\n" +
      "- Comment jouer\n" +
      "- Les déplacements des lutins\n" +
      "- La gestion des ponts\n" +
      "- Les conditions de victoire\n" +
      "- L'intelligence artificielle\n" +
      "- Les stratégies de jeu";
  }

  return best.response;
}


// Fonction pour consulter le bot Prolog
async function getPrologResponse(message) {
  try {
    const session = pl.create();

    // Charger la lib lists
    await new Promise((resolve, reject) => {
      session.consult(":- use_module(library(lists)).", { success: resolve, error: reject });
    });

    // Charger le fichier Prolog du bot
    const prologText = await fetch("pbot-elm.pl").then(r => r.text());
    await new Promise((resolve, reject) => {
      session.consult(prologText, { success: resolve, error: reject });
    });

    // Normaliser + tokeniser (sans accents)
    const words = message.toLowerCase()
      .replace(/['']/g, "'")
      .replace(/[\u00e0\u00e2]/g, "a")
      .replace(/[\u00e8\u00e9\u00ea\u00eb]/g, "e")
      .replace(/[\u00ee\u00ef]/g, "i")
      .replace(/[\u00f4]/g, "o")
      .replace(/[\u00f9\u00fb]/g, "u")
      .replace(/[\u00e7]/g, "c")
      .replace(/[^a-z0-9\s?]/g, ' ')
      .trim()
      .split(/\s+/)
      .filter(Boolean);

    const query = `produire_reponse([${words.join(',')}], Response)`;

    const ans = await new Promise((resolve, reject) => {
      session.query(query, {
        success: () => session.answer({
          success: resolve,
          fail: () => resolve(null),
          error: reject
        }),
        error: reject
      });
    });

    if (!ans || !ans.links || !ans.links.Response) {
      return null; // => déclenche le fallback
    }

    // Response est une liste Prolog
    const tokens = prologListToArray(ans.links.Response);

    // Si certaines règles renvoient déjà des phrases (espaces), on fait 1 bulle par élément.
    const hasFull = tokens.some(t => /\s/.test(t));
    let lines = [];
    if (hasFull) {
      tokens.forEach(t => {
        const clean = formatTokensToText(t.split(/\s+/));
        if (clean) lines.push(clean);
      });
    } else {
      const clean = formatTokensToText(tokens);
      if (clean) lines.push(clean);
    }

    return lines.length ? lines : null; // pas d’affichage ici
  } catch (e) {
    console.error('Erreur Prolog:', e);
    return null; // => fallback
  }
}


// Initialize the bot when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    console.log('DOM loaded, initializing bot...');
    initBot();
});
