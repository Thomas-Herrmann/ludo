var gameState;

var c, ctx;
var tileWidth = 50;
var tileHeight = 50;

var drawGameState, drawOptions, drawRoll;

const numTilesX = 15;
const numTilesY = 15;

const playerColor = {
    Green: [3, 166, 120],
    Yellow: [242, 159, 5],
    Red: [242, 7, 70],
    Blue: [4, 196, 217]
}

const playerColorDark = {
    Green: [2, 132, 95],
    Yellow: [193, 127, 4],
    Red: [193, 5, 56],
    Blue: [3, 156, 173]
}


function onHasteStart() {
    c = document.getElementById("canvas");
    c.width = c.scrollWidth;
    c.height = c.scrollHeight;
    ctx = c.getContext("2d");

    ctx.imageSmoothingEnabled = false;

    tileWidth = c.width / numTilesX;
    tileHeight = c.height / numTilesY;

    window.onresize = function(event) {    
        c.width = c.scrollWidth;
        c.height = c.scrollHeight;
        tileWidth = c.width / numTilesX;
        tileHeight = c.height / numTilesY;

        drawBoard(drawGameState, drawOptions, drawRoll);
    };
}

function drawTiles(tiles, drawFunc, ...args) {
    for (var i = 0; i < tiles.length; i++) {
        let tile = tiles[i];
        let posX = tile[0] * tileWidth;
        let posY = tile[1] * tileHeight;

        drawFunc(posX, posY, ...args)
    }
}

function cssColor(r, g, b) {
    return "rgb(" + r + "," + g + "," + b + ")";
}

function drawSolid(posX, posY, r, g, b, border = (tileWidth * 0.015)) {
    ctx.fillStyle = cssColor(r, g, b);
    ctx.fillRect(posX + border, posY + border, tileWidth - border * 2, tileHeight - border * 2);
}

function drawGlobe(posX, posY) {
    ctx.beginPath();
    ctx.strokeStyle = "#808080"
    ctx.lineWidth = tileWidth * 0.05;
    ctx.arc(posX + tileWidth / 2, posY + tileHeight / 2, tileWidth / 2.5, 0, Math.PI*2);
    ctx.stroke();
}

function drawStar(posX, posY) {
    ctx.beginPath();
    ctx.strokeStyle = "#808080"
    ctx.lineWidth = tileWidth * 0.05;
    ctx.moveTo(posX, posY);
    ctx.lineTo(posX + tileWidth, posY + tileHeight);
    ctx.stroke();
}

function drawPlayer(posX, posY, player = "Green") {
    ctx.beginPath();
    ctx.strokeStyle = cssColor(...playerColorDark[player]);
    ctx.lineWidth = tileWidth * 0.1;

    ctx.moveTo(posX + tileWidth / 2, posY + tileHeight * 1/4);
    ctx.lineTo(posX + tileWidth * 1/4, posY + tileHeight * 3/4);
    ctx.moveTo(posX + tileWidth / 2, posY + tileHeight * 1/4);
    ctx.lineTo(posX + tileWidth * 3/4, posY + tileHeight * 3/4);
    ctx.stroke();
}

function drawDice(posX, posY, num) {
    ctx.fillStyle = cssColor(255, 255, 255);
    ctx.fillRect(posX + tileWidth * 0.15, posY + tileHeight * 0.15, tileWidth * 0.7, tileHeight * 0.7)

    ctx.fillStyle = cssColor(0, 0, 0);

    switch(num) {
        case -1:
            ctx.font = Math.floor(tileWidth * 0.6) + "px Georgia";
            ctx.fillText("?", posX + tileWidth * 0.35, posY + tileHeight * 0.7, tileWidth);
            break;
        case 1:
            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.5, posY + tileHeight * 0.5, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();
            break;
        case 2:
            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.3, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.7, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();
            break;
        case 3:
            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.3, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.5, posY + tileHeight * 0.5, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.7, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();
            break;
        case 4:
            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.3, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.7, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.7, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.3, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();
            break;
        case 5:
            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.3, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.7, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.7, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.3, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.5, posY + tileHeight * 0.5, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();
            break;
        case 6:
            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.3, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.7, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.7, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.3, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.3, posY + tileHeight * 0.5, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();

            ctx.beginPath();
            ctx.arc(posX + tileWidth * 0.7, posY + tileHeight * 0.5, tileWidth * 0.08, 0, 2 * Math.PI);
            ctx.fill();
    }
}

function drawHighlight(posX, posY, gameState) {
    let color = playerColorDark[gameState.turn]
    ctx.beginPath();
    ctx.strokeStyle = cssColor(...color);
    ctx.lineWidth = tileWidth * 0.1;
    
    ctx.moveTo(posX, posY);
    ctx.lineTo(posX + tileWidth, posY);
    ctx.lineTo(posX + tileWidth, posY + tileHeight);
    ctx.lineTo(posX, posY + tileHeight)
    ctx.lineTo(posX, posY);
    ctx.stroke();
}

const boardPositions = [
    [1, 6], [2, 6], [3, 6], [4, 6], [5, 6],
    [6, 5], [6, 4], [6, 3], [6, 2], [6, 1],
    [6, 0], [7, 0], [8, 0],
    [8, 1], [8, 2], [8, 3], [8, 4], [8, 5],
    [9, 6], [10, 6], [11, 6], [12, 6], [13, 6],
    [14, 6], [14, 7], [14, 8],
    [13, 8], [12, 8], [11, 8], [10, 8], [9, 8],
    [8, 9], [8, 10], [8, 11], [8, 12], [8, 13], 
    [8, 14], [7, 14], [6, 14], 
    [6, 13], [6, 12], [6, 11], [6, 10], [6, 9], 
    [5, 8], [4, 8], [3, 8], [2, 8], [1, 8],
    [0, 8], [0, 7], [0, 6]
];

const homePositions = {
    Green: [[1, 7], [2, 7], [3, 7], [4, 7], [5, 7], [6, 7]],
    Yellow: [[7, 1], [7, 2], [7, 3], [7, 4], [7, 5], [7, 6]],
    Red: [[13, 7], [12, 7], [11, 7], [10, 7], [9, 7], [8, 7]],
    Blue: [[7, 13], [7, 12], [7, 11], [7, 10], [7, 9], [7, 8]],
}

const outPositions = {
    Green: [[2, 2], [3, 2], [2, 3], [3, 3]],
    Yellow: [[11, 2], [12, 2], [11, 3], [12, 3]],
    Blue: [[2, 11], [3, 11], [2, 12], [3, 12]],
    Red: [[11, 11], [12, 11], [11, 12], [12, 12]]
}

const startPosition = {
    Green: [1, 6],
    Yellow: [8, 1],
    Blue: [6, 13],
    Red: [13, 8]
}

const dicePositions =  [[7, 7]];

const starCells = [5, 11, 18, 24, 31, 37, 44, 50];
const globeCells = [0, 8, 13, 21, 26, 34, 39, 47];

const playerOffsets = {
    Green: 13 * 0,
    Yellow: 13 * 1,
    Red: 13 * 2,
    Blue: 13 * 3
}

function playerPosToTilePos(player, pos) {
    if (pos > 50) {
        return homePositions[player][pos - 51];
    }

    let newPos = (pos + playerOffsets[player]) % 52;
    return boardPositions[newPos];
}

var playerPos = 0;

function drawStaticBoard() {
    ctx.fillStyle = "#404040";
    ctx.fillRect(0, 0, c.width, c.height);

    for (let x = 0; x < numTilesX; x++) {
        for (let y = 0; y < numTilesY; y++) {
            drawTiles([[x, y]], drawSolid, 100, 100, 100)
        }
    }

    drawTiles(boardPositions, drawSolid, 255, 255, 255);

    drawTiles([...homePositions.Green, ...outPositions.Green, startPosition.Green], drawSolid, ...playerColor.Green);
    drawTiles([...homePositions.Yellow, ...outPositions.Yellow, startPosition.Yellow], drawSolid, ...playerColor.Yellow);
    drawTiles([...homePositions.Red, ...outPositions.Red, startPosition.Red], drawSolid, ...playerColor.Red);
    drawTiles([...homePositions.Blue, ...outPositions.Blue, startPosition.Blue], drawSolid, ...playerColor.Blue);

    drawTiles(globeCells.map((v, i) => boardPositions[v]), drawGlobe);
    drawTiles(starCells.map((v, i) => boardPositions[v]), drawStar);
}

function drawBoard(gameState, options, roll) {

    drawGameState = gameState; // save for window resize
    drawOptions = options;
    drawRoll = roll;

    drawStaticBoard();

    // Draw dice
    drawTiles(dicePositions, drawDice, roll);

    // Draw players
    for (let player in gameState.pieces) {
        let playerPieces = gameState.pieces[player];

        for (let pieceIndex in gameState.pieces[player]) {
            let piece = playerPieces[pieceIndex];

            if (piece.piece == "Out") {
                drawTiles([outPositions[player][pieceIndex - 1]], drawPlayer, player);
            }
            else if (piece.piece == "Active") {
                drawTiles([playerPosToTilePos(player, piece.field)], drawPlayer, player);
            }
        }
    }

    // Draw options
    let player = gameState.turn;
    switch (gameState.stage.stage) {
        case "Roll":
            drawTiles(dicePositions, drawHighlight, gameState);
        break;

        case "SelectPiece":
            for (let option of options) {
                if (option.option == "Play") {
                    drawTiles([outPositions[player][option.piece - 1]], drawHighlight, gameState);
                }
                else if (option.option == "Move") {
                    let pos = gameState.pieces[player][option.piece].field;
                    drawTiles([playerPosToTilePos(player, pos)], drawHighlight, gameState);
                }
            }

        break;

        case "SelectField":
            for (let option of options) {
                if (option.piece == gameState.stage.pieceIndex) {
                    if (option.option == "Play") {
                        drawTiles([startPosition[player]], drawHighlight, gameState);
                    }
                    else if (option.option == "Move") {
                        drawTiles([playerPosToTilePos(player, option.field)], drawHighlight, gameState);
                    }
                }
            }
        break;

        case "GameFinished":
        break;
    }
}

function posToField(posX, posY, player) {
    let tileX = Math.floor(posX / tileWidth);
    let tileY = Math.floor(posY / tileHeight);
    let tilePosStr = [tileX, tileY].toString();
 
    let toStrArr = (arr) => arr.map((v, i) => v.toString())
    
    // Check if field is on board
    let field = toStrArr(boardPositions).indexOf(tilePosStr);
    if (field != -1) {
        // Offset according to player
        field -= playerOffsets[player];

        return (field + 52) % 52;
    }
    
    // Check if field is in a home position
    field = toStrArr(homePositions[player]).indexOf(tilePosStr);
    if (field != -1) {
        return field + 51;
    }

    // Check if field is in an out position
    field = toStrArr(outPositions[player]).indexOf(tilePosStr);
    if (field != -1) {
        return field - 4;
    }

    // Check if field is dice
    field = toStrArr(dicePositions).indexOf(tilePosStr);
    if (field != -1) {
        return -5; // Clicked dice
    }

    // Field not found
    return null;
}