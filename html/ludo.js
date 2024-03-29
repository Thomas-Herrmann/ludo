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

const iconColor = [80, 80, 80];
const boardColor = [255, 255, 255];
const diceColor = boardColor;

function onHasteStart() {
    c = document.getElementById("canvas");
    c.width = c.scrollWidth;
    c.height = c.scrollHeight;
    ctx = c.getContext("2d");

    ctx.imageSmoothingEnabled = false;

    tileWidth = c.width / numTilesX;
    tileHeight = c.height / numTilesY;

    function drawBoardPrev() {
        drawBoard(drawGameState, drawOptions, drawRoll);
    }

    window.onresize = function(event) {    
        c.width = c.scrollWidth;
        c.height = c.scrollHeight;
        tileWidth = c.width / numTilesX;
        tileHeight = c.height / numTilesY;

        requestAnimationFrame(drawBoardPrev);
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
    ctx.strokeStyle = cssColor(...iconColor);
    ctx.lineWidth = tileWidth * 0.05;
    ctx.arc(posX + tileWidth / 2, posY + tileHeight / 2, tileWidth / 2.5, 0, Math.PI*2);
    ctx.stroke();
}

function drawStar(posX, posY) {
    let centerX = posX + tileWidth / 2;
    let centerY = posY + tileHeight / 2;

    const starOuterRadiusEven = tileWidth / 2 * 0.9;
    const starOuterRadiusOdd = tileWidth / 2 * 0.65;
    const starInnerRadius = tileWidth / 2 * 0.2;
    const numPoints = 12;

    const angInterval = Math.PI * 2 / numPoints;

    let getPoint = (ang, rad) => [centerX + Math.cos(ang) * rad, centerY + Math.sin(ang) * rad];

    ctx.beginPath();
    ctx.moveTo(...getPoint(0), starOuterRadiusEven);
    for (let i = 0; i < numPoints; i++) {
        let outerRadius = i % 2 == 0 ? starOuterRadiusEven : starOuterRadiusOdd;
        ctx.lineTo(...getPoint(angInterval * (i + 0.5), starInnerRadius));
        ctx.lineTo(...getPoint(angInterval * (i + 1), outerRadius));
    }
    ctx.fillStyle = cssColor(...iconColor);
    ctx.lineWidth = tileWidth * 0.05;
    ctx.fill();
}

function drawPlayer(posX, posY, player, gameState) {
    let field = posToField(posX, posY, player);
    let numPieces = Haste.numPiecesAt(gameState, player, field);

    ctx.beginPath();
    ctx.fillStyle = cssColor(...playerColorDark[player]);
    ctx.arc(posX + tileWidth * 0.5, posY + tileHeight * 0.5, tileWidth * 0.3, 0, 2 * Math.PI);
    ctx.fill();
    ctx.beginPath();
    ctx.fillStyle = cssColor(255, 255, 255);
    ctx.arc(posX + tileWidth * 0.5, posY + tileHeight * 0.5, tileWidth * 0.2, 0, 2 * Math.PI);
    ctx.fill();

    ctx.fillStyle = cssColor(...iconColor);
    ctx.font = "bold " + Math.floor(tileWidth * 0.35) + "px helvetica";
    ctx.fillText(numPieces.toString(), posX + tileWidth * 0.4, posY + tileHeight * 0.62, tileWidth);
}


function drawDiceAnimation(gameState, options, roll, prevRoll) {
    let posX = dicePositions[0][0] * tileWidth;
    let posY = dicePositions[0][1] * tileHeight;

    let angle = 0.0;
    let frameCount = 0;

    function animation() {
        ctx.setTransform(1, 0, 0, 1, 0, 0);
        ctx.clearRect(posX, posY, tileWidth, tileHeight);
        
        let transX = posX + tileWidth * 0.5;
        let transY = posY + tileHeight * 0.5;

        ctx.translate(transX, transY);
        ctx.rotate(angle);
        ctx.translate(-transX, -transY);

        drawDice(posX, posY, prevRoll);

        if (frameCount < 100) {
            ++frameCount;
            angle += (2 * Math.PI) / 100;
            requestAnimationFrame(animation);
        }
        else {
            ctx.setTransform(1, 0, 0, 1, 0, 0);
            drawBoard(drawGameState, drawOptions, drawRoll);
        }
    }

    drawGameState = gameState;
    drawOptions = options;
    drawRoll = roll;
    requestAnimationFrame(animation);
}


function drawDice(posX, posY, num) {
    
    function drawDot(mulX, mulY) {
        ctx.beginPath();
        ctx.arc(posX + tileWidth * mulX, posY + tileHeight * mulY, tileWidth * 0.08, 0, 2 * Math.PI);
        ctx.fill();
    }

    ctx.fillStyle = cssColor(...diceColor);
    ctx.fillRect(posX + tileWidth * 0.15, posY + tileHeight * 0.15, tileWidth * 0.7, tileHeight * 0.7)
    ctx.fillStyle = cssColor(...iconColor);

    switch(num) {
        case -1:
            ctx.font = "bold " + Math.floor(tileWidth * 0.6) + "px helvetica";
            ctx.fillText("?", posX + tileWidth * 0.33, posY + tileHeight * 0.7, tileWidth);
            break;
        case 1:
            drawDot(0.5, 0.5);
            break;
        case 2:
            drawDot(0.3, 0.3);
            drawDot(0.7, 0.7);
            break;
        case 3:
            drawDot(0.3, 0.3);
            drawDot(0.5, 0.5);
            drawDot(0.7, 0.7);
            break;
        case 4:
            drawDot(0.3, 0.3);
            drawDot(0.7, 0.7);
            drawDot(0.3, 0.7);
            drawDot(0.7, 0.3);
            break;
        case 5:
            drawDot(0.3, 0.3);
            drawDot(0.7, 0.7);
            drawDot(0.3, 0.7);
            drawDot(0.7, 0.3);
            drawDot(0.5, 0.5);
            break;
        case 6:
            drawDot(0.3, 0.3);
            drawDot(0.7, 0.7);
            drawDot(0.3, 0.7);
            drawDot(0.7, 0.3);
            drawDot(0.3, 0.5);
            drawDot(0.7, 0.5);
            break;
    }
}

function drawHighlight(posX, posY, color = [255, 255, 255]) {
    ctx.beginPath();
    ctx.strokeStyle = cssColor(...color);
    ctx.lineWidth = tileWidth * 0.1;
    
    ctx.strokeRect(posX + tileWidth * 0.05, posY + tileHeight * 0.05, tileWidth * 0.9, tileHeight * 0.9);
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

function drawStaticBoard() {
    ctx.clearRect(0, 0, c.width, c.height);

    /*for (let x = 0; x < numTilesX; x++) {
        for (let y = 0; y < numTilesY; y++) {
            drawTiles([[x, y]], drawSolid, 100, 100, 100, 0);
        }
    }*/

    drawTiles(boardPositions, drawSolid, ...boardColor);

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

    ctx.setTransform(1, 0, 0, 1, 0, 0);

    drawStaticBoard();

    // Draw dice
    drawTiles(dicePositions, drawDice, roll);

    // Draw players
    for (let player in gameState.pieces) {
        let playerPieces = gameState.pieces[player];

        for (let pieceIndex in gameState.pieces[player]) {
            let piece = playerPieces[pieceIndex];

            if (piece.piece == "Out") {
                drawTiles([outPositions[player][pieceIndex - 1]], drawPlayer, player, gameState);
            }
            else if (piece.piece == "Active") {
                drawTiles([playerPosToTilePos(player, piece.field)], drawPlayer, player, gameState);
            }
        }
    }

    // Draw options
    let player = gameState.turn;
    switch (gameState.stage.stage) {
        case "Roll":
            drawTiles(dicePositions, drawHighlight, playerColorDark[player]);
        break;

        case "SelectPiece":
            for (let option of options) {
                if (option.option == "Play") {
                    drawTiles([outPositions[player][option.piece - 1]], drawHighlight, playerColorDark[player]);
                }
                else if (option.option == "Move") {
                    let pos = gameState.pieces[player][option.piece].field;
                    drawTiles([playerPosToTilePos(player, pos)], drawHighlight, playerColorDark[player]);
                }
            }
        break;

        case "SelectField":
            for (let option of options) {
                if (option.piece == gameState.stage.pieceIndex) {
                    if (option.option == "Play") {
                        drawTiles([startPosition[player]], drawHighlight, playerColorDark[player]);
                    }
                    else if (option.option == "Move") {
                        drawTiles([playerPosToTilePos(player, option.field)], drawHighlight, playerColorDark[player]);
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