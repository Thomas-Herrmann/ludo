var gameState;

var c, ctx;

const canvasScale = 50;
const numTilesX = 15;
const numTilesY = 15;

const colorGreen = [3, 166, 120];
const colorYellow = [242, 159, 5];
const colorRed = [242, 7, 70];
const colorBlue = [4, 196, 217];
const colorDarkGreen = [2, 132, 95];
const colorDarkYellow = [193, 127, 4];
const colorDarkRed = [193, 5, 56];
const colorDarkBlue = [3, 156, 173];

var tileWidth = 50;
var tileHeight = 50;


window.onload = function() {
    c = document.getElementById("canvas");
    c.width = numTilesX * canvasScale;
    c.height = numTilesY * canvasScale;
    ctx = c.getContext("2d");

    ctx.imageSmoothingEnabled = false;

    tileWidth = c.width / numTilesX;
    tileHeight = c.height / numTilesY;

    drawBoard();
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

function drawSolid(posX, posY, r, g, b, border = 1) {
    ctx.fillStyle = cssColor(r, g, b);
    ctx.fillRect(posX + border, posY + border, tileWidth - border * 2, tileHeight - border * 2);
}

function drawGlobe(posX, posY) {
    ctx.beginPath();
    ctx.strokeStyle = "#808080"
    ctx.lineWidth = 3;
    ctx.arc(posX + tileWidth / 2, posY + tileHeight / 2, tileWidth / 2.5, 0, Math.PI*2);
    ctx.stroke();
}

function drawStar(posX, posY) {
    ctx.beginPath();
    ctx.strokeStyle = "#808080"
    ctx.lineWidth = 3;
    ctx.moveTo(posX, posY);
    ctx.lineTo(posX + tileWidth, posY + tileHeight);
    ctx.stroke();
}

function drawPlayer(posX, posY, color = "Green") {
    ctx.beginPath();

    switch (color) {
        case "Blue": ctx.strokeStyle = cssColor(...colorDarkBlue); break;
        case "Red": ctx.strokeStyle = cssColor(...colorDarkRed); break;
        case "Yellow": ctx.strokeStyle = cssColor(...colorDarkYellow); break;
        default: ctx.strokeStyle = cssColor(...colorDarkGreen); break;
    }

    ctx.lineWidth = 6;

    ctx.moveTo(posX + tileWidth / 2, posY + tileHeight * 1/4);
    ctx.lineTo(posX + tileWidth * 1/4, posY + tileHeight * 3/4);
    ctx.moveTo(posX + tileWidth / 2, posY + tileHeight * 1/4);
    ctx.lineTo(posX + tileWidth * 3/4, posY + tileHeight * 3/4);
    ctx.stroke();
}

function drawDie(posX, posY, num) {
    ctx.fillStyle = "#ffffff";
    ctx.font = Math.floor(tileWidth * 0.8) + "px Georgia";
    ctx.fillText(num.toString(), posX + tileWidth * 1/4, posY + tileHeight * 3/5, tileWidth);
}

var boardPositions = [
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

var greenHomePositions = [[1, 7], [2, 7], [3, 7], [4, 7], [5, 7], [6, 7]];
var yellowHomePositions = [[7, 1], [7, 2], [7, 3], [7, 4], [7, 5], [7, 6]];
var redHomePositions = [[13, 7], [12, 7], [11, 7], [10, 7], [9, 7], [8, 7]];
var blueHomePositions = [[7, 13], [7, 12], [7, 11], [7, 10], [7, 9], [7, 8]];

var greenOutPositions = [[2, 2], [3, 2], [2, 3], [3, 3]];
var yellowOutPositions = [[11, 2], [12, 2], [11, 3], [12, 3]];
var blueOutPositions = [[2, 11], [3, 11], [2, 12], [3, 12]];
var redOutPositions = [[11, 11], [12, 11], [11, 12], [12, 12]];

var starCells = [5, 11, 18, 24, 31, 37, 44, 50];
var globeCells = [0, 8, 13, 21, 26, 34, 39, 47];

function playerPosToTilePos(player, pos) {
    let homePositions = [];
    let offset = 0;

    switch (player) {
        case "Green": homePositions = greenHomePositions; offset = 13 * 0; break;
        case "Yellow": homePositions = yellowHomePositions; offset = 13 * 1; break;
        case "Red": homePositions = redHomePositions; offset = 13 * 2; break;
        case "Blue": homePositions = blueHomePositions; offset = 13 * 3; break;
    }

    if (pos > 50) {
        return homePositions[pos - 51];
    }

    let newPos = (pos + offset) % 52;
    return boardPositions[newPos];
}

var playerPos = 0;

function drawBoard(gameState, options, rolls) {
    ctx.fillStyle = "#404040";
    ctx.fillRect(0, 0, c.width, c.height);

    for (let x = 0; x < numTilesX; x++) {
        for (let y = 0; y < numTilesY; y++) {
            drawTiles([[x, y]], drawSolid, 100, 100, 100)
        }
    }

    drawTiles(boardPositions, drawSolid, 255, 255, 255);

    drawTiles([...greenHomePositions, ...greenOutPositions, [1, 6]], drawSolid, ...colorGreen);
    drawTiles([...yellowHomePositions, ...yellowOutPositions, [8, 1]], drawSolid, ...colorYellow);
    drawTiles([...redHomePositions, ...redOutPositions, [13, 8]], drawSolid, ...colorRed);
    drawTiles([...blueHomePositions, ...blueOutPositions, [6, 13]], drawSolid, ...colorBlue);

    drawTiles(globeCells.map((v, i) => boardPositions[v]), drawGlobe);
    drawTiles(starCells.map((v, i) => boardPositions[v]), drawStar);

    drawTiles(greenOutPositions, drawPlayer, "Green");
    drawTiles(yellowOutPositions, drawPlayer, "Yellow");
    drawTiles(redOutPositions, drawPlayer, "Red");
    drawTiles(blueOutPositions, drawPlayer, "Blue");

    drawTiles([[4, 4]], drawDie, 4);
    drawTiles([[10, 4]], drawDie, 6);
    drawTiles([[10, 10]], drawDie, 1);
    drawTiles([[4, 10]], drawDie, 3);

    drawTiles([playerPosToTilePos("Green", playerPos)], drawPlayer, "Green")
    drawTiles([playerPosToTilePos("Yellow", playerPos)], drawPlayer, "Yellow")
    drawTiles([playerPosToTilePos("Red", playerPos)], drawPlayer, "Red")
    drawTiles([playerPosToTilePos("Blue", playerPos)], drawPlayer, "Blue")

    playerPos++;
    setTimeout(() => drawBoard(), 200);
}

function posToField(posX, posY, player) {
    tileX = Math.floor(posX / tileWidth);
    tileY = Math.floor(posY / tileHeight);

    
}