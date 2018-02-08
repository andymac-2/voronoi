'use strict'
var circumcentre = function (x1, y1, x2, y2, x3, y3) {
    var cx1 = 2 * (x3 - x1);
    var cx2 = 2 * (x3 - x2);
    var cy1 = 2 * (y3 - y1);
    var cy2 = 2 * (y3 - y2);
    var c1 = x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1;
    var c2 = x3 * x3 - x2 * x2 + y3 * y3 - y2 * y2;
    var determinant = cx1 * cy2 - cx2 * cy1;
    var x = Math.floor((cy2 * c1 - cy1 * c2) / determinant);
    var y = Math.floor((cx1 * c2 - cx2 * c1) / determinant);
    return {x: x, y: y};
}

var size = 10000
var sample = 1000
var specificity = 0;
var sensitivity = 0
var totaldiff = 0;
for (var i = 0; i < sample; ++i) {
    var x1 = Math.floor(Math.random() * size);
    var x2 = Math.floor(Math.random() * size);
    var x3 = Math.floor(Math.random() * size);
    var y1 = Math.floor(Math.random() * size);
    var y2 = Math.floor(Math.random() * size);
    var y3 = Math.floor(Math.random() * size);
    
    var point = circumcentre (x1, y1, x2, y2, x3, y3);
    
    var dx2 = (point.x - x1) * (point.x - x1)
    var dy2 = (point.y - y1) * (point.y - y1)
    
    point.y += Math.ceil(Math.sqrt(dx2 + dy2))
    
    var point2 = circumcentre (x1, y1, x2, y2, point.x, point.y)
    
    console.log (point2.x - point.x)
    
    /* if (Math.abs (x1 - x2) > Math.abs (y2 - y1) ){ // vertical
        var x = ((x2 * x2) - (x1 * x1) + (y2 * y2) - (y1 * y1) + 
            (point.y * (y1 - y2) * 2)) / ((x2 - x1) * 2)
        x = Math.floor (x)
        console.assert (Math.abs(x - point.x) <= 1, x - point.x)
    }
    else { // horizontal
        var y = ((y2 * y2) - (y1 * y1) + (x2 * x2) - (x1 * x1) + 
            (point.x * (x1 - x2) * 2)) / ((y2 - y1) * 2)
        y = Math.floor (y)
        console.assert (Math.abs(y - point.y) <= 1, y - point.y)
    } */
}