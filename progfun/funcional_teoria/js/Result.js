"use strict";


/**
 * @param {integer} missing
 * @param {integer} right
 * @param {integer} wrong
 */
function Result(missing, right, wrong) {
    this.html = document.createElement("table");

    var t = ["Sin respuesta", "Correctas", "Erroneas"]

    var labels = ["Missing", "Right", "Wrong"],
        tr = document.createElement("tr");

    for (var i = 0; i < arguments.length; i += 1) {
        var label = document.createElement("td"),
            score = document.createElement("td");

        label.className = labels[i].toLowerCase() + "-label";
        score.className = labels[i].toLowerCase() + "-score";

        label.innerHTML = t[i];
        score.innerHTML = arguments[i];

        tr.appendChild(label);
        tr.appendChild(score);
    }

    this.html.appendChild(tr);
}


Result.prototype.render = function() {
    var div = document.getElementById("results");
    var old = div.getElementsByTagName("table")[0];
    if (old) {
        div.removeChild(old);
    }

    div.appendChild(this.html);

    return this;
};
