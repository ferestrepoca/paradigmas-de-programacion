"use strict";

/**
 * @param {string} option value
 * @param {string} Question uid
 */
function Option(value, uid) {
    this.value = value;
    this.uid = uid;
    this.checked = false;
}


/**
 * Renders HTML.
 * @return {DOM Element}
 */
Option.prototype.render = function() {
    var label = document.createElement("label"),
        option = document.createElement("input"),
        self = this;

    option.value = this.value;
    option.type = "radio";
    option.name = this.uid;

    option.addEventListener("change", function(e) {
        self.checked = this.checked;
    });

    label.appendChild(option);
    label.appendChild(document.createTextNode(option.value));

    return label;
};


/**
 * @return {string}
 */
Option.prototype.getValue = function() {
    return this.value;
};


/**
 * @return {boolean}
 * ugly
 */
Option.prototype.isSelected = function() {
    var choice = document.querySelector("input[name='" + this.uid + "']:checked");

    return choice ? choice.value == this.value : false;
};
