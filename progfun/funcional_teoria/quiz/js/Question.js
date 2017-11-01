"use strict";

/**
 * @param {string} question to ask
 * @param {array} options to choose from
 * @param {integer} right option array index
 */
function Question(question, options, rightOption) {
    this.question = question;
    this.rightOption = rightOption;
    this.uid =  this._uid();
    this.options = this._createOptions(options);

    this.RNDR_RIGHT = "#6BBA70",
    this.RNDR_WRONG = "#D01F3C",
    this.RNDR_MISSING = "#C79810";
}


/**
 * @return {string} question id
 */
Question.prototype._uid = function() {
    // https://stackoverflow.com/questions/3242324/javascript-dateobj-gettime-for-a-uid-is-the-length-not-fixed
    return "q" + Math.random().toString(36).substr(2,9);
};


/**
 * @param  {array} options
 * @return {array} of Option objects
 */
Question.prototype._createOptions = function(_options) {
    var options = [];

    for (var i = 0; i < _options.length; i += 1) {
        options[i] = new Option(_options[i], this.uid);
    }

    return options;
}


/**
 * @return {array}
 */
Question.prototype.getOptions = function() {
    return this.options;
};


/**
 * @return {DOM Element}
 */
Question.prototype.render = function(shuffle) {
    var fieldset = document.createElement("fieldset"),
        legend = document.createElement("legend"),
        self = this;

    fieldset.id = this.uid;

    if (shuffle) {
        this._shuffleOptions();
    }

    legend.innerHTML = this.question;

    fieldset.appendChild(legend);

    for (var i = 0; i < this.options.length; i += 1) {
        fieldset.appendChild(this.options[i].render());
    }

    return fieldset;
};


Question.prototype.renderMissing = function() {
    document.getElementById(this.uid).style.borderColor = this.RNDR_MISSING;
};


Question.prototype.renderRight = function(first_argument) {
    document.getElementById(this.uid).style.borderColor = this.RNDR_RIGHT;
};


Question.prototype.renderWrong = function() {
    document.getElementById(this.uid).style.borderColor = this.RNDR_WRONG;
};

/**
 * @param  {Option}
 * @return {boolean}
 */
Question.prototype.isRight = function(option) {
    return option.getValue() == this.options[this.rightOption].value;
};


/**
 * Shuffles the options.
 */
Question.prototype._shuffleOptions = function() {
    var result = [],
        rightOption = this.options[this.rightOption].value; // obtain right option

    while (this.options.length) {
        var len = this.options.length,
            idx = parseInt(Math.random() * len); // find an index

        result.push(this.options.splice(idx, 1)[0]); // remove that item from array

        if (result[result.length - 1].value == rightOption) {
            this.rightOption = result.length - 1; //
        }
    }

    this.options = result; // switch arrays
}
