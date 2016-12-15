'use strict';

var options = require("./bancova.options");

var layout = ui.extend({

    label: "Bayesian ANCOVA",
    type: "root",
    stage: 2,
    controls: [

    ]
});

var actions = Actions.extend({
    events: [

    ]
});

module.exports = { ui : layout, actions: actions, options: options };
