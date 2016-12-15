'use strict';

var options = require("./banova.options");

var layout = ui.extend({

    label: "Bayesian ANOVA",
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
