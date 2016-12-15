'use strict';

var options = require("./brmanova.options");

var layout = ui.extend({

    label: "Bayesian Repeated Measures ANOVA",
    type: "root",
    stage: 2,
    controls: [

    ]
});

var actions = Actions.extend({
    events : [

    ]
});

module.exports = { ui : layout, actions: actions, options: options };
