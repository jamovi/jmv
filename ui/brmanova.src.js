'use strict';

var options = require("./brmanova.options");

var layout = ui.extend({

    label: "Bayesian Repeated Measures ANOVA",
    type: "root",
    stage: 2,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { ui : layout, options: options };
