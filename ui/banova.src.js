'use strict';

var options = require("./banova.options");

var layout = ui.extend({

    label: "Bayesian ANOVA",
    type: "root",
    stage: 2,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { ui : layout, options: options };
