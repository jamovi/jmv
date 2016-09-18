'use strict';

var options = require("./banova.options");

var layout = LayoutDef.extend({

    label: "Bayesian ANOVA",
    type: "root",
    stage: 2,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { LayoutDef : layout, options: options };
