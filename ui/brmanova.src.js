'use strict';

var options = require("./brmanova.options");

var layout = LayoutDef.extend({

    label: "Bayesian Repeated Measures ANOVA",
    type: "root",
    stage: 2,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { LayoutDef : layout, options: options };
