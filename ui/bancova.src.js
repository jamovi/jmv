'use strict';

var options = require("./bancova.options");

var layout = LayoutDef.extend({

    label: "Bayesian ANCOVA",
    type: "root",
    stage: 2,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { LayoutDef : layout, options: options };
