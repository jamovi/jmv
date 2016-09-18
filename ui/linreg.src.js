'use strict';

var options = require("./linreg.options");

var layout = LayoutDef.extend({

    label: "Linear Regression",
    type: "root",
    stage: 2,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { LayoutDef : layout, options: options };
