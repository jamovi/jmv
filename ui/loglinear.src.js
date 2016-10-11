
'use strict';

var options = require("./loglinear.options");

var layout = LayoutDef.extend({

    label: "LogLinear",
    type: "root",
    stage: 0,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { LayoutDef : layout, options: options };
