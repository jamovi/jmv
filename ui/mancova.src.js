'use strict';

var options = require("./mancova.options");

var layout = LayoutDef.extend({

    label: "MANCOVA",
    type: "root",
    stage: 2,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { LayoutDef : layout, options: options };
