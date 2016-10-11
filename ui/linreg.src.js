'use strict';

var options = require("./linreg.options");

var layout = ui.extend({

    label: "Linear Regression",
    type: "root",
    stage: 2,
    controls: [

    ],

    actions: [

    ]
});

module.exports = { ui : layout, options: options };
