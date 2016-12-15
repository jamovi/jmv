'use strict';

var options = require("./linreg.options");

var layout = ui.extend({

    label: "Linear Regression",
    type: "root",
    stage: 2,
    controls: [

    ]
});

var actions = Actions.extend({
    events: [

    ]
});

module.exports = { ui : layout, actions: actions, options: options };
