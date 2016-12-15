'use strict';

var options = require("./mancova.options");

var layout = ui.extend({

    label: "MANCOVA",
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
