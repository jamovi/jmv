'use strict';

var options = require("./linreg.options");

const view = View.extend({

    initialize: function(ui) {

    },

    events: [

    ]
});

view.layout = ui.extend({

    label: "Linear Regression",
    type: "root",
    stage: 2,
    controls: [

    ]
});

module.exports = { view : view, options: options };
