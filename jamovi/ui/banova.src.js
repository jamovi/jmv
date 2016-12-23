'use strict';

var options = require("./banova.options");

const view = View.extend({

    initialize: function(ui) {

    },

    events: [

    ]
});

view.layout = ui.extend({

    label: "Bayesian ANOVA",
    type: "root",
    stage: 2,
    controls: [

    ]
});


module.exports = { view : view, options: options };
