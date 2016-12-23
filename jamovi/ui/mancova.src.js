'use strict';

var options = require("./mancova.options");

const view = View.extend({

    initialize: function(ui) {

    },

    events: [

    ]
});

view.layout = ui.extend({

    label: "MANCOVA",
    type: "root",
    stage: 2,
    controls: [

    ]
});


module.exports = { view : view, options: options };
