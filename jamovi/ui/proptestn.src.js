
'use strict';

var options = require("./proptestn.options");

var layout = ui.extend({

    label: "Proportion Test (N Outcomes)",
    type: "root",
    stage: 0,
    controls: [
        {
           type: "variablesupplier",
           persistentItems: false,
           stretchFactor: 1,
           controls: [
                {
                   type: "variabletargetlistbox",
                   name: "vars",
                   label: "Variables",
                   showColumnHeaders: false,
                   fullRowSelect: true,
                   columns: [
                       { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                   ]
                }
           ]
       },
       {
           type: "layoutbox",
           margin: "large",
           controls : [
               { type:"checkbox", name: "areCounts", label: "Values are counts" },
           ]
       }    ],

    actions: []
});

module.exports = { ui : layout, options: options };
