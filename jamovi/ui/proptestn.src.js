
'use strict';

const options = require("./proptestn.options");

const layout = ui.extend({

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
                   name: "var",
                   label: "Variable",
                   maxItemCount: 1,
                   showColumnHeaders: false,
                   fullRowSelect: true,
                   columns: [
                       { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                   ]
                },
                {
                   type: "variabletargetlistbox",
                   name: "counts",
                   label: "Counts (optional)",
                   maxItemCount: 1,
                   showColumnHeaders: false,
                   fullRowSelect: true,
                   columns: [
                       { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                   ]
                }           ]
        }   ],

    actions: []
});

module.exports = { ui : layout, options: options };
