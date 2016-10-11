
'use strict';

var options = require("./conttablespaired.options");

var layout = ui.extend({

    label: "Paired Samples Contingency Tables",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "supplier",
            persistentItems: true,
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    type:"targetlistbox",
                    name: "pairs",
                    label: "Paired Variables",
                    showColumnHeaders: false,
                    fullRowSelect: true,
                    columns: [
                        { type: "listitem.variablelabel", name: "i1", label: "", format: FormatDef.variable, stretchFactor: 1 },
                        { type: "listitem.variablelabel", name: "i2", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "layoutbox",
            label: "Cells",
            margin: "large",
            controls : [
                { type:"checkbox", name: "areCounts", label: "Values are counts" },
                {
                    type: "label",
                    label: "Percentages",
                    controls : [
                        { type:"checkbox", name: "pcRow", label: "Row" },
                        { type:"checkbox", name: "pcCol", label: "Column" },
                    ]
                }
            ]
        }
    ],

    actions: [

    ]
});

module.exports = { ui : layout, options: options };
