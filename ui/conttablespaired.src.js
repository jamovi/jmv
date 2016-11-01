
'use strict';

var options = require("./conttablespaired.options");

var layout = ui.extend({

    label: "Paired Samples Contingency Tables",
    type: "root",
    stage: 2,
    controls: [
        {
            type: "variablesupplier",
            persistentItems: true,
            stretchFactor: 1,
            suggested: ["continuous", "nominal", "ordinal"],
            controls: [
                {
                    type:"variabletargetlistbox",
                    name: "rows",
                    label: "Rows",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 },
                    ]
                },
                {
                    type:"variabletargetlistbox",
                    name: "cols",
                    label: "Columns",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 },
                    ]
                },
                {
                    type:"variabletargetlistbox",
                    name: "counts",
                    label: "Counts (optional)",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 },
                    ]
                },
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
