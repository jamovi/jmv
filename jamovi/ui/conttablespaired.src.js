
'use strict';

var options = require("./conttablespaired.options");

var layout = ui.extend({

    label: "Paired Samples Contingency Tables",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "variablesupplier",
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    type:"variabletargetlistbox",
                    name: "rows",
                    label: "Rows",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    itemDropBehaviour: "overwrite",
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
                    itemDropBehaviour: "overwrite",
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
                    itemDropBehaviour: "overwrite",
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
    ]
});

var actions = Actions.extend({
    events : [

    ]
});

module.exports = { ui : layout, actions: actions, options: options };
