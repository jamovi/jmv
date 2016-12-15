
'use strict';

const options = require('./anovanp.options');

const kruskalLayout = ui.extend({

    label: "One-way ANOVA (Non-parametric)",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "variablesupplier",
            suggested: ["continuous", "ordinal", "nominal" ],
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    type: "variabletargetlistbox",
                    name: "deps",
                    label: "Dependent Variables",
                    showColumnHeaders: false,
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                },
                {
                    type: "variabletargetlistbox",
                    name: "group",
                    label: "Grouping Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    itemDropBehaviour: "overwrite",
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "layoutbox",
            margin: "large",
            controls: [
                { type:"checkbox", name: "pairs", label: "DSCF pairwise comparisons" }
            ]
        }
    ]
});

var actions = Actions.extend({
    events: [

    ]
});

module.exports = { ui : kruskalLayout, actions: actions, options: options };
