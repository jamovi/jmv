
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./kruskal.options')

var kruskalLayout = ui.extend({

    label: "Kruskal-Wallis",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "supplier",
            persistentItems: false,
            useVariables: true,
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
                { type:"checkbox", name: "pairs", label: "DSCF Pairwise Comparisons" }
            ]
        }
    ]
});

module.exports = { ui : kruskalLayout, options: options };
