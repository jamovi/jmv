
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./kruskal.options')

var kruskalLayout = LayoutDef.extend({

    label: "Kruskal-Wallis",
    type: "root",
    controls: [
        {
            type: "supplier",
            persistentItems: false,
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    type:"targetlistbox",
                    name: "deps",
                    label: "Dependent Variables",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "group",
                    label: "Grouping Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        { type:"checkbox", name: "pairs", label: "DSCF Pairwise Comparisons" }
    ]
});

module.exports = { LayoutDef : kruskalLayout, options: options };
