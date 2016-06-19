
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./kruskal.options')

var kruskalLayout = LayoutDef.extend({

    label: "Kruskal-Wallis",
    type: "root",
    items: [
        {
            name: "group1",
            type: "supplier",
            cell: [0, 0],
            persistentItems: false,
            useVariables: true,
            stretchFactor: 1,
            items: [
                {
                    name: "deps",
                    type:"targetlistbox",
                    label: "Dependent Variables",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "group",
                    type:"targetlistbox",
                    label: "Grouping Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                }
            ]
        },
        { name: "pairs", type:"checkbox", label: "DSCF Pairwise Comparisons" }
    ]
});

module.exports = { LayoutDef : kruskalLayout, options: options };
