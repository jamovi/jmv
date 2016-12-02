
'use strict';

const options = require("./reliability.options");

const layout = ui.extend({

    label: "Reliability Analysis",
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
            controls: [
                {
                    type: "label",
                    label: "Scale Statistics",
                    cell: [0, 0],
                    controls : [
                        { name: "alphaScale",   type:"checkbox", label: "Cronbach's α" },
                        { name: "omegaScale",   type:"checkbox", label: "McDonald's ω" },
                        { name: "meanScale",   type:"checkbox", label: "Mean" },
                        { name: "sdScale",   type:"checkbox", label: "Standard deviation" },
                    ]
                },
                {
                    type: "label",
                    label: "Item Statistics",
                    cell: [1, 0],
                    controls : [
                        { name: "alphaItems",   type:"checkbox", label: "Cronbach's α (if item is dropped)" },
                        { name: "omegaItems",   type:"checkbox", label: "McDonald's ω (if item is dropped)" },
                        { name: "meanItems",   type:"checkbox", label: "Mean" },
                        { name: "sdItems",   type:"checkbox", label: "Standard deviation" },
                        { name: "itemRestCor",   type:"checkbox", label: "Item-rest correlation" }
                    ]
                },
                {
                    type: "label",
                    label: "Additional Options",
                    cell: [0, 1],
                    controls : [
                        { name: "corPlot",   type:"checkbox", label: "Correlation heat plot" },
                    ]
                }
            ]
        }
    ],
    actions: []
});

module.exports = { ui : layout, options: options };
