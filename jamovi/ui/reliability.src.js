
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

'use strict';

var options = require("./reliability.options");

var layout = ui.extend({

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
                    type: "layoutbox",
                    name: "column1",
                    cell: [0, 0],
                    controls : [
                        {
                            type: "label",
                            label: "Scale Statistics",
                            controls : [
                                { name: "alphaScale",   type:"checkbox", label: "Cronbach's α" },
                                { name: "omegaScale",   type:"checkbox", label: "McDonald's ω" },
                                { name: "meanScale",   type:"checkbox", label: "Mean" },
                                { name: "sdScale",   type:"checkbox", label: "Standard deviation" }
                            ]
                        }
                    ]
                },
                {
                    type: "layoutbox",
                    name: "column2",
                    cell: [1, 0],
                    controls : [
                        {
                            type: "label",
                            label: "Item Statistics",
                            controls : [
                                { name: "alphaItems",   type:"checkbox", label: "Cronbach's α (if item is dropped)" },
                                { name: "omegaItems",   type:"checkbox", label: "McDonald's ω (if item is dropped)" },
                                { name: "meanItems",   type:"checkbox", label: "Mean" },
                                { name: "sdItems",   type:"checkbox", label: "Standard deviation" },
                                { name: "itemRestCor",   type:"checkbox", label: "Item-rest correlation" }
                            ]
                        }
                    ]
                }
            ]
        }
    ],
    actions: []
});

module.exports = { ui : layout, options: options };
