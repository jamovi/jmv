
'use strict';

var options = require("./loglinear.options");
var view = require("./loglinear.actions");

view.layout = ui.extend({

    label: "Log-Linear Regression",
    type: "root",
    stage: 2,
    controls: [
        {
            type: "variablesupplier",
            suggested: ["continuous", "nominal", "ordinal"],
            name: "variableSupplier",
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    name: "counts",
                    type: "variabletargetlistbox",
                    label: "Counts (optional)",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    itemDropBehaviour: "overwrite",
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", stretchFactor: 1 }
                    ]
                },
                {
                    name: "factors",
                    type: "variabletargetlistbox",
                    label: "Factors",
                    showColumnHeaders: false,
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            name: "modelgroup",
            type: "collapsebox",
            label: "Model",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "supplier",
                    name: "modelSupplier",
                    label: "Componets",
                    persistentItems: true,
                    stretchFactor: 1,
                    controls: [
                        {
                            type:"targetlistbox",
                            name: "modelTerms",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            valueFilter: "unique",
                            itemDropBehaviour: "empty_space",
                            columns: [
                                { type: "listitem.termlabel", name: "column1", label: "", stretchFactor: 1 }
                            ]
                        }
                    ]
                }
            ]
        },
        {
            name: "statisticsgroup",
            type: "collapsebox",
            label: "Statistics",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "label",
                    label: "Regression Coefficients",
                    controls: [
                        { name: "est", type:"checkbox", label: "Estimates" },
                        {
                            name: "ci", type:"checkbox", label: "Confidence intervals" ,
                            controls: [ { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" } ]
                        }
                    ]
                }
            ]
        }
    ],

});

module.exports = { view : view, options: options };
