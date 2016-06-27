
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./friedman.options')

var friedmanLayout = LayoutDef.extend({

    label: "Friedman",
    type: "root",
    controls: [
        {
            type: "supplier",
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    type:"targetlistbox",
                    name: "measures",
                    label: "Measures",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "groupbox",
            controls : [
                { name: "pairs", type:"checkbox", label: "Pairwise Comparisons (Durbin-Conover)" },
                { name: "desc", type:"checkbox", label: "Descriptives" },
                {
                    name: "plots", type:"checkbox", label: "Descriptives Plot",
                    controls: [
                        { name: "plotType_means", optionId: "plotType", type:"radiobutton", checkedValue: "means", label: "Means" },
                        { name: "plotType_medians", optionId: "plotType", type:"radiobutton", checkedValue: "medians", label: "Medians" }
                    ]
                }
            ]
        }
    ],

    actions: [
        {
            onChange : "plots", execute : function(context) {
                var disabled = context.getValue("plots") === false;
                context.set("plotType_means", "disabled", disabled);
                context.set("plotType_medians", "disabled", disabled);
            }
        }
    ]
});

module.exports = { LayoutDef : friedmanLayout, options: options };
