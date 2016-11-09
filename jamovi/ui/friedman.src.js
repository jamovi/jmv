
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./friedman.options')

var friedmanLayout = ui.extend({

    label: "Friedman",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "variablesupplier",
            suggested: ["continuous" ],
            permitted: ["continuous", "nominal", "ordinal" ],
            stretchFactor: 1,
            controls: [
                {
                    type: "variabletargetlistbox",
                    name: "measures",
                    label: "Measures",
                    showColumnHeaders: false,
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "layoutbox",
            margin: "large",
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
            onChange : "plots", execute : function(ui) {
                var value = ui.plots.value();
                ui.plotType_means.setEnabled(value);
                ui.plotType_medians.setEnabled(value);
            }
        }
    ]
});

module.exports = { ui : friedmanLayout, options: options };
