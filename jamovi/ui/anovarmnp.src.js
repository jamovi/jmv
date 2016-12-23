
'use strict';

const options = require('./anovarmnp.options');

const view = View.extend({

    initialize: function(ui) {
        var value = ui.plots.value();
        ui.plotType_means.setEnabled(value);
        ui.plotType_medians.setEnabled(value);
    },

    events: [
        {
            onChange : "plots", execute : function(ui) {
                var value = ui.plots.value();
                ui.plotType_means.setEnabled(value);
                ui.plotType_medians.setEnabled(value);
            }
        }
    ]
});

view.layout = ui.extend({

    label: "Repeated Measures ANOVA (Non-parametric)",
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
                { name: "pairs", type:"checkbox", label: "Pairwise comparisons (Durbin-Conover)" },
                { name: "desc", type:"checkbox", label: "Descriptives" },
                {
                    name: "plots", type:"checkbox", label: "Descriptives plot",
                    controls: [
                        { name: "plotType_means", optionId: "plotType", type:"radiobutton", checkedValue: "means", label: "Means" },
                        { name: "plotType_medians", optionId: "plotType", type:"radiobutton", checkedValue: "medians", label: "Medians" }
                    ]
                }
            ]
        }
    ]
});


module.exports = { view : view, options: options };
