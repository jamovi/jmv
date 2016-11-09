'use strict';

var options = require("./corrmatrix.options");


var binomialTestLayout = ui.extend({

    label: "Correlation Matrix",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "variablesupplier",
            suggested: ["continuous", "ordinal" ],
            permitted: ["continuous", "ordinal", "nominal" ],
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    type: "variabletargetlistbox",
                    name: "vars",
                    showColumnHeaders: false,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 }
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
                    label: "Correlation Coefficients",
                    cell: [0, 0],
                    level: "2",
                    controls : [
                        { type:"checkbox", name: "pearson", label: "Pearson" },
                        { type:"checkbox", name: "spearman", label: "Spearman" },
                        { type:"checkbox", name: "kendall", label: "Kendall's tau-b" },
                    ]
                },
                {
                    type: "label",
                    cell: [1, 0],
                    controls : [
                        { type:"checkbox", name: "sig", label: "Report significance" },
                        { type:"checkbox", name: "flag", label: "Flag significant correlations" },
                        {
                            type:"checkbox", name: "ci", label: "Confidence intervals", controls: [
                                { type:"textbox", name: "ciWidth", label: "Interval", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" }
                            ]
                        }
                    ]
                },
                {
                    type: "label",
                    label: "Hypothesis",
                    level: "2",
                    cell: [0, 1],
                    controls : [
                        { type:"radiobutton", name: "hypothesis_corr", optionId: "hypothesis", checkedValue: "corr", label: "Correlated" },
                        { type:"radiobutton", name: "hypothesis_pos", optionId: "hypothesis", checkedValue: "pos", label: "Correlated positively" },
                        { type:"radiobutton", name: "hypothesis_neg", optionId: "hypothesis", checkedValue: "neg", label: "Correlated negatively" }
                    ]
                },
                {
                    type: "label",
                    label: "Plot",
                    level: "2",
                    cell: [1, 1],
                    controls : [
                        {
                            type:"checkbox", name: "plots", label: "Correlation matrix", controls: [
                                { type:"checkbox", name: "plotDens", label: "Densities for variables" },
                                { type:"checkbox", name: "plotStats", label: "Statistics" }
                            ]
                        }
                    ]
                }
            ]
        }
    ],

    actions: [
        {
            onChange : "plots", execute : function(ui) {
                var value = ui.plots.value();
                ui.plotDens.setEnabled(value);
                ui.plotStats.setEnabled(value);
            }
        },
        {
            onChange : "ci", execute : function(ui) {
                var value = ui.ci.value();
                ui.ciWidth.setEnabled(value);
            }
        }
    ]
});

module.exports = { ui : binomialTestLayout, options: options };
