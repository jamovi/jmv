'use strict';

var options = require("./corrmatrix.options");


var binomialTestLayout = ui.extend({

    label: "Correlation Matrix",
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
                    type:"variabletargetlistbox",
                    name: "vars",
                    showColumnHeaders: false,
                    variableFilter: ["ordinal", "continuous"],
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
                            type:"checkbox", name: "plot", label: "Correlation matrix", controls: [
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
            onChange : "plot", execute : function(context) {
                var disabled = context.getValue("plot") === false;
                context.set("plotDens",  "disabled", disabled);
                context.set("plotStats", "disabled", disabled);
            }
        },
        {
            onChange : "ci", execute : function(context) {
                var disabled = context.getValue("ci") === false;
                context.set("ciWidth", "disabled", disabled);
            }
        }
    ]
});

module.exports = { ui : binomialTestLayout, options: options };
