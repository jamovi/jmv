
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./ttestis.options')

var ttestisLayout = ui.extend({

    label: "Independent Samples T-Test",
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
                    label: "Dependent Variables",
                    showColumnHeaders: false,
                    variableFilter: ["continuous"],
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                },
                {
                    name: "group",
                    type:"variabletargetlistbox",
                    label: "Grouping Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    variableFilter: ["nominal", "ordinal"],
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "layoutbox",
            stretchFactor: 1,
            margin: "large",
            controls : [
                {
                    type: "layoutbox",
                    name: "column1",
                    cell: [0, 0],
                    stretchFactor: 1,
                    controls : [
                        {
                            type: "label",
                            label: "Tests",
                            controls : [
                                { type:"checkbox", name: "students", label: "Student's", controls: [
                                    {
                                        type:"checkbox",
                                        name: "bf",
                                        label: "Bayes factor",
                                        controls: [
                                            { type:"textbox", name: "bfPrior", label: "Prior", format: FormatDef.number, inputPattern: "[0-9]+" }
                                        ]
                                    },
                                ]},
                                { type:"checkbox", name: "welchs", label: "Welch's" },
                                { type:"checkbox", name: "mann", label: "Mann-Whitney U" }
                            ]
                        },
                        {
                            type: "label",
                            label: "Hypothesis",
                            controls : [
                                { type:"radiobutton", name: "hypothesis_different", optionId: "hypothesis", checkedValue: "different", label: "Group 1 ≠ Group 2" },
                                { type:"radiobutton", name: "hypothesis_oneGreater", optionId: "hypothesis", checkedValue: "oneGreater", label: "Group 1 > Group 2" },
                                { type:"radiobutton", name: "hypothesis_twoGreater", optionId: "hypothesis", checkedValue: "twoGreater", label: "Group 1 < Group 2" }
                            ]
                        },
                        {
                            type: "label",
                            label: "Assumption Checks",
                            controls : [
                                { type:"checkbox", name: "norm", label: "Normality" },
                                { type:"checkbox", name: "eqv", label: "Equality of variances" }
                            ]
                        }
                    ]
                },
                {
                    type: "layoutbox",
                    name: "column2",
                    cell: [1, 0],
                    stretchFactor: 1,
                    controls : [
                        {
                            type: "label",
                            label: "Additional Statistics",
                            controls : [
                                { type:"checkbox", name: "meanDiff", label: "Mean difference" },
                                { type:"checkbox", name: "effectSize", label: "Effect size" },
                                {
                                    type:"checkbox", name: "ci", label: "Confidence interval",
                                    controls: [
                                        { type:"textbox", name: "ciWidth", label: "Interval", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" }
                                    ]
                                },
                                { type:"checkbox", name: "desc", label: "Descriptives" },
                                { type:"checkbox", name: "plots", label: "Descriptives plots" },
                            ]
                        },
                        {
                            type: "label",
                            label: "Missing values",
                            controls : [
                                { type:"radiobutton", name: "miss_perAnalysis", optionId: "miss", checkedValue: "perAnalysis", label: "Exclude cases analysis by analysis" },
                                { type:"radiobutton", name: "miss_listwise", optionId: "miss", checkedValue: "listwise", label: "Exclude cases listwise" }
                            ]
                        }
                    ]
                }
            ]
        }
    ],


    actions: [
        {
            onChange : "ci", execute : function(context) {
                var disabled = context.getValue("ci") === false;
                context.set("ciWidth", "disabled", disabled);
            }
        },
        {
            onChange : "bf", execute : function(context) {
                var disabled = context.getValue("bf") === false;
                context.set("bfPrior", "disabled", disabled);
            }
        }
    ]
});

module.exports = { ui : ttestisLayout, options: options };
