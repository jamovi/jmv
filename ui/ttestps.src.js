
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./ttestps.options')

var ttestpsLayout = ui.extend({

    label: "Paired Samples T-Test",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "variablesupplier",
            suggested: ["continuous"],
            permitted: ["continuous", "ordinal", "nominal" ],
            persistentItems: true,
            stretchFactor: 1,
            controls: [
                {
                    type: "variabletargetlistbox",
                    name: "pairs",
                    label: "Paired Variables",
                    showColumnHeaders: false,
                    fullRowSelect: true,
                    rowDataAsArray: true,
                    columns: [
                        { type: "listitem.variablelabel", name: "i1", label: "", format: FormatDef.variable, stretchFactor: 1 },
                        { type: "listitem.variablelabel", name: "i2", label: "", format: FormatDef.variable, stretchFactor: 1 }
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
                            level: "2",
                            controls : [
                                { name: "students", type:"checkbox", label: "Student's", controls: [
                                    {
                                        type:"checkbox",
                                        name: "bf",
                                        label: "Bayes factor",
                                        controls: [
                                            { type:"textbox", name: "bfPrior", label: "Prior", format: FormatDef.number, inputPattern: "[0-9]+" }
                                        ]
                                    },
                                ]},
                                { name: "wilcoxon", type:"checkbox", label: "Wilcoxon rank" },
                            ]
                        },
                        {
                            type: "label",
                            label: "Hypothesis",
                            level: "2",
                            controls : [
                                { name: "hypothesis_different", optionId: "hypothesis", type:"radiobutton", checkedValue: "different", label: "Measure 1 ≠ Measure 2" },
                                { name: "hypothesis_oneGreater", optionId: "hypothesis", type:"radiobutton", checkedValue: "oneGreater", label: "Measure 1 > Measure 2" },
                                { name: "hypothesis_twoGreater", optionId: "hypothesis", type:"radiobutton", checkedValue: "twoGreater", label: "Measure 1 < Measure 2" }
                            ]
                        },
                        {
                            type: "label",
                            label: "Assumption Checks",
                            level: "2",
                            controls : [
                                { name: "norm", type:"checkbox", label: "Normality" }
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
                            level: "2",
                            controls : [
                                { name: "meanDiff", type:"checkbox", label: "Mean difference" },
                                { name: "effectSize", type:"checkbox", label: "Effect size" },
                                {
                                    name: "ci", type:"checkbox", label: "Confidence interval",
                                    controls: [
                                        { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" }
                                    ]
                                },
                                { name: "desc", type:"checkbox", label: "Descriptives" },
                                { name: "plots", type:"checkbox", label: "Descriptives plots" }
                            ]
                        },
                        {
                            type: "label",
                            label: "Missing values",
                            level: "2",
                            controls : [
                                { name: "miss_perAnalysis", optionId: "miss", type:"radiobutton", checkedValue: "perAnalysis", label: "Exclude cases analysis by analysis" },
                                { name: "miss_listwise", optionId: "miss", type:"radiobutton", checkedValue: "listwise", label: "Exclude cases listwise" }
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

module.exports = { ui : ttestpsLayout, options: options };
