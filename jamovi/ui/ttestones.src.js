
'use strict';

const options = require('./ttestones.options');

const ttestonesLayout = ui.extend({

    label: "One Sample T-Test",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "variablesupplier",
            suggested: ["continuous"],
            permitted: ["continuous", "ordinal", "nominal" ],
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    type: "variabletargetlistbox",
                    name: "vars",
                    label: "Dependent Variables",
                    showColumnHeaders: false,
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
                            level: "2",
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
                                { type:"checkbox", name: "mann", label: "Mann-Whitney U" }
                            ]
                        },
                        {
                            type: "label",
                            label: "Hypothesis",
                            level: "2",
                            controls : [
                                {
                                    type: "layoutbox",
                                    controls: [{ type:"textbox", name: "testValue", label: "Test value", format: FormatDef.number, inputPattern: "[0-9]+" }]
                                },
                                { name: "hypothesis_dt", optionId: "hypothesis", type:"radiobutton", checkedValue: "dt", label: "≠ Test value" },
                                { name: "hypothesis_gt", optionId: "hypothesis", type:"radiobutton", checkedValue: "gt", label: "> Test value" },
                                { name: "hypothesis_lt", optionId: "hypothesis", type:"radiobutton", checkedValue: "lt", label: "< Test value" }
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
                            label: "Assumption Checks",
                            level: "2",
                            controls : [
                                { name: "norm", type:"checkbox", label: "Normality" }
                            ]
                        }
                    ]
                }
            ]
        }
    ],

    actions: [
        {
            onChange : "ci", execute : function(ui) {
                ui.ciWidth.setEnabled(ui.ci.value());
            }
        },
        {
            onChange : "bf", execute : function(ui) {
                ui.bfPrior.setEnabled(ui.bf.value());
            }
        }
    ]
});

module.exports = { ui : ttestonesLayout, options: options };
