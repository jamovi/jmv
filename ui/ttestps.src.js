
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./ttestps.options')

var ttestpsLayout = LayoutDef.extend({

    label: "Paired Samples T-Test",
    type: "root",
    items: [
        {
            name: "group1",
            type: "supplier",
            cell: [0, 0],
            persistentItems: true,
            useVariables: true,
            stretchFactor: 1,
            items: [
                {
                    name: "pairs",
                    type:"targetlistbox",
                    label: "Paired Variables",
                    showColumnHeaders: false,
                    columns: [
                        { name: "i1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 },
                        { name: "i2", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            name: "group2",
            cell: [0, 1],
            stretchFactor: 1,
            items : [
                {
                    name: "column1",
                    cell: [0, 0],
                    stretchFactor: 1,
                    fitToGrid: false,
                    items : [
                        {
                            name: "column1-1",
                            label: "Tests",
                            level: "2",
                            items : [
                                { name: "student", type:"checkbox", label: "Student" },
                                { name: "wilcoxon", type:"checkbox", label: "Wilcoxon rank" },
                            ]
                        },
                        {
                            name: "column1-2",
                            label: "Hypothesis",
                            level: "2",
                            items : [
                                { name: "hypothesis_different", optionId: "hypothesis", type:"radiobutton", checkedValue: "different", label: "Measure 1 ≠ Measure 2" },
                                { name: "hypothesis_oneGreater", optionId: "hypothesis", type:"radiobutton", checkedValue: "oneGreater", label: "Measure 1 > Measure 2" },
                                { name: "hypothesis_twoGreater", optionId: "hypothesis", type:"radiobutton", checkedValue: "twoGreater", label: "Measure 1 < Measure 2" }
                            ]
                        },
                        {
                            name: "column1-3",
                            label: "Assumption Checks",
                            level: "2",
                            items : [
                                { name: "norm", type:"checkbox", label: "Normality" }
                            ]
                        }
                    ]
                },
                {
                    name: "column2",
                    cell: [1, 0],
                    stretchFactor: 1,
                    fitToGrid: false,
                    items : [
                        {
                            name: "column2-1",
                            label: "Additional Statistics",
                            level: "2",
                            items : [
                                { name: "meanDiff", type:"checkbox", label: "Mean difference" },
                                { name: "effectSize", type:"checkbox", label: "Effect size" },
                                {
                                    name: "groupA",
                                    label: { name: "ci", type:"checkbox", label: "Confidence interval" },
                                    items: [
                                        { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", formatName: "number", inputPattern: "[0-9]+" }
                                    ]
                                },
                                { name: "desc", type:"checkbox", label: "Descriptives" }
                            ]
                        },
                        {
                            name: "column2-2",
                            label: "Missing values",
                            level: "2",
                            items : [
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
        }
    ]
});

module.exports = { LayoutDef : ttestpsLayout, options: options };