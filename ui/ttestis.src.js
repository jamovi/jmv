
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./ttestis.options')

var ttestisLayout = LayoutDef.extend({

    label: "Independent Samples T-Test",
    type: "root",
    controls: [
        {
            type: "supplier",
            persistentItems: false,
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    type:"targetlistbox",
                    name: "vars",
                    label: "Dependent Variables",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "group",
                    type:"targetlistbox",
                    label: "Grouping Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            stretchFactor: 1,
            controls : [
                {
                    name: "column1",
                    cell: [0, 0],
                    stretchFactor: 1,
                    controls : [
                        {
                            type: "groupbox",
                            label: "Tests",
                            level: "2",
                            controls : [
                                { type:"checkbox", name: "students", label: "Student's" },
                                { type:"checkbox", name: "welch", label: "Welch" },
                                { type:"checkbox", name: "mann", label: "Mann-Whitney U" }
                            ]
                        },
                        {
                            type: "groupbox",
                            label: "Hypothesis",
                            level: "2",
                            controls : [
                                { type:"radiobutton", name: "hypothesis_different", optionId: "hypothesis", checkedValue: "different", label: "Group 1 ≠ Group 2" },
                                { type:"radiobutton", name: "hypothesis_oneGreater", optionId: "hypothesis", checkedValue: "oneGreater", label: "Group 1 > Group 2" },
                                { type:"radiobutton", name: "hypothesis_twoGreater", optionId: "hypothesis", checkedValue: "twoGreater", label: "Group 1 < Group 2" }
                            ]
                        },
                        {
                            type: "groupbox",
                            label: "Assumption Checks",
                            level: "2",
                            controls : [
                                { type:"checkbox", name: "norm", label: "Normality" },
                                { type:"checkbox", name: "equality", label: "Equality of variances" }
                            ]
                        }
                    ]
                },
                {
                    name: "column2",
                    cell: [1, 0],
                    stretchFactor: 1,
                    controls : [
                        {
                            type: "groupbox",
                            label: "Additional Statistics",
                            level: "2",
                            controls : [
                                { type:"checkbox", name: "meanDiff", label: "Mean difference" },
                                { type:"checkbox", name: "effectSize", label: "Effect size" },
                                {
                                    type:"checkbox", name: "ci", label: "Confidence interval",
                                    controls: [
                                        { type:"textbox", name: "ciWidth", label: "Interval", suffix: "%", formatName: "number", inputPattern: "[0-9]+" }
                                    ]
                                },
                                { type:"checkbox", name: "desc", label: "Descriptives" }
                            ]
                        },
                        {
                            type: "groupbox",
                            label: "Missing values",
                            level: "2",
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
        }
    ]
});

module.exports = { LayoutDef : ttestisLayout, options: options };
