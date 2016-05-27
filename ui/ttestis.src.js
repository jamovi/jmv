
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./ttestis.options')

var ttestisLayout = LayoutDef.extend({

    title: "Independent Samples T-Test",

    layout: [
        {
            name: "group1",
            type: "supplier",
            cell: [0, 0],
            persistentItems: false,
            items: [
                {
                    name: "vars",
                    type:"listbox",
                    label: "Dependent Variables",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "groupingVar",
                    type:"listbox",
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
            name: "group2",
            cell: [0, 1],
            stretchFactor: 1,
            fitToGrid: false,
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
                                { name: "welch", type:"checkbox", label: "Welch" },
                                { name: "mann", type:"checkbox", label: "Mann-Whitney U" }
                            ]
                        },
                        {
                            name: "column1-2",
                            label: "Hypothesis",
                            level: "2",
                            items : [
                                { name: "hypothesis", type:"radiobutton", value: "different", label: "Group 1 ≠ Group 2" },
                                { name: "hypothesis", type:"radiobutton", value: "oneGreater", label: "Group 1 > Group 2" },
                                { name: "hypothesis", type:"radiobutton", value: "twoGreater", label: "Group 1 < Group 2" }
                            ]
                        },
                        {
                            name: "column1-3",
                            label: "Assumption Checks",
                            level: "2",
                            items : [
                                { name: "norm", type:"checkbox", label: "Normality" },
                                { name: "equality", type:"checkbox", label: "Equality of variances" }
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
                                    name: "ci",
                                    type:"checkbox",
                                    label: "Confidence interval",
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
                                { name: "miss", type:"radiobutton", value: "perAnalysis", label: "Exclude cases analysis by analysis" },
                                { name: "miss", type:"radiobutton", value: "listwise", label: "Exclude cases listwise" }
                            ]
                        }
                    ]
                }
            ]
        }
    ]
});

module.exports = { LayoutDef : ttestisLayout, options: options };
