
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./ttestones.options')

var ttestonesLayout = LayoutDef.extend({

    label: "One Sample T-Test",
    type: "root",
    items: [
        {
            name: "group1",
            type: "supplier",
            cell: [0, 0],
            persistentItems: false,
            useVariables: true,
            items: [
                {
                    name: "vars",
                    type:"listbox",
                    label: "Dependent Variables",
                    showColumnHeaders: false,
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
                                { name: "mann", type:"checkbox", label: "Mann-Whitney U" },
                                {
                                    name: "column1-1-1",
                                    items : [
                                        { name: "testValue", type:"textbox", label: "Test value", formatName: "number", inputPattern: "[0-9]+" }
                                    ]
                                }
                            ]
                        },
                        {
                            name: "column1-2",
                            label: "Hypothesis",
                            level: "2",
                            items : [
                                { name: "hypothesis_different", optionId: "hypothesis", type:"radiobutton", value: "different", label: "Group 1 ≠ Group 2" },
                                { name: "hypothesis_oneGreater", optionId: "hypothesis", type:"radiobutton", value: "oneGreater", label: "Group 1 > Group 2" },
                                { name: "hypothesis_twoGreater", optionId: "hypothesis", type:"radiobutton", value: "twoGreater", label: "Group 1 < Group 2" }
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
                                { name: "miss_perAnalysis", optionId: "miss", type:"radiobutton", value: "perAnalysis", label: "Exclude cases analysis by analysis" },
                                { name: "miss_listwise", optionId: "miss", type:"radiobutton", value: "listwise", label: "Exclude cases listwise" }
                            ]
                        }
                    ]
                }
            ]
        }
    ],

    actions: {

        disable_ciWidth: function(context) {
            var disabled = context.getObject("ci").get("value") === false;
            context.getObject("ciWidth").set("disabled", disabled);
        }
    }
});

module.exports = { LayoutDef : ttestonesLayout, options: options };
