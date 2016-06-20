
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./ttestones.options')

var ttestonesLayout = LayoutDef.extend({

    label: "One Sample T-Test",
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
                                { type:"checkbox", name: "student", label: "Student" },
                                { type:"checkbox", name: "mann", label: "Mann-Whitney U" },
                                {
                                    controls : [
                                        { type:"textbox", name: "testValue", label: "Test value", formatName: "number", inputPattern: "[0-9]+" }
                                    ]
                                }
                            ]
                        },
                        {
                            type: "groupbox",
                            label: "Hypothesis",
                            level: "2",
                            controls : [
                                { name: "hypothesis_different", optionId: "hypothesis", type:"radiobutton", checkedValue: "different", label: "Group 1 ≠ Group 2" },
                                { name: "hypothesis_oneGreater", optionId: "hypothesis", type:"radiobutton", checkedValue: "oneGreater", label: "Group 1 > Group 2" },
                                { name: "hypothesis_twoGreater", optionId: "hypothesis", type:"radiobutton", checkedValue: "twoGreater", label: "Group 1 < Group 2" }
                            ]
                        },
                        {
                            type: "groupbox",
                            label: "Assumption Checks",
                            level: "2",
                            controls : [
                                { name: "norm", type:"checkbox", label: "Normality" }
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
                                { name: "meanDiff", type:"checkbox", label: "Mean difference" },
                                { name: "effectSize", type:"checkbox", label: "Effect size" },
                                {
                                    name: "ci", type:"checkbox", label: "Confidence interval",
                                    controls: [
                                        { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", formatName: "number", inputPattern: "[0-9]+" }
                                    ]
                                },
                                { name: "desc", type:"checkbox", label: "Descriptives" }
                            ]
                        },
                        {
                            type: "groupbox",
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
        }
    ]
});

module.exports = { LayoutDef : ttestonesLayout, options: options };
