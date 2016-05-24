'use strict';

var options = require("./descriptives.options");


var descriptivesLayout = LayoutDef.extend({

    title: "Descriptives",

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
                    label: function() { return "Columns and stuff"; },
                    showColumnHeaders: false,
                    columns: [
                        { name: 'column1', label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "vars2",
                    type:"listbox",
                    label: function() { return "More stuff"; },
                    showColumnHeaders: true,
                    columns: [
                        { name: "vars", label: "Variable", readOnly: true, formatName: "variable", stretchFactor: 1 },
                        { name: "werf", label: "Fixed", readOnly: true, formatName: "bool", stretchFactor: 0.5 }
                    ]
                }
            ]
        },
        {
            name: "group2",
            cell: [0, 1],
            items : [
                { name: "plots", type:"radiobutton", label: "Display Plots", actiongroup:"aG1" },
                { name: "plotCorr", type:"radiobutton", label: "Display Correlation Plot", actiongroup:"aG1" },
                { name: "freq", type:"checkbox", label: "Display Frequency Tables (nominal and ordinal variables)" }
            ]
        },
        {
            name: "group10",
            label: "Stuff",
            cell: [0, 2],
            items : [
                {
                    name: "group4",
                    label: "Percentile Values",
                    cell: [0, 0],
                    items : [
                        { name: "quart", type:"checkbox", label: "Quartiles" },
                        {
                            name: "pcEqGr",
                            label: "Cut points for",
                            type: "checkbox",
                            cell: [0, 1],
                            style: "inline",
                            items : [
                                { name: "pcNEqGr", type:"textbox", label: "", suffix: "equal groups", formatName: "number", inputPattern: "[0-9]+" }
                            ]
                        }
                    ],
                }
            ]
        },
        {
            name: "group3",
            label: "Statistics",
            cell: [0, 3],
            items : [
                {
                    name: "group11",
                    label: "Percentile Values",
                    cell: [0, 0],
                    items : [
                        { name: "quart", type:"checkbox", label: "Quartiles" },
                        {
                            name: "pcEqGr",
                            type: "checkbox",
                            label: "Cut points for",
                            cell: [0, 1],
                            style: "inline",
                            items : [
                                { name: "pcNEqGr", type:"textbox", label: "", suffix : "equal groups", formatName:"number", inputPattern: "[0-9]+" }
                            ]
                        }
                    ],
                },
                {
                    name: "group5",
                    label: "Central Tendency",
                    cell: [1, 0],
                    items: [
                        { name: "mean", type:"checkbox", label: "Mean" },
                        { name: "median", type:"checkbox", label: function() { return "Median and stuff"; } },
                        { name: "mode", type:"checkbox", label: "Mode" },
                        { name: "sum", type:"checkbox", label: "Sum" }
                    ]
                },
                {
                    name: "group6",
                    label: "Dispersion",
                    cell: [0, 1],
                    items: [
                        {
                            name: "group7",
                            cell: [0, 0],
                            items : [
                                { name: "sd", type:"checkbox", label: "Std. deviation" },
                                { name: "variance", type:"checkbox", label: "Variance" },
                                { name: "range", type:"checkbox", label: "Range" }
                            ]
                        },
                        {
                            name: "group8",
                            cell: [1, 0],
                            items : [
                                { name: "min", type:"checkbox", label: "Minimum" },
                                { name: "max", type:"checkbox", label: "Maximum" },
                                { name: "se", type:"checkbox", label: "S. E. Mean" }
                            ]
                        }
                    ]
                },
                {
                    name: "group9",
                    label: "Distribution",
                    cell: [1, 1],
                    items : [
                        { name: "skew", type:"checkbox", label: "Skewness" },
                        { name: "kurt", type:"checkbox", label: "Kurtosis" }
                    ],
                }
            ]
        }
    ]
});

module.exports = { LayoutDef : descriptivesLayout, options: options };
