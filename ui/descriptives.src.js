'use strict';

var options = require("./descriptives.options");


var descriptivesLayout = LayoutDef.extend({

    label: "Descriptives",
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
                    label: function() { return "Columns and stuff"; },
                    showColumnHeaders: false,
                    columns: [
                        { name: 'column1', label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            name: "group2",
            cell: [0, 1],
            items : [
                { name: "plots", type:"checkbox", label: "Display Plots" },
                { name: "plotCorr", type:"checkbox", label: "Display Correlation Plot" },
                { name: "freq", type:"checkbox", label: "Display Frequency Tables (nominal and ordinal variables)" }
            ]
        },
        {
            name: "group3",
            label: "Statistics",
            cell: [0, 3],
            collapsed: true,
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
                        { name: "median", type:"checkbox", label: "Median" },
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
    ],

    actions: [
        {
            onChange : "pcEqGr", execute : function(context) {
                var disabled = context.getValue("pcEqGr") === false;
                context.set("pcNEqGr", "disabled", disabled);
            }
        }
    ]
});

module.exports = { LayoutDef : descriptivesLayout, options: options };
