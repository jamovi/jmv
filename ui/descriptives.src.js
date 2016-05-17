'use strict';

var options = require("./descriptives.options");


var descriptivesLayout = LayoutDef.extend({

    title: "Descriptives",

    optionText: {
        vars : function() { return "Columns and stuff"; },
        vars2 : function() { return "More stuff"; },
        mean : "Mean",
        median : function() { return "Median and stuff"; },
        freq : "Display Frequency Tables (nominal and ordinal variables)" ,
        plots : "Display Plots",
        plotCorr : "Display Correlation Plot",
        quart : "Quartiles",
        pcEqGr : "Cut points for",
        pcPercent : "Percentiles",
        mode : "Mode",
        sum : "Sum",
        sd : "Std. deviation",
        variance : "Variance",
        range : "Range",
        min : "Minimum",
        max : "Maximum",
        se : "S. E. Mean",
        skew : "Skewness",
        kurt : "Kurtosis",
        plotW : "Plot Width",
        plotH : "Polt Height",
        pcNEqGr : ""
    },

    optionSuffix: {
        pcNEqGr : "equal groups"
    },

    groupText: {
        group3: "Statistics",
        group4: "Percentile Values",
        group5: "Central Tendency",
        group6: "Dispersion",
        group9: "Distribution",
        group10: "Stuff"
    },

    layout: [
        {
            name: "group1",
            cell: [0, 0],
            type: "supplier",
            persistentItems: false,
            items: [
                {
                    name: "vars",
                    type:"listbox",
                    showColumnHeaders: false,
                    columns: [
                        { name: 'column1', readOnly: true, formatName: "variable", stretchFactor: 1, label: "" }
                    ]
                },
                {
                    name: "vars2",
                    type:"listbox",
                    showColumnHeaders: true,
                    columns: [
                        { name: "vars", readOnly: true, formatName: "variable", stretchFactor: 1, label: "Variable" },
                        { name: "werf", readOnly: true, formatName: "bool", stretchFactor: 0.5, label: "Fixed" },
                        "rger"
                    ]
                }
            ]
        },
        {
            name: "group2",
            cell: [0, 1],
            items : [
                { name: "plots", type:"radiobutton", actiongroup:"aG1" },
                { name: "plotCorr", type:"radiobutton", actiongroup:"aG1" },
                { name: "freq", type:"checkbox" }
            ]
        },
        {
            name: "group10",
            cell: [0, 2],
            items : [
                {
                    name: "group4",
                    cell: [0, 0],
                    items : [
                        { name: "quart", type:"checkbox" },
                        {
                            name: "pcEqGr",
                            cell: [0, 1],
                            style: "inline",
                            type: "checkbox",
                            items : [
                                { name: "pcNEqGr", type:"textbox", formatName: "number", inputPattern: "[0-9]+" }
                            ]
                        }
                    ],
                }
            ]
        },
        {
            name: "group3",
            cell: [0, 3],
            items : [
                {
                    name: "group4",
                    cell: [0, 0],
                    items : [
                        { name: "quart", type:"checkbox" },
                        {
                            name: "pcEqGr",
                            cell: [0, 1],
                            style: "inline",
                            type: "checkbox",
                            items : [
                                { name: "pcNEqGr", type:"textbox", formatName:"number", inputPattern: "[0-9]+" }
                            ]
                        }
                    ],
                },
                {
                    name: "group5",
                    cell: [1, 0],
                    items: [
                        { name: "mean", type:"checkbox" },
                        { name: "median", type:"checkbox" },
                        { name: "mode", type:"checkbox" },
                        { name: "sum", type:"checkbox" }
                    ]
                },
                {
                    name: "group6",
                    cell: [0, 1],
                    items: [
                        {
                            name: "group7",
                            cell: [0, 0],
                            items : [
                                { name: "sd", type:"checkbox" },
                                { name: "variance", type:"checkbox"  },
                                { name: "range", type:"checkbox"  }
                            ]
                        },
                        {
                            name: "group8",
                            cell: [1, 0],
                            items : [
                                { name: "min", type:"checkbox" },
                                { name: "max", type:"checkbox"},
                                { name: "se", type:"checkbox" }
                            ]
                        }
                    ]
                },
                {
                    name: "group9",
                    cell: [1, 1],
                    items : [
                        { name: "skew", type:"checkbox" },
                        { name: "kurt", type:"checkbox" }
                    ],
                }
            ]
        }
    ],

    onRender_mean: function(e) {
        return "STUFF AND THINGS!";
    }
});

module.exports = { LayoutDef : descriptivesLayout, options: options };
