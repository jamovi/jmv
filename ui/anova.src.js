
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./anova.options')

var anovaLayout = LayoutDef.extend({

    title: "ANOVA",

    layout: [
        {
            name: "group1",
            type: "supplier",
            cell: [0, 0],
            persistentItems: false,
            items: [
                {
                    name: "dependent",
                    type:"listbox",
                    label: "Dependent Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "fixedFactors",
                    type:"listbox",
                    label: "Fixed Factors",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "wlsWeights",
                    type:"listbox",
                    label: "WLS Weights",
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
            label: "Model",
            collapsed: true,
            cell: [0, 1],
            items : [
                {
                    name: "group2-1",
                    type: "supplier",
                    cell: [0, 0],
                    persistentItems: true,
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    items: [
                        {
                            name: "modelTerms",
                            type:"listbox",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                }
            ]
        },
        {
            name: "group7",
            label: "Assumption Checks",
            collapsed: true,
            cell: [0, 2],
            items : [
                { name: "homoTests", type:"checkbox", label: "Homogeneity tests" },
                { name: "qqPlotRes", type:"checkbox", label: "Q-Q plot of residuals" }
            ]
        },
        {
            name: "group6",
            label: "Contrasts",
            collapsed: true,
            cell: [0, 3],
            items : [
                {
                    name: "factors",
                    type:"listbox",
                    label: "Factors",
                    showColumnHeaders: false,
                    columns: [
                        { name: "var", label: "", readOnly: true, formatName: "variable", stretchFactor: 0.5 },
                        { name: "type", label: "", readOnly: false, formatName: "string", stretchFactor: 1, options: ['none', 'deviation', 'simple', 'difference', 'helmert', 'repeated', 'polynomial'] }
                    ]
                }
            ]
        },
        {
            name: "group3",
            label: "Post Hoc Tests",
            collapsed: true,
            cell: [0, 4],
            items : [
                {
                    name: "group3-1",
                    type: "supplier",
                    cell: [0, 0],
                    persistentItems: false,
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    items: [
                        {
                            name: "postHocTests",
                            type:"listbox",
                            label: "",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    name: "group3-2",
                    label: "Correction",
                    cell: [0, 1],
                    items: [
                        { name: "corrTukey", type:"checkbox", label: "Tukey" },
                        { name: "corrScheffe", type:"checkbox", label: "Scheffe" },
                        { name: "corrBonf", type:"checkbox", label: "Bonferroni" },
                        { name: "corrHolm", type:"checkbox", label: "Holm" }
                    ]
                }
            ]
        },
        {
            name: "group5",
            label: "Descriptive Plots",
            collapsed: true,
            cell: [0, 5],
            items : [
                {
                    name: "group5-1",
                    type: "supplier",
                    label: "Factors",
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    cell: [0, 0],
                    persistentItems: false,
                    items: [
                        {
                            name: "descPlotsHAxis",
                            type:"listbox",
                            label: "Horizontal axis",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        },
                        {
                            name: "descPlotsSepLines",
                            type:"listbox",
                            label: "Separate lines",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        },
                        {
                            name: "descPlotsSepPlots",
                            type:"listbox",
                            label: "Separate plots",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    name: "group5-2",
                    label: "Display",
                    cell: [0, 1],
                    items: [
                        { name: "dispErrBars", type:"checkbox", label: "Error bars displaying", items: [
                            { name: "errBarDef", type:"radiobutton", value: "ci", label: "Confidence interval", items:[
                                { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", formatName: "number", inputPattern: "[0-9]+" }
                            ]},
                            { name: "errBarDef", type:"radiobutton", value: "se", label: "Standard Error" }
                        ]}
                    ]
                }
            ]
        },
        {
            name: "group4",
            label: "Additional Options",
            collapsed: true,
            cell: [0, 6],
            items : [
                {
                    name: "group4-1",
                    type: "supplier",
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    cell: [0, 0],
                    persistentItems: false,
                    items: [
                        {
                            name: "margMeans",
                            type:"listbox",
                            label: "Marginal means",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    name: "compMainEff",
                    label: "Compare main effects",
                    type:"checkbox",
                    cell: [0, 1],
                    items: [
                        { name: "confIntAdj", type:"textbox", label: "Confidence interval adjustment" }
                    ]
                },
                {
                    name: "group4-2",
                    label: "Display",
                    cell: [0, 2],
                    items: [
                        { name: "dispDescStats", type:"checkbox", label: "Descriptive statistics" },
                        {
                            name: "estEffSize",
                            label: "Estimates of effect size",
                            type:"checkbox",
                            items: [
                                { name: "effSizeN2", type:"checkbox", label: "n2" },
                                { name: "partEffSizeN2", type:"checkbox", label: "partial n2" },
                                { name: "effSizeW2", type:"checkbox", label: "w2" }
                            ]
                        }
                    ]
                }
            ]
        }
    ]
});

module.exports = { LayoutDef : anovaLayout, options: options };
