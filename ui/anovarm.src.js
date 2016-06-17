
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./anovarm.options');
//var _ = require('underscore');

var anovarmLayout = LayoutDef.extend({

    label: "Repeated Measures ANOVA",
    type: "root",
    items: [
        {
            name: "variableSupplier",
            type: "supplier",
            cell: [0, 0],
            persistentItems: false,
            useVariables: true,
            items: [
                {
                    name: "rmFactors",
                    type:"listbox",
                    label: "Repeated Measures Factors",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "rmCells",
                    type:"listbox",
                    label: "Repeated Measures Cells",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "btwSubjFactors",
                    type:"listbox",
                    label: "Between Subject Factors",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "covariates",
                    type:"listbox",
                    label: "Covariates",
                    showColumnHeaders: false,
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
                    name: "rmcModelSupplier",
                    type: "supplier",
                    label: "Repeated Measures Components",
                    cell: [0, 0],
                    persistentItems: true,
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    items: [
                        {
                            name: "rmcModelTerms",
                            type:"listbox",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    name: "bscMdelSupplier",
                    type: "supplier",
                    label: "Repeated Measures Components",
                    cell: [0, 1],
                    persistentItems: true,
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    items: [
                        {
                            name: "bscModelTerms",
                            type:"listbox",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                { name: "sumOfSqu", type:"combobox", label: "Sum of squares", options: [ {label: 'Type I', value:'Type I'}, {label: 'Type II', value:'Type II'}, {label: 'Type III', value:'Type III'}] }
            ]
        },
        {
            name: "group7",
            label: "Assumption Checks",
            collapsed: true,
            cell: [0, 2],
            items : [
                { name: "spherTests", type:"checkbox", label: "Sphericity tests" },
                {
                    name: "spherCorrs",
                    type:"checkbox",
                    label: "Sphericity corrections",
                    items : [
                        { name: "spherCorrNone", type:"checkbox", label: "None" },
                        { name: "spherCorrGreenGsser", type:"checkbox", label: "Greenhouse-Geisser" },
                        { name: "spherCorrHuyFdt", type:"checkbox", label: "Huynh-Feldt" }
                    ]
                },
                { name: "homoTests", type:"checkbox", label: "Homogeneity tests" }
            ]
        },
        {
            name: "group6",
            label: "Contrasts",
            collapsed: true,
            cell: [0, 3],
            items : [
                {
                    name: "contrasts",
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
                    name: "postHocSupplier",
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
                    name: "plotsSupplier",
                    type: "supplier",
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
                            { name: "errBarDef_ci", optionId: "errBarDef", type:"radiobutton", checkedValue: "ci", label: "Confidence interval", items:[
                                { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", formatName: "number", inputPattern: "[0-9]+" }
                            ]},
                            { name: "errBarDef_se", optionId: "errBarDef", type:"radiobutton", checkedValue: "se", label: "Standard Error" }
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
    ],

    actions: [
        {
            onChange: "estEffSize", execute: function(context) {
                var disabled = context.getValue("estEffSize") === false;
                context.set("effSizeN2", "disabled", disabled);
                context.set("partEffSizeN2", "disabled", disabled);
                context.set("effSizeW2", "disabled", disabled);
            }
        },
        {
            onChange: "spherCorrs", execute: function(context) {
                var disabled = context.getValue("spherCorrs") === false;
                context.set("spherCorrNone", "disabled", disabled);
                context.set("spherCorrGreenGsser", "disabled", disabled);
                context.set("spherCorrHuyFdt", "disabled", disabled);
            }
        },
        {
            onChange: "dispErrBars", execute: function(context) {
                var disabled = context.getValue("dispErrBars") === false;
                context.set("errBarDef_se", "disabled", disabled);
                context.set("errBarDef_ci", "disabled", disabled);
            }
        },
        {
            onChange: ["dispErrBars", "errBarDef"], execute: function(context) {
                var value = context.getValue("dispErrBars") === false || context.getValue("errBarDef") !== "ci";
                context.set("ciWidth", "disabled", value);
            }
        }
    ]
});

module.exports = { LayoutDef : anovarmLayout, options: options };
