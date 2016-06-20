
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./anovarm.options');
//var _ = require('underscore');

var anovarmLayout = LayoutDef.extend({

    label: "Repeated Measures ANOVA",
    type: "root",
    controls: [
        {
            type: "supplier",
            name: "variableSupplier",
            persistentItems: false,
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    type:"targetlistbox",
                    name: "rmFactors",
                    label: "Repeated Measures Factors",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "rmCells",
                    label: "Repeated Measures Cells",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "btwSubjFactors",
                    label: "Between Subject Factors",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "covariates",
                    label: "Covariates",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "groupbox",
            label: "Model",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "supplier",
                    name: "rmcModelSupplier",
                    label: "Repeated Measures Components",
                    persistentItems: true,
                    stretchFactor: 1,
                    controls: [
                        {
                            type: "targetlistbox",
                            name: "rmcModelTerms",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "supplier",
                    name: "bscMdelSupplier",
                    label: "Between Subjects Components",
                    persistentItems: true,
                    stretchFactor: 1,
                    controls: [
                        {
                            type:"targetlistbox",
                            name: "bscModelTerms",
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
            type: "groupbox",
            label: "Assumption Checks",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                { type:"checkbox", name: "spherTests", label: "Sphericity tests" },
                {
                    type:"checkbox", name: "spherCorrs", label: "Sphericity corrections",
                    controls : [
                        {
                            style: "inline",
                            controls: [
                                { type:"checkbox", name: "spherCorrNone", label: "None" },
                                { type:"checkbox", name: "spherCorrGreenGsser", label: "Greenhouse-Geisser" },
                                { type:"checkbox", name: "spherCorrHuyFdt", label: "Huynh-Feldt" }
                            ]
                        }
                    ]
                },
                { type:"checkbox", name: "homoTests", label: "Homogeneity tests" }
            ]
        },
        {
            type: "groupbox",
            label: "Contrasts",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type:"listbox",
                    name: "contrasts",
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
            type: "groupbox",
            label: "Post Hoc Tests",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "supplier",
                    name: "postHocSupplier",
                    persistentItems: false,
                    stretchFactor: 1,
                    controls: [
                        {
                            type:"targetlistbox",
                            name: "postHocTests",
                            label: "",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "groupbox",
                    label: "Correction",
                    controls: [
                        { type:"checkbox", name: "corrTukey", label: "Tukey" },
                        { type:"checkbox", name: "corrScheffe", label: "Scheffe" },
                        { type:"checkbox", name: "corrBonf", label: "Bonferroni" },
                        { type:"checkbox", name: "corrHolm", label: "Holm" }
                    ]
                }
            ]
        },
        {
            type: "groupbox",
            label: "Descriptive Plots",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "supplier",
                    name: "plotsSupplier",
                    stretchFactor: 1,
                    persistentItems: false,
                    controls: [
                        {
                            type:"targetlistbox",
                            name: "descPlotsHAxis",
                            label: "Horizontal axis",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        },
                        {
                            type:"targetlistbox",
                            name: "descPlotsSepLines",
                            label: "Separate lines",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        },
                        {
                            type:"targetlistbox",
                            name: "descPlotsSepPlots",
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
                    type: "groupbox",
                    label: "Display",
                    controls: [
                        {
                            name: "dispErrBars", type:"checkbox", label: "Error bars displaying",
                            controls: [
                                {
                                    name: "errBarDef_ci", optionId: "errBarDef", type:"radiobutton", checkedValue: "ci", label: "Confidence interval",
                                    controls:[
                                        { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", formatName: "number", inputPattern: "[0-9]+" }
                                    ]
                                },
                                { name: "errBarDef_se", optionId: "errBarDef", type:"radiobutton", checkedValue: "se", label: "Standard Error" }
                            ]
                        }
                    ]
                }
            ]
        },
        {
            type: "groupbox",
            label: "Additional Options",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "groupbox",
                    label: "Display",
                    controls: [
                        { type:"checkbox", name: "dispDescStats", label: "Descriptive statistics" },
                        {
                            type:"checkbox", name: "estEffSize", label: "Estimates of effect size",
                            controls: [
                                {
                                    style: "inline",
                                    controls: [
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
