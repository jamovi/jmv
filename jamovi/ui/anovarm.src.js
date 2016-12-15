
'use strict';

const options = require('./anovarm.options');
const actions = require('./anovarm.actions');
const rma_cell = require('./rmacell');
const rma_cellMeasure = require('./rmacellmeasure');
const rmaCellMeasureCtrl =  require('./rmacellmeasurectrl');

const customControls = new ControlManager();
customControls.registerControl("listitem.rma_cellMeasure", rmaCellMeasureCtrl);

var anovarmLayout = ui.extend({

    label: "Repeated Measures ANOVA",
    type: "root",
    stage: 1,
    controls: [
        {
            type: "variablesupplier",
            name: "variableSupplier",
            suggested: ["continuous", "nominal", "ordinal"],
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    type:"rmanovafactorsbox",
                    name: "rm",
                    label: "Repeated Measures Factors",
                },
                {
                    type:"variabletargetlistbox",
                    name: "rmCells",
                    label: "Repeated Measures Cells",
                    showColumnHeaders: false,
                    removeAction: "clear_cell",
                    height: "large",
                    itemDropBehaviour: "overwrite",
                    columns: [
                        { type: "listitem.rma_cellMeasure", name: "measure", label: "Measure", stretchFactor: 1 }
                        /*{ type: "listitem.variablelabel", name: "measure", label: "Measure", stretchFactor: 1 },
                        { type: "listitem.label", name: "cell", label: "Cell", format: rma_cell,  selectable: false, stretchFactor: 1 }*/
                    ]
                },
                {
                    type:"variabletargetlistbox",
                    name: "bs",
                    label: "Between Subject Factors",
                    showColumnHeaders: false,
                    height: "small",
                    itemDropBehaviour: "insert",
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", stretchFactor: 1 }
                    ]
                },
                {
                    type:"variabletargetlistbox",
                    name: "cov",
                    label: "Covariates",
                    showColumnHeaders: false,
                    height: "small",
                    itemDropBehaviour: "insert",
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "label",
            label: "Effect Size",
            style: "list-inline",
            margin: "large",
            controls: [
                { name: "effSizeN2",   type:"checkbox", label: "η²" },
                { name: "partEffSizeN2",  type:"checkbox", label: "partial η²" },
                { name: "effSizeW2", type:"checkbox", label: "ω²" }
            ]
        },
        {
            type: "collapsebox",
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
                            name: "rmTerms",
                            label: "Model Terms",
                            valueFilter: "unique",
                            showColumnHeaders: false,
                            itemDropBehaviour: "empty_space",
                            columns: [
                                { type: "listitem.termlabel", name: "column1", label: "", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "supplier",
                    name: "bscModelSupplier",
                    label: "Between Subjects Components",
                    persistentItems: true,
                    stretchFactor: 1,
                    controls: [
                        {
                            type:"targetlistbox",
                            name: "bsTerms",
                            label: "Model Terms",
                            valueFilter: "unique",
                            showColumnHeaders: false,
                            itemDropBehaviour: "empty_space",
                            columns: [
                                { type: "listitem.termlabel", name: "column1", label: "", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "layoutbox",
                    controls: [
                        { name: "sumOfSqu", type:"combobox", label: "Sum of squares", options: [
                            {label: 'Type I', value:'Type I'},
                            {label: 'Type II', value:'Type II'},
                            {label: 'Type III', value:'Type III'}
                        ] }
                    ]
                }
            ]
        },
        {
            type: "collapsebox",
            label: "Assumption Checks",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                { type:"checkbox", name: "spherTests", label: "Sphericity tests" },
                {
                    type:"checkbox", name: "spherCorrs", label: "Sphericity corrections",
                    style: "list-inline",
                    controls : [
                        { type:"checkbox", name: "spherCorrNone", label: "None" },
                        { type:"checkbox", name: "spherCorrGreenGsser", label: "Greenhouse-Geisser" },
                        { type:"checkbox", name: "spherCorrHuyFdt", label: "Huynh-Feldt" }
                    ]
                },
                { type:"checkbox", name: "homoTests", label: "Homogeneity tests" }
            ]
        },
        {
            type: "collapsebox",
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
                        { type: "listitem.variablelabel", name: "var", label: "", selectable: false, stretchFactor: 0.5 },
                        { type: "listitem.combobox", name: "type", label: "", selectable: false, stretchFactor: 1, options: ['none', 'deviation', 'simple', 'difference', 'helmert', 'repeated', 'polynomial'] }
                    ]
                }
            ]
        },
        {
            type: "collapsebox",
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
                            name: "postHoc",
                            label: "",
                            showColumnHeaders: false,
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "label",
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
            type: "collapsebox",
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
                            itemDropBehaviour: "overwrite",
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", stretchFactor: 1 }
                            ]
                        },
                        {
                            type:"targetlistbox",
                            name: "descPlotsSepLines",
                            label: "Separate lines",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            itemDropBehaviour: "overwrite",
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", stretchFactor: 1 }
                            ]
                        },
                        {
                            type:"targetlistbox",
                            name: "descPlotsSepPlots",
                            label: "Separate plots",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            itemDropBehaviour: "overwrite",
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "label",
                    label: "Display",
                    controls: [
                        {
                            name: "dispErrBars", type:"checkbox", label: "Error bars displaying",
                            controls: [
                                {
                                    name: "errBarDef_ci", optionId: "errBarDef", type:"radiobutton", checkedValue: "ci", label: "Confidence interval",
                                    controls:[
                                        { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" }
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
            type: "collapsebox",
            label: "Additional Options",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "label",
                    label: "Display",
                    controls: [
                        { type:"checkbox", name: "dispDescStats", label: "Descriptive statistics" }
                    ]
                }
            ]
        }
    ]
});

module.exports = { ui : anovarmLayout, options: options, actions: actions, customControls: customControls };
