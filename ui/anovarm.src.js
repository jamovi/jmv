
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./anovarm.options');
//var _ = require('underscore');

var rma_cell =  {

    name: 'rma_cell',

    default: [],

    toString: function(raw) {
        var r = raw[0];
        for (var i = 1; i < raw.length; i++)
            r = r + ", " + raw[i];
        return r;
    },

    parse: function(value) {
        throw "need to implement";
        return value;
    },

    isValid: function(raw) {
        if (Array.isArray(raw) === false)
            return false;

        for (var i = 0; i < raw.length; i++) {
            if (typeof(raw[i]) !== 'string')
                return false;
        }

        return true;
    },

    isEqual: function(raw1, raw2) {

        if (raw1.length !== raw2.length)
            return false;

        for (var i = 0; i < raw1.length; i++) {
            if (raw1[i] !== raw2[i])
                return false;
        }

        return true;
    }
}

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
                    type:"rmanovafactorsbox",
                    name: "rm",
                    label: "Repeated Measures Factors",
                },
                {
                    type:"targetlistbox",
                    name: "rmCells",
                    label: "Repeated Measures Cells",
                    showColumnHeaders: false,
                    removeAction: "clear_cell",
                    height: "large",
                    columns: [
                        { name: "measure", label: "Measure", readOnly: true, format: FormatDef.variable, stretchFactor: 1 },
                        { name: "cell", label: "Cell", readOnly: true, selectable: false, format: rma_cell, stretchFactor: 1 }
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "bs",
                    label: "Between Subject Factors",
                    showColumnHeaders: false,
                    height: "small",
                    columns: [
                        { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "cov",
                    label: "Covariates",
                    showColumnHeaders: false,
                    height: "small",
                    columns: [
                        { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
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
                                { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
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
                                { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
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
                        { name: "var", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 0.5 },
                        { name: "type", label: "", readOnly: false, format: FormatDef.string, stretchFactor: 1, options: ['none', 'deviation', 'simple', 'difference', 'helmert', 'repeated', 'polynomial'] }
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
                            name: "postHoc",
                            label: "",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
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
                                { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
                            ]
                        },
                        {
                            type:"targetlistbox",
                            name: "descPlotsSepLines",
                            label: "Separate lines",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
                            ]
                        },
                        {
                            type:"targetlistbox",
                            name: "descPlotsSepPlots",
                            label: "Separate plots",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, format: FormatDef.variable, stretchFactor: 1 }
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
        },
        {
            onChange: "rm", execute: function(context) {
                var value = context.getValue("rm");
                if (value === null)
                    value = [{label: "RM Factors 1", levels: ["Level 1", "Level 2"]}];

                var data = []
                var indices = []
                for (var i = 0; i < value.length; i++) {
                    indices[i] = 0;
                }

                var end = false;
                var pos = 0;
                while (end === false) {
                    var cell = []
                    for (var k = 0; k < indices.length; k++) {
                        cell.push(value[k].levels[indices[k]])
                    }
                    data.push(cell);
                    pos += 1;
                    var zeroCount = 0;

                    var r = indices.length - 1;
                    if (r < 0)
                        end = true;
                    while (r >= 0) {
                        indices[r] = (indices[r] + 1) % value[r].levels.length;
                        if (indices[r] === 0)
                            r -= 1;
                        else
                            break;

                        if (r === -1)
                            end = true;
                    }
                }

                this._factorCells = data;
                this.filterCells(context);
            }
        },
        {
            onChange: "rmCells", execute: function(context) {
                this.filterCells(context);
            }
        }
    ],

    filterCells: function(context) {
        var cells = this.clone(context.getValue("rmCells"));
        if (cells === null)
            cells = [];

        var factorCells = this.clone(this._factorCells);

        var changed = false;
        for (var j = 0; j < factorCells.length; j++) {
            if (j < cells.length) {
                if (rma_cell.isEqual(cells[j].cell, factorCells[j]) === false) {
                    cells[j].cell = factorCells[j];
                    changed = true;
                }
            }
            else {
                cells.push({ measure: null, cell: factorCells[j] });
                changed = true;
            }
        }

        if (cells.length > factorCells.length) {
            cells.splice(factorCells.length, cells.length - factorCells.length);
            changed = true;
        }

        if (changed) {
            context.setValue("rmCells", cells);
            context.set("rmCells", "maxItemCount", cells.length);
        }
    },

    isInArray: function(value, array) {
        for (var i = 0; i < array.length; i++) {
            if (rma_cell.isEqual(value, array[i]))
                return i;
        }
        return -1;
    },

    clone: function(object) {
        return JSON.parse(JSON.stringify(object));
    },

    _factorCells : []
});

module.exports = { LayoutDef : anovarmLayout, options: options };
