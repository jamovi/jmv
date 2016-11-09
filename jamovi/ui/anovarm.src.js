
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
                    columns: [
                        { type: "listitem.variablelabel", name: "measure", label: "Measure", format: FormatDef.variable, stretchFactor: 1 },
                        { type: "listitem.label", name: "cell", label: "Cell", selectable: false, format: rma_cell, stretchFactor: 1 }
                    ]
                },
                {
                    type:"variabletargetlistbox",
                    name: "bs",
                    label: "Between Subject Factors",
                    showColumnHeaders: false,
                    height: "small",
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                },
                {
                    type:"variabletargetlistbox",
                    name: "cov",
                    label: "Covariates",
                    showColumnHeaders: false,
                    height: "small",
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "layoutbox",
            style: "inline",
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
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.term, stretchFactor: 1 }
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
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.term, stretchFactor: 1 }
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
                        { type: "listitem.variablelabel", name: "var", label: "", format: FormatDef.variable, selectable: false, stretchFactor: 0.5 },
                        { type: "listitem.combobox", name: "type", label: "", format: FormatDef.string, selectable: false, stretchFactor: 1, options: ['none', 'deviation', 'simple', 'difference', 'helmert', 'repeated', 'polynomial'] }
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
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
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
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                            ]
                        },
                        {
                            type:"targetlistbox",
                            name: "descPlotsSepLines",
                            label: "Separate lines",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                            ]
                        },
                        {
                            type:"targetlistbox",
                            name: "descPlotsSepPlots",
                            label: "Separate plots",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
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
    ],

    actions: [
        {
            onChange: "spherCorrs", execute: function(ui) {
                var value = ui.spherCorrs.value();
                ui.spherCorrNone.setEnabled(value);
                ui.spherCorrGreenGsser.setEnabled(value);
                ui.spherCorrHuyFdt.setEnabled(value);
            }
        },
        {
            onChange: "dispErrBars", execute: function(ui) {
                var value = ui.dispErrBars.value();
                ui.errBarDef_se.setEnabled(value);
                ui.errBarDef_ci.setEnabled(value);
            }
        },
        {
            onChange: ["dispErrBars", "errBarDef"], execute: function(ui) {
                ui.ciWidth.setEnabled(ui.dispErrBars.value() && ui.errBarDef.value() === "ci");
            }
        },
        {
            onChange: "rm", execute: function(ui) {
                var value = ui.rm.value();
                if (value === null)
                    return;

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
                this.filterCells(ui);
            }
        },
        {
            onChange: "rmCells", execute: function(ui) {
                this.filterCells(ui);
            }
        },
        {
            onChange: ["bs", "cov", "rm"], execute: function(ui) {
                this.calcModelTerms(ui);
            }
        },
        {
            onChange: "bsTerms", execute: function(ui) {
                this.filterModelTerms(ui);
            }
        },
        {
            onEvent: "bsTerms.preprocess", execute: function(ui, data) {
                data.items = this._variableListInteractions(data.items);
            }
        },
        {
            onEvent: "view.loaded", execute: function(ui) {
                this._loaded = true;
            }
        },
        {
            onEvent: "view.data-initialising", execute: function(ui) {
                this._lastVariableList = null;
                this._lastCombinedList = null;
                this._lastCombinedList2 = null;
                this._lastCovariatesList = null;
                this._lastCurrentList = null;
                this._initialising = true;
            }
        },
        {
            onEvent: "view.data-initialised", execute: function(ui) {
                if (this._lastVariableList === null || this._lastCombinedList === null || this._lastCombinedList2 === null || this._lastCovariatesList === null)
                    this.calcModelTerms(ui);

                if (this._lastCurrentList === null)
                    this.filterModelTerms(ui);

                this._initialising = false;
            }
        }
    ],

    _variableListInteractions: function(componentListItems) {
        var list = [];
        for (let i = 0; i < componentListItems.length; i++) {
            var listLength = list.length;
            var rawVar = componentListItems[i].value.raw;

            for (let j = 0; j < listLength; j++) {
                var newVar = JSON.parse(JSON.stringify(list[j].value.raw));
                newVar.push(rawVar);
                list.push({ value: new FormatDef.constructor(newVar, FormatDef.term) });
            }

            list.push({ value: new FormatDef.constructor([rawVar], FormatDef.term), original: componentListItems[i] });
        }

        return list;
    },

    sortByLength : function(terms) {
        var changed = false;
        for (var i = 0; i < terms.length - 1; i++) {
            var l1 = terms[i].length;
            var l2 = terms[i+1].length;

            if (terms.length > i + 1 && (l1 > l2)) {
                changed = true;
                var temp = terms[i+1];
                terms[i+1] = terms[i];
                terms[i] = temp;
                if (i > 0)
                    i = i - 2
            }
        }

        return changed;
    },

    clone: function(object) {
        return JSON.parse(JSON.stringify(object));
    },

    listContains: function(list, value, format) {
        for (var i = 0; i < list.length; i++) {
            if (format.isEqual(list[i], value))
                return true;
        }

        return false;
    },

    findDifferences: function(format, from, to) {
        var j = 0;

        var obj = { removed: [], added: [] };

        if ((from === null || _.isUndefined(from)) && (to === null || _.isUndefined(to)))
            return obj;
        else if (from === null || _.isUndefined(from)) {
            for (j = 0; j < to.length; j++)
                obj.added.push(to[j]);
        }
        else if (to === null || _.isUndefined(to)) {
            for (j = 0; j < from.length; j++)
                obj.removed.push(from[j]);
        }
        else {
            for (j = 0; j < from.length; j++) {
                if (this.listContains(to, from[j], format) === false)
                    obj.removed.push(from[j]);
            }

            for (j = 0; j < to.length; j++) {
                if (this.listContains(from, to[j], format) === false)
                    obj.added.push(to[j]);
            }
        }

        return obj;
    },

    convertArrayToSupplierList: function(array, format) {
        var list = [];
        for (var i = 0; i < array.length; i++) {
            list.push({ value: new FormatDef.constructor(array[i], format) });
        }
        return list;
    },

    isInArray: function(value, array, format) {
        for (var i = 0; i < array.length; i++) {
            if (format.isEqual(value, array[i]))
                return i;
        }
        return -1;
    },



    calcModelTerms : function(ui) {
        var variableList = this.clone(ui.bs.value());
        if (variableList === null)
            variableList = [];

        var covariatesList = this.clone(ui.cov.value());
        if (covariatesList === null)
            covariatesList = [];

        var factorList = this.clone(ui.rm.value());
        if (factorList === null)
            factorList = [];
        else {
            for(let i = 0; i < factorList.length; i++)
                factorList[i] = factorList[i].label;
        }

        var combinedList = variableList.concat(covariatesList);

        var combinedList2 = factorList.concat(variableList);

        ui.rmcModelSupplier.setValue(this.convertArrayToSupplierList(factorList, FormatDef.variable))
        ui.bscModelSupplier.setValue(this.convertArrayToSupplierList(combinedList, FormatDef.variable));
        ui.plotsSupplier.setValue(this.convertArrayToSupplierList(combinedList2, FormatDef.variable));
        ui.postHocSupplier.setValue(this.convertArrayToSupplierList(combinedList2, FormatDef.variable));

        var diff = { removed: [], added: [] };
        if (this._lastVariableList !== null)
            diff = this.findDifferences(FormatDef.variable, this._lastVariableList, variableList);
        this._lastVariableList = variableList;

        var diff2 = { removed: [], added: [] };
        if (this._lastCovariatesList !== null)
            diff2 = this.findDifferences(FormatDef.variable, this._lastCovariatesList, covariatesList);
        this._lastCovariatesList = covariatesList;

        var combinedDiff = { removed: [], added: [] };
        if (this._lastCombinedList !== null)
            combinedDiff = this.findDifferences(FormatDef.variable, this._lastCombinedList, combinedList);
        this._lastCombinedList = combinedList;


        if (this._initialising || !this._loaded)
            return;

        var termsList = this.clone(ui.bsTerms.value());
        if (termsList === null)
            termsList = [];

        var termsChanged = false;

        for (var i = 0; i < combinedDiff.removed.length; i++) {
            for (var j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], combinedDiff.removed[i])) {
                    termsList.splice(j, 1);
                    termsChanged = true;
                    j -= 1;
                }
            }
        }

        for (var i = 0; i < diff.added.length; i++) {
            var listLength = termsList.length;
            for (var j = 0; j < listLength; j++) {
                var newTerm = this.clone(termsList[j]);
                if (this.containsCovariate(newTerm, covariatesList) === false) {
                    newTerm.push(diff.added[i])
                    termsList.push(newTerm);
                }
            }
            termsList.push([diff.added[i]]);
            termsChanged = true;
        }

        for (var i = 0; i < diff2.added.length; i++) {
            termsList.push([diff2.added[i]]);
            termsChanged = true;
        }

        if (termsChanged)
            ui.bsTerms.setValue(termsList);

        this.updateContrasts(ui, combinedList2);
    },

    filterModelTerms: function(ui) {
        var termsList = this.clone(ui.bsTerms.value());
        if (termsList === null)
            termsList = [];

        var diff = null;
        if ( ! this._initialising && this._loaded)
            diff = this.findDifferences(FormatDef.term, this._lastCurrentList, termsList);
        this._lastCurrentList = termsList;

        if (this._initialising || !this._loaded)
            return;

        var changed = false;
        if (diff.removed.length > 0) {
            var itemsRemoved = false;
            for (var i = 0; i < diff.removed.length; i++) {
                var item = diff.removed[i];
                for (var j = 0; j < termsList.length; j++) {
                    if (FormatDef.term.contains(termsList[j], item)) {
                        termsList.splice(j, 1);
                        j -= 1;
                        itemsRemoved = true;
                    }
                }
            }

            if (itemsRemoved)
                changed = true;
        }

        if (this.sortByLength(termsList))
            changed = true;

        if (changed)
            ui.bsTerms.setValue(termsList);
    },

    containsCovariate: function(value, covariates) {
        for (var i = 0; i < covariates.length; i++) {
            if (FormatDef.term.contains(value, covariates[i]))
                return true;
        }

        return false;
    },

    updateContrasts : function(ui, variableList) {
        var currentList = this.clone(ui.contrasts.value());
        if (currentList === null)
            currentList = [];

        var list3 = [];
        for (var i = 0; i < variableList.length; i++)
            list3.push({ var: variableList[i], type: "none" });

        ui.contrasts.setValue(list3);
    },

    filterCells: function(ui) {

        if (this._factorCells === null)
            return;

        var cells = this.clone(ui.rmCells.value());
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
            ui.rmCells.setValue(cells);
            ui.rmCells.setPropertyValue("maxItemCount", cells.length);
        }
    },


    _factorCells : null
});

module.exports = { ui : anovarmLayout, options: options };
