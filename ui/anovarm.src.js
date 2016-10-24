
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
                    type:"targetlistbox",
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
                            name: "rmcModelTerms",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
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
                            name: "bscModelTerms",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
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
                this.filterCells(context);
            }
        },
        {
            onChange: "rmCells", execute: function(context) {
                this.filterCells(context);
            }
        },
        {
            onChange: ["bs", "cov", "rm"], execute: function(context) {
                this.calcModelTerms(context);
            }
        },
        {
            onChange: "bscModelTerms", execute: function(context) {
                this.filterModelTerms(context);
            }
        },
        {
            onEvent: "view.data-initialising", execute: function(context) {
                this._lastVariableList = null;
                this._lastCombinedList = null;
                this._lastCombinedList2 = null;
                this._lastCovariatesList = null;
                this._lastCurrentList = null;
                this._initialising = true;
            }
        },
        {
            onEvent: "view.data-initialised", execute: function(context) {
                if (this._lastVariableList === null || this._lastCombinedList === null || this._lastCombinedList2 === null || this._lastCovariatesList === null)
                    this.calcModelTerms(context);

                if (this._lastCurrentList === null)
                    this.filterModelTerms(context);

                this._initialising = false;
            }
        }
    ],

    sortByLength : function(list) {
        var changed = false;
        for (var i = 0; i < list.length; i++) {
            var l1 = 1;
            if (Array.isArray(list[i]))
                l1 = list[i].length;

            var l2 = 1;
            if (Array.isArray(list[i+1]))
                l2 = list[i+1].length;

            if (list.length > i + 1 && (l1 > l2)) {
                changed = true;
                var temp = list[i+1];
                list[i+1] = list[i];
                list[i] = temp;
                if (i > 0)
                    i = i - 2
            }
        }

        return changed;
    },

    filterModelTerms: function(context) {
        var currentList = this.clone(context.getValue("bscModelTerms"));
        if (currentList === null)
            currentList = [];

        var covariatesList = context.getValue("cov");
        if (covariatesList === null)
            covariatesList = [];

        var covariateFreeList = [];
        for (let i = 0; i < currentList.length; i++) {
            let newVar = currentList[i];
            if (this.containsCovariate(newVar, covariatesList) === false)
                covariateFreeList.push(this.clone(newVar));
        }

        //var list = this.convertArrayToSupplierList(covariateFreeList, FormatDef.variable);
        //context.setValue("marginalMeansSupplier", list);

        var diff = null;
        if ( ! this._initialising)
            diff = this.findDifferences(this._lastCurrentList, currentList);
        this._lastCurrentList = currentList;

        if (this._initialising)
            return;

        var changed = false;
        if (diff.removed.length > 0 && currentList !== null) {
            var itemsRemoved = false;
            for (var i = 0; i < diff.removed.length; i++) {
                var item = diff.removed[i];
                for (var j = 0; j < currentList.length; j++) {
                    if (FormatDef.variable.contains(currentList[j], item)) {
                        currentList.splice(j, 1);
                        j -= 1;
                        itemsRemoved = true;
                    }
                }
            }

            if (itemsRemoved)
                changed = true;
        }

        if (this.sortByLength(currentList))
            changed = true;

        if (changed)
            context.setValue("bscModelTerms", currentList);
    },

    listContains: function(list, value) {
        for (var i = 0; i < list.length; i++) {
            if (FormatDef.variable.isEqual(list[i], value))
                return true;
        }

        return false;
    },

    containsCovariate: function(value, covariates) {
        for (var i = 0; i < covariates.length; i++) {
            if (FormatDef.variable.contains(value, covariates[i]))
                return true;
        }

        return false;
    },

    findDifferences: function(from, to) {
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
                if (this.listContains(to, from[j]) === false)
                    obj.removed.push(from[j]);
            }

            for (j = 0; j < to.length; j++) {
                if (this.listContains(from, to[j]) === false)
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

    calcModelTerms : function(context) {
        var variableList = this.clone(context.getValue("bs"));
        if (variableList === null)
            variableList = [];

        var covariatesList = this.clone(context.getValue("cov"));
        if (covariatesList === null)
            covariatesList = [];

        var factorList = this.clone(context.getValue("rm"));
        if (factorList === null)
            factorList = [];
        else {
            for(let i = 0; i < factorList.length; i++) {
                factorList[i] = factorList[i].label;
            }
        }

        var combinedList = variableList.concat(covariatesList);

        var combinedList2 = factorList.concat(variableList);

        context.setValue("rmcModelSupplier", this.convertArrayToSupplierList(factorList, FormatDef.variable))
        context.setValue("bscModelSupplier", this.convertArrayToSupplierList(combinedList, FormatDef.variable));
        context.setValue("plotsSupplier", this.convertArrayToSupplierList(combinedList2, FormatDef.variable));
        context.setValue("postHocSupplier", this.convertArrayToSupplierList(combinedList2, FormatDef.variable));

        var diff = { removed: [], added: [] };
        if (this._lastVariableList !== null)
            diff = this.findDifferences(this._lastVariableList, variableList);
        this._lastVariableList = variableList;

        var diff2 = { removed: [], added: [] };
        if (this._lastCovariatesList !== null)
            diff2 = this.findDifferences(this._lastCovariatesList, covariatesList);
        this._lastCovariatesList = covariatesList;

        var combinedDiff = { removed: [], added: [] };
        if (this._lastCombinedList !== null)
            combinedDiff = this.findDifferences(this._lastCombinedList, combinedList);
        this._lastCombinedList = combinedList;

        var combinedDiff2 = { removed: [], added: [] };
        if (this._lastCombinedList2 !== null)
            combinedDiff2 = this.findDifferences(this._lastCombinedList2, combinedList2);
        this._lastCombinedList2 = combinedList2;

        if (this._initialising)
            return;

        var currentList = this.clone(context.getValue("bscModelTerms"));
        if (currentList === null)
            currentList = [];

        for (var i = 0; i < combinedDiff.removed.length; i++) {
            for (var j = 0; j < currentList.length; j++) {
                if (FormatDef.variable.contains(currentList[j], combinedDiff.removed[i])) {
                    currentList.splice(j, 1);
                    j -= 1;
                }
            }
        }

        if (currentList === null)
            currentList = [];

        for (var i = 0; i < diff.added.length; i++) {
            var listLength = currentList.length;
            for (var j = 0; j < listLength; j++) {
                var newVar = currentList[j];
                if (this.containsCovariate(newVar, covariatesList) === false) {
                    if (Array.isArray(newVar))
                        newVar = this.clone(newVar);
                    else
                        newVar = [newVar];
                    newVar.push(diff.added[i])
                    currentList.push(newVar);
                }
            }
            currentList.push(diff.added[i]);
        }

        for (var i = 0; i < diff2.added.length; i++)
            currentList.push(diff2.added[i]);

        context.setValue("bscModelTerms", currentList);

        this.updateContrasts(context, combinedList2);
    },

    updateContrasts : function(context, variableList) {
        var currentList = this.clone(context.getValue("contrasts"));
        if (currentList === null)
            currentList = [];

        var list3 = [];
        for (var i = 0; i < variableList.length; i++)
            list3.push({ var: variableList[i], type: "none" });

        context.setValue("contrasts", list3);
    },

    filterCells: function(context) {

        if (this._factorCells === null)
            return;

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

    _factorCells : null
});

module.exports = { ui : anovarmLayout, options: options };
