
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./anova.options');

var controls = new ControlManager();

var anovaLayout = LayoutDef.extend({

    label: "ANOVA",
    type: "root",
    controls: [
        {
            type: "supplier",
            name: "variableSupplier",
            label: "Variables",
            persistentItems: false,
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    name: "dependent",
                    type:"targetlistbox",
                    label: "Dependent Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                },
                {
                    name: "fixedFactors",
                    type:"targetlistbox",
                    label: "Fixed Factors",
                    showColumnHeaders: false,
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            controls: [
                {
                    style: "inline",
                    controls: [
                        { name: "etaSq",   type:"checkbox", label: "η²" },
                        { name: "etaSqP",  type:"checkbox", label: "partial η²" },
                        { name: "omegaSq", type:"checkbox", label: "ω²" }
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
                    name: "modelSupplier",
                    label: "Componets",
                    persistentItems: true,
                    stretchFactor: 1,
                    controls: [
                        {
                            type:"targetlistbox",
                            name: "modelTerms",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            valueFilter: "unique",
                            multipleSelectionAction: "interactions",
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                { name: "ss", type:"combobox", label: "Sum of squares", options: [
                    { label: 'Type 1', value: '1' },
                    { label: 'Type 2', value: '2' },
                    { label: 'Type 3', value: '3' }
                ] }
            ]
        },
        {
            type: "groupbox",
            label: "Assumption Checks",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                { name: "homo", type:"checkbox", label: "Homogeneity tests" },
                { name: "qq", type:"checkbox", label: "Q-Q plot of residuals" }
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
                        { type: "listitem.variablelabel", name: "var", label: "", format: FormatDef.variable, stretchFactor: 0.5 },
                        { type: "listitem.combobox", name: "type", label: "", format: FormatDef.string, stretchFactor: 1, options: ['none', 'deviation', 'simple', 'difference', 'helmert', 'repeated', 'polynomial'] }
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
                            showColumnHeaders: false,
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "groupbox",
                    label: "Correction",
                    controls: [
                        { name: "corrTukey", type:"checkbox", label: "Tukey" },
                        { name: "corrScheffe", type:"checkbox", label: "Scheffe" },
                        { name: "corrBonf", type:"checkbox", label: "Bonferroni" },
                        { name: "corrHolm", type:"checkbox", label: "Holm" }
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
                    label: "Factors",
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
                    type: "groupbox",
                    label: "Display",
                    controls: [
                        {
                            name: "errBarDef_ci", optionId: "plotError", type:"radiobutton", checkedValue: "ci", label: "Confidence interval",
                            controls: [ { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" } ]
                        },
                        { name: "errBarDef_se", optionId: "plotError", type:"radiobutton", checkedValue: "se", label: "Standard Error" }
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
                    type: "supplier",
                    name: "marginalMeansSupplier",
                    stretchFactor: 1,
                    persistentItems: false,
                    controls: [
                        {
                            type:"targetlistbox",
                            name: "margMeans",
                            label: "Marginal means",
                            showColumnHeaders: false,
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "groupbox",
                    controls: [
                        {
                            name: "compMain", label: "Compare main effects", type:"checkbox",
                            controls: [
                                { name: "compMainCorr", type:"combobox", label: "Correction", options: [
                                    { label: "None", value: "none" },
                                    { label: "Tukey", value: "tukey" },
                                    { label: "Bonferroni", value: "bonferroni" },
                                    { label: "Scheffe", value: "scheffe" },
                                    { label: "Sidak", value: "sidak" } ] }
                            ]
                        }
                    ]
                },
                {
                    type: "groupbox",
                    label: "Display",
                    controls: [
                        { name: "descStats", type:"checkbox", label: "Descriptive statistics" }
                    ]
                }
            ]
        }
    ],

    actions: [
        {
            onChange: "compMain", execute: function(context) {
                var value = context.getValue("compMain") ;
                context.set("compMainCorr", "disabled", value === false);
            }
        },
        {
            onChange: "plotError", execute: function(context) {
                var disabled = context.getValue("plotError") !== "ci";
                context.set("ciWidth", "disabled", disabled);
            }
        },
        {
            onChange: "fixedFactors", execute: function(context) {
                this.calcModelTerms(context);
            }
        },
        {
            onChange: "modelTerms", execute: function(context) {
                this.filterModelTerms(context);
            }
        },
        {
            onEvent: "analysis.initialising", execute: function(context) {
                this._lastVariableList = null;
                this._lastCurrentList = null;
                this._initialising = true;
            }
        },
        {
            onEvent: "analysis.initialised", execute: function(context) {
                if (this._lastVariableList === null)
                    this.calcModelTerms(context);

                if (this._lastCurrentList === null)
                    this.filterModelTerms(context);

                this._initialising = false;
            }
        }
    ],

    calcModelTerms: function(context) {
        var variableList = this.clone(context.getValue("fixedFactors"));
        if (variableList === null)
            variableList = [];

        context.setValue("modelSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));
        context.setValue("plotsSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));
        context.setValue("postHocSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));

        var diff = { removed: [], added: [] };
        if (this._lastVariableList !== null)
            diff = this.findDifferences(this._lastVariableList, variableList);
        this._lastVariableList = variableList;

        if (this._initialising)
            return;

        var currentList = this.clone(context.getValue("modelTerms"));
        if (currentList === null)
            currentList = [];

        for (var i = 0; i < diff.removed.length; i++) {
            for (var j = 0; j < currentList.length; j++) {
                if (FormatDef.variable.contains(currentList[j], diff.removed[i])) {
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
                if (Array.isArray(newVar))
                    newVar = this.clone(newVar);
                else
                    newVar = [newVar];
                newVar.push(diff.added[i])
                currentList.push(newVar);
            }
            currentList.push(diff.added[i]);
        }

        context.setValue("modelTerms", currentList);

        this.updateContrasts(context, variableList);
    },

    filterModelTerms : function(context) {
        var currentList = this.clone(context.getValue("modelTerms"));
        if (currentList === null)
            currentList = [];

        var list = this.convertArrayToSupplierList(currentList, FormatDef.variable);
        context.setValue("marginalMeansSupplier", list);

        var diff = { removed: [], added: [] };
        if (this._lastCurrentList !== null)
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
            context.setValue("modelTerms", currentList);

        if (diff.removed.length > 0) {
            itemsRemoved = false;
            var margMeans = this.clone(context.getValue("margMeans"));
            if (margMeans === null)
                margMeans = [];

            for (var i = 0; i < diff.removed.length; i++) {
                var item = diff.removed[i];
                for (var j = 0; j < margMeans.length; j++) {
                    if (FormatDef.variable.contains(margMeans[j], item)) {
                        margMeans.splice(j, 1);
                        j -= 1;
                        itemsRemoved = true;
                    }
                }
            }

            if (itemsRemoved) {
                context.setValue("margMeans", margMeans);
            }
        }
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

    convertArrayToSupplierList: function(array, format) {
        var list = [];
        for (var i = 0; i < array.length; i++) {
            list.push({ value: new FormatDef.constructor(array[i], format) });
        }
        return list;
    },

    clone: function(object) {
        return JSON.parse(JSON.stringify(object));
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

    listContains: function(list, value) {
        for (var i = 0; i < list.length; i++) {
            if (FormatDef.variable.isEqual(list[i], value))
                return true;
        }

        return false;
    }
});

module.exports = { LayoutDef : anovaLayout, options: options, customControls: controls };
