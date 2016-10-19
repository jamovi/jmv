
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./anova.options');

var controls = new ControlManager();

var anovaLayout = ui.extend({

    label: "ANOVA",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "variablesupplier",
            name: "variableSupplier",
            suggested: ["continuous", "nominal", "ordinal"],
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    name: "dependent",
                    type:"variabletargetlistbox",
                    label: "Dependent Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                },
                {
                    name: "fixedFactors",
                    type:"variabletargetlistbox",
                    label: "Fixed Factors",
                    showColumnHeaders: false,
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
                { name: "etaSq",   type:"checkbox", label: "η²" },
                { name: "etaSqP",  type:"checkbox", label: "partial η²" },
                { name: "omegaSq", type:"checkbox", label: "ω²" }
            ]
        },
        {
            name: "modelgroup",
            type: "collapsebox",
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
                            format: FormatDef.term,
                            label: "Model Terms",
                            showColumnHeaders: false,
                            valueFilter: "unique",
                            columns: [
                                { type: "listitem.termlabel", name: "column1", label: "", format: FormatDef.term, stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "layoutbox",
                    controls: [
                        { name: "ss", type:"combobox", label: "Sum of squares", options: [
                            { label: 'Type 1', value: '1' },
                            { label: 'Type 2', value: '2' },
                            { label: 'Type 3', value: '3' }
                        ] }
                    ]
                }
            ]
        },
        {
            name: "assCheckgroup",
            type: "collapsebox",
            label: "Assumption Checks",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                { name: "homo", type:"checkbox", label: "Homogeneity tests" },
                { name: "qq", type:"checkbox", label: "Q-Q plot of residuals" }
            ]
        },
        {
            name: "contrastgroup",
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
                        { type: "listitem.variablelabel", name: "var", label: "", selectable: false, format: FormatDef.variable, stretchFactor: 0.5 },
                        { type: "listitem.combobox", name: "type", label: "", selectable: false, format: FormatDef.string, stretchFactor: 1, options: ['none', 'deviation', 'simple', 'difference', 'helmert', 'repeated', 'polynomial'] }
                    ]
                }
            ]
        },
        {
            name: "posthocgroup",
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
                            showColumnHeaders: false,
                            columns: [
                                { type: "listitem.termlabel", name: "column1", label: "", format: FormatDef.term, stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    type: "label",
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
            name: "descplotgroup",
            type: "collapsebox",
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
                    type: "label",
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
            type: "collapsebox",
            label: "Additional Options",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "label",
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
            onEvent: "modelTerms.preprocess", execute: function(context, data) {
                data.items = this._variableListInteractions(data.items);
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

    calcModelTerms: function(context) {
        var variableList = this.clone(context.getValue("fixedFactors"));
        if (variableList === null)
            variableList = [];

        context.setValue("modelSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));
        context.setValue("plotsSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));
        //context.setValue("postHocSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));

        var varsDiff = { removed: [], added: [] };
        if (this._lastVariableList !== null)
            varsDiff = this.findDifferences(FormatDef.variable, this._lastVariableList, variableList);
        this._lastVariableList = variableList;

        if (this._initialising)
            return;

        var termsList = this.clone(context.getValue("modelTerms"));
        if (termsList === null)
            termsList = [];

        for (var i = 0; i < varsDiff.removed.length; i++) {
            for (var j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], varsDiff.removed[i])) {
                    termsList.splice(j, 1);
                    j -= 1;
                }
            }
        }

        for (var i = 0; i < varsDiff.added.length; i++) {
            var listLength = termsList.length;
            for (var j = 0; j < listLength; j++) {
                var newTerm = this.clone(termsList[j]);
                newTerm.push(varsDiff.added[i]);
                termsList.push(newTerm);
            }
            termsList.push([varsDiff.added[i]]);
        }

        context.setValue("modelTerms", termsList);

        this.updateContrasts(context, variableList);
        this.updateFactorDependents(context, variableList, termsList);
    },

    filterModelTerms : function(context) {
        var termsList = this.clone(context.getValue("modelTerms"));
        if (termsList === null)
            termsList = [];

        var list = this.convertArrayToSupplierList(termsList, FormatDef.term);
        context.setValue("postHocSupplier", list);

        var termsDiff = { removed: [], added: [] };
        if (this._lastCurrentList !== null)
            termsDiff = this.findDifferences(FormatDef.term, this._lastCurrentList, termsList);
        this._lastCurrentList = termsList;

        if (this._initialising)
            return;

        var changed = false;
        if (termsDiff.removed.length > 0 && termsList !== null) {
            var itemsRemoved = false;
            for (var i = 0; i < termsDiff.removed.length; i++) {
                var item = termsDiff.removed[i];
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
            context.setValue("modelTerms", termsList);

        if (termsDiff.removed.length > 0) {
            itemsRemoved = false;
            var postHoc = this.clone(context.getValue("postHoc"));
            if (postHoc === null)
                postHoc = [];

            for (var i = 0; i < termsDiff.removed.length; i++) {
                var item = termsDiff.removed[i];
                for (var j = 0; j < postHoc.length; j++) {
                    if (FormatDef.term.contains(postHoc[j], item)) {
                        postHoc.splice(j, 1);
                        j -= 1;
                        itemsRemoved = true;
                    }
                }
            }

            if (itemsRemoved)
                context.setValue("postHoc", postHoc);
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

    cleanDependentOption: function(context, sourceList, format, dependantName, dependantIsList) {
        var dependantList = this.clone(context.getValue(dependantName));
        if (dependantIsList && dependantList === null)
            dependantList = [];

        var changed = false;

        if (dependantIsList) {
            for (var i = 0; i < dependantList.length; i++) {
                if (this.listContains(sourceList, dependantList[i], format) === false) {
                    dependantList.splice(i, 1);
                    i -= 1;
                    changed = true;
                }
            }
        }
        else if (this.listContains(sourceList, dependantList, format) === false) {
            dependantList = null;
            changed = true;
        }

        if (changed)
            context.setValue(dependantName, dependantList);
    },

    updateFactorDependents : function(context, factorList, modelList) {
        this.cleanDependentOption(context, factorList, FormatDef.variable, "descPlotsHAxis", false);
        this.cleanDependentOption(context, factorList, FormatDef.variable, "descPlotsSepLines", false);
        this.cleanDependentOption(context, factorList, FormatDef.variable, "descPlotsSepPlots", false);
        this.cleanDependentOption(context, modelList, FormatDef.term, "postHoc", true);
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

    listContains: function(list, value, format) {
        for (var i = 0; i < list.length; i++) {
            if (format.isEqual(list[i], value))
                return true;
        }

        return false;
    }
});

module.exports = { ui : anovaLayout, options: options, customControls: controls };
