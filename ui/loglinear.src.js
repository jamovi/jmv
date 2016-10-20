
'use strict';

var options = require("./loglinear.options");

var layout = ui.extend({

    label: "Log-Linear Regression",
    type: "root",
    stage: 2,
    controls: [
        {
            type: "variablesupplier",
            suggested: ["continuous", "nominal", "ordinal"],
            name: "variableSupplier",
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    name: "counts",
                    type: "variabletargetlistbox",
                    label: "Counts (optional)",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                },
                {
                    name: "factors",
                    type: "variabletargetlistbox",
                    label: "Factors",
                    showColumnHeaders: false,
                    columns: [
                        { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
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
                            label: "Model Terms",
                            showColumnHeaders: false,
                            valueFilter: "unique",
                            columns: [
                                { type: "listitem.variablelabel", name: "column1", label: "", format: FormatDef.variable, stretchFactor: 1 }
                            ]
                        }
                    ]
                }
            ]
        },
        {
            name: "statisticsgroup",
            type: "collapsebox",
            label: "Statistics",
            collapsed: true,
            stretchFactor: 1,
            controls : [
                {
                    type: "label",
                    label: "Regression Coefficients",
                    controls: [
                        { name: "est", type:"checkbox", label: "Estimates" },
                        {
                            name: "ci", type:"checkbox", label: "Confidence intervals" ,
                            controls: [ { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" } ]
                        }
                    ]
                }
            ]
        }
    ],

    actions: [
        {
            onChange: "ci", execute: function(context) {
                var disabled = context.getValue("ci") === false;
                context.set("ciWidth", "disabled", disabled);
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
            onChange: "factors", execute: function(context) {
                this.calcModelTerms(context);
            }
        },
        {
            onChange: "modelTerms", execute: function(context) {
                this.filterModelTerms(context);
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

    calcModelTerms: function(context) {
        var variableList = this.clone(context.getValue("factors"));
        if (variableList === null)
            variableList = [];

        context.setValue("modelSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));

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
    },

    filterModelTerms : function(context) {
        var currentList = this.clone(context.getValue("modelTerms"));
        if (currentList === null)
            currentList = [];

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
    },

    convertArrayToSupplierList: function(array, format) {
        var list = [];
        for (var i = 0; i < array.length; i++) {
            list.push({ value: new FormatDef.constructor(array[i], format) });
        }
        return list;
    },


});

module.exports = { ui : layout, options: options };
