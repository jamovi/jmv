
'use strict';

var actions = {
    events: [
        {
            onChange: "ci", execute: function(ui) {
                ui.ciWidth.setEnabled(ui.ci.value());
            }
        },
        {
            onEvent: "view.data-initialising", execute: function(ui) {
                this._lastVariableList = null;
                this._lastCurrentList = null;
                this._initialising = true;
            }
        },
        {
            onChange: "factors", execute: function(ui) {
                this.calcModelTerms(ui);
            }
        },
        {
            onChange: "modelTerms", execute: function(ui) {
                this.filterModelTerms(ui);
            }
        },
        {
            onEvent: "view.data-initialised", execute: function(ui) {
                if (this._lastVariableList === null)
                    this.calcModelTerms(ui);

                if (this._lastCurrentList === null)
                    this.filterModelTerms(ui);

                this._initialising = false;
            }
        }
    ],

    calcModelTerms: function(ui) {
        var variableList = this.clone(ui.factors.value());
        if (variableList === null)
            variableList = [];

        ui.modelSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));

        var diff = { removed: [], added: [] };
        if (this._lastVariableList !== null)
            diff = this.findDifferences(this._lastVariableList, variableList);
        this._lastVariableList = variableList;

        if (this._initialising)
            return;

        var currentList = this.clone(ui.modelTerms.value());
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

        ui.modelTerms.setValue(currentList);
    },

    filterModelTerms : function(ui) {
        var currentList = this.clone(ui.modelTerms.value());
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

        if (this.sortArraysByLength(currentList))
            changed = true;

        if (changed)
            ui.modelTerms.setValue(currentList);
    },
}

module.exports = actions;
