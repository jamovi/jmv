
'use strict';

var view = View.extend({
    initialize: function(ui) {
        ui.ciWidth.setEnabled(ui.ci.value());
        this.calcModelTerms(ui);
        this.filterModelTerms(ui);
    },
    events: [
        {
            onChange: "ci", execute: function(ui) {
                ui.ciWidth.setEnabled(ui.ci.value());
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
            onEvent: "modelTerms.preprocess", execute: function(ui, data) {
                if (data.intoSelf === false)
                    data.items = this.getItemCombinations(data.items);
            }
        }
    ],

    calcModelTerms: function(ui) {
        var variableList = this.cloneArray(ui.factors.value(), []);

        ui.modelSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));

        var diff = this.findChanges("variableList", variableList, true, FormatDef.variable);

        var currentList = this.cloneArray(ui.modelTerms.value(), []);

        var termsChanged = false;
        for (var i = 0; i < diff.removed.length; i++) {
            for (var j = 0; j < currentList.length; j++) {
                if (FormatDef.term.contains(currentList[j], diff.removed[i])) {
                    currentList.splice(j, 1);
                    termsChanged = true;
                    j -= 1;
                }
            }
        }

        if (currentList === null)
            currentList = [];

        currentList = this.getCombinations(diff.added, currentList);
        termsChanged = termsChanged || diff.added.length > 0;

        if (termsChanged)
            ui.modelTerms.setValue(currentList);
    },

    filterModelTerms : function(ui) {
        var currentList = this.cloneArray(ui.modelTerms.value(), []);

        var diff = this.findChanges("currentList", currentList, true, FormatDef.term);

        var changed = false;
        if (diff.removed.length > 0 && currentList !== null) {
            var itemsRemoved = false;
            for (var i = 0; i < diff.removed.length; i++) {
                var item = diff.removed[i];
                for (var j = 0; j < currentList.length; j++) {
                    if (FormatDef.term.contains(currentList[j], item)) {
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
});

module.exports = view;
