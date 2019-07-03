
'use strict';

module.exports = {

    view_updated: function(ui) {
        this.calcModelTerms(ui);
        this.filterModelTerms(ui);
        this.updateModelLabels(ui.emMeans, 'Term');
    },

    factors_changed: function(ui) {
        this.calcModelTerms(ui);
    },

    modelTerms_changed: function(ui) {
        this.filterModelTerms(ui);
    },

    emMeansSupplier_updated: function(ui) {
        this.calcMarginalMeansSupplier(ui);
    },

    modelSupplier_updated: function(ui) {
        let variableList = utils.clone(ui.factors.value(), []);
        ui.modelSupplier.setValue(utils.valuesToItems(variableList, FormatDef.variable));
    },

    postHocSupplier_updated: function(ui) {
        let termsList = utils.clone(ui.modelTerms.value(), []);
        ui.postHocSupplier.setValue(utils.valuesToItems(termsList, FormatDef.term));
    },

    emMeansSupplier_changed: function(ui) {
        let values = utils.itemsToValues(ui.emMeansSupplier.value());
        utils.checkValue(ui.emMeans, 2, values, FormatDef.variable);
    },

    postHocSupplier_changed: function(ui) {
        let values = utils.itemsToValues(ui.postHocSupplier.value());
        utils.checkValue(ui.postHoc, true, values, FormatDef.term);
    },

    emMeans_listItemsChanged: function(ui) {
        this.updateModelLabels(ui.emMeans, 'Term');
    },

    filterModelTerms: function(ui) {
        var termsList = utils.clone(ui.modelTerms.value(), []);

        //Remove common terms
        var termsDiff = this.findChanges("currentList", termsList, true, FormatDef.term);
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
        /////////////////////

        //Sort terms
        if (utils.sortArraysByLength(termsList))
            changed = true;
        ////////////

        if (changed)
            ui.modelTerms.setValue(termsList);

        ui.postHocSupplier.setValue(utils.valuesToItems(termsList, FormatDef.term));
    },

    calcMarginalMeansSupplier: function(ui) {

        let b1 = utils.clone(ui.factors.value(), []);
        b1 = utils.valuesToItems(b1, FormatDef.variable);

        if (ui.emMeansSupplier)
            ui.emMeansSupplier.setValue(b1);
    },

    updateModelLabels: function(list, blockName) {
        list.applyToItems(0, (item, index) => {
            item.controls[0].setPropertyValue("label", blockName + " " + (index + 1) );
        });
    },

    calcModelTerms: function(ui) {
        var variableList = utils.clone(ui.factors.value(), []);

        ui.modelSupplier.setValue(utils.valuesToItems(variableList, FormatDef.variable));

        this.calcMarginalMeansSupplier(ui);

        var varsDiff = this.findChanges("variableList", variableList, true, FormatDef.variable);
        var termsList = utils.clone(ui.modelTerms.value(), []);

        var termsChanged = false;
        for (var i = 0; i < varsDiff.removed.length; i++) {
            for (var j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], varsDiff.removed[i])) {
                    termsList.splice(j, 1);
                    termsChanged = true;
                    j -= 1;
                }
            }
        }

        termsList = utils.getCombinations(varsDiff.added, termsList);
        termsChanged = termsChanged || varsDiff.added.length > 0;

        if (termsChanged)
            ui.modelTerms.setValue(termsList);

        this.updateContrasts(ui, variableList);
    },

    updateContrasts: function(ui, variableList) {
        let value = ui.contrasts.value();
        var currentList = utils.clone(value, []);

        var list3 = [];
        for (let i = 0; i < variableList.length; i++) {
            let found = null;
            for (let j = 0; j < currentList.length; j++) {
                if (currentList[j].var === variableList[i]) {
                    found = currentList[j];
                    break;
                }
            }
            if (found === null)
                list3.push({ var: variableList[i], type: "none" });
            else
                list3.push(found);
        }

        let oldLength = value === null ? 0 : value.length;

        let changed = oldLength !== list3.length || JSON.stringify(value) !== JSON.stringify(list3);

        if (changed)
            ui.contrasts.setValue(list3);
    }

};
