
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var view = View.extend({

    initialize: function(ui) {

        ui.ciWidth.setEnabled(ui.plotError.value() === "ci");

        this.calcModelTerms(ui);
        this.filterModelTerms(ui);
    },

    events : [
        {
            onChange: "plotError", execute: function(ui) {
                ui.ciWidth.setEnabled(ui.plotError.value() === "ci");
            }
        },
        {
            onChange: ["fixedFactors", "covariates"], execute: function(ui) {
                this.calcModelTerms(ui);
            }
        },
        {
            onEvent: "modelTerms.preprocess", execute: function(ui, data) {
                if (data.intoSelf === false)
                    data.items = this.getItemCombinations(data.items);
            }
        },
        {
            onChange: "modelTerms", execute: function(ui) {
                this.filterModelTerms(ui);
            }
        }
    ],

    calcModelTerms : function(ui) {
        var variableList = this.cloneArray(ui.fixedFactors.value(), []);
        var covariatesList = this.cloneArray(ui.covariates.value(), []);

        var combinedList = variableList.concat(covariatesList);

        ui.modelSupplier.setValue(this.valuesToItems(combinedList, FormatDef.variable));
        ui.plotsSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));
        ui.postHocSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));

        var diff = this.findChanges("variableList", variableList, true, FormatDef.variable);
        var diff2 = this.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
        var combinedDiff = this.findChanges("combinedList", combinedList, true, FormatDef.variable);


        var termsList = this.cloneArray(ui.modelTerms.value(), []);
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
            ui.modelTerms.setValue(termsList);

        this.updateContrasts(ui, variableList);
    },

    filterModelTerms: function(ui) {
        var termsList = this.cloneArray(ui.modelTerms.value(), []);
        var diff = this.findChanges("termsList", termsList, true, FormatDef.term);

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

        if (this.sortArraysByLength(termsList))
            changed = true;

        if (changed)
            ui.modelTerms.setValue(termsList);
    },

    updateContrasts : function(ui, variableList) {
        var currentList = this.cloneArray(ui.contrasts.value(), []);

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

        ui.contrasts.setValue(list3);
    },

    containsCovariate: function(value, covariates) {
        for (var i = 0; i < covariates.length; i++) {
            if (FormatDef.term.contains(value, covariates[i]))
                return true;
        }

        return false;
    }

});

module.exports = view;
