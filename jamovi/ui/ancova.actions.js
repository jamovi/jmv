
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var actions = Actions.extend({

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
        },
        {
            onEvent: "view.loaded", execute: function(ui) {
                this._loaded = true;
            }
        },
        {
            onEvent: "view.data-initialising", execute: function(ui) {
                this._lastVariableList = null;
                this._lastCurrentList = null;
                this._lastCombinedList = null;
                this._lastCovariatesList = null;
                this._initialising = true;
            }
        },
        {
            onEvent: "view.data-initialised", execute: function(ui) {
                if (this._lastVariableList === null || this._lastCombinedList === null || this._lastCovariatesList === null)
                    this.calcModelTerms(ui);

                if (this._lastCurrentList === null)
                    this.filterModelTerms(ui);

                this._initialising = false;
            }
        }
    ],

    calcModelTerms : function(ui) {
        var variableList = this.clone(ui.fixedFactors.value());
        if (variableList === null)
            variableList = [];

        var covariatesList = this.clone(ui.covariates.value());
        if (covariatesList === null)
            covariatesList = [];

        var combinedList = variableList.concat(covariatesList);

        ui.modelSupplier.setValue(this.valuesToItems(combinedList, FormatDef.variable));
        ui.plotsSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));
        ui.postHocSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));


        var diff = { removed: [], added: [] };
        if (this._lastVariableList !== null)
            diff = this.findDifferences(this._lastVariableList, variableList, FormatDef.variable);
        this._lastVariableList = variableList;

        var diff2 = { removed: [], added: [] };
        if (this._lastCovariatesList !== null)
            diff2 = this.findDifferences(this._lastCovariatesList, covariatesList, FormatDef.variable);
        this._lastCovariatesList = covariatesList;

        var combinedDiff = { removed: [], added: [] };
        if (this._lastCombinedList !== null)
            combinedDiff = this.findDifferences(this._lastCombinedList, combinedList, FormatDef.variable);
        this._lastCombinedList = combinedList;


        if (this._initialising || !this._loaded)
            return;

        var termsList = this.clone(ui.modelTerms.value());
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
            ui.modelTerms.setValue(termsList);

        this.updateContrasts(ui, variableList);
    },

    filterModelTerms: function(ui) {
        var termsList = this.clone(ui.modelTerms.value());
        if (termsList === null)
            termsList = [];

        var diff = null;
        if ( ! this._initialising && this._loaded)
            diff = this.findDifferences(this._lastCurrentList, termsList, FormatDef.term);
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

        if (this.sortArraysByLength(termsList))
            changed = true;

        if (changed)
            ui.modelTerms.setValue(termsList);
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

    containsCovariate: function(value, covariates) {
        for (var i = 0; i < covariates.length; i++) {
            if (FormatDef.term.contains(value, covariates[i]))
                return true;
        }

        return false;
    }

});

module.exports = actions;
