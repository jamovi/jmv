
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var actions = Actions.extend({

    events: [
        {
            onChange: "plotError", execute: function(ui) {
                ui.ciWidth.setEnabled(ui.plotError.value() === "ci");
            }
        },
        {
            onChange: "fixedFactors", execute: function(ui) {
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
                this._initialising = true;
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
        var variableList = this.clone(ui.fixedFactors.value());
        if (variableList === null)
            variableList = [];

        ui.modelSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));
        ui.plotsSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));

        var varsDiff = { removed: [], added: [] };
        if (this._lastVariableList !== null)
            varsDiff = this.findDifferences(this._lastVariableList, variableList, FormatDef.variable);
        this._lastVariableList = variableList;

        if (this._initialising || !this._loaded)
            return;

        var termsList = this.clone(ui.modelTerms.value());
        if (termsList === null)
            termsList = [];

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

        for (var i = 0; i < varsDiff.added.length; i++) {
            var listLength = termsList.length;
            for (var j = 0; j < listLength; j++) {
                var newTerm = this.clone(termsList[j]);
                newTerm.push(varsDiff.added[i]);
                termsList.push(newTerm);
            }
            termsList.push([varsDiff.added[i]]);
            termsChanged = true;
        }

        if (termsChanged)
            ui.modelTerms.setValue(termsList);

        this.updateContrasts(ui, variableList);
        this.updateFactorDependents(ui, variableList, termsList);
    },

    filterModelTerms : function(ui) {
        var termsList = this.clone(ui.modelTerms.value());
        if (termsList === null)
            termsList = [];

        var list = this.valuesToItems(termsList, FormatDef.term);
        ui.postHocSupplier.setValue(list);

        var termsDiff = { removed: [], added: [] };
        if (this._lastCurrentList !== null)
            termsDiff = this.findDifferences(this._lastCurrentList, termsList, FormatDef.term);
        this._lastCurrentList = termsList;

        if (this._initialising || !this._loaded)
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

        if (this.sortArraysByLength(termsList))
            changed = true;

        if (changed)
            ui.modelTerms.setValue(termsList);

        if (termsDiff.removed.length > 0) {
            itemsRemoved = false;
            var postHoc = this.clone(ui.postHoc.value());
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
                ui.postHoc.setValue(postHoc);
        }
    },

    updateContrasts : function(ui, variableList) {
        var currentList = this.clone(ui.contrasts.value());
        if (currentList === null)
            currentList = [];

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

    cleanDependentOption: function(ui, sourceList, dependantName, dependantIsList, format) {
        var dependantList = this.clone(ui[dependantName].value());
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
            ui[dependantName].setValue(dependantList);
    },

    updateFactorDependents : function(ui, factorList, modelList) {
        this.cleanDependentOption(ui, factorList, "descPlotsHAxis", false, FormatDef.variable);
        this.cleanDependentOption(ui, factorList, "descPlotsSepLines", false, FormatDef.variable);
        this.cleanDependentOption(ui, factorList, "descPlotsSepPlots", false, FormatDef.variable);
        this.cleanDependentOption(ui, modelList, "postHoc", true, FormatDef.term);
    }
});

module.exports = actions;
