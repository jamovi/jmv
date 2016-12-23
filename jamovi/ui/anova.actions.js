
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var view = View.extend({

    initialize: function(ui) {
        ui.ciWidth.setEnabled(ui.plotError.value() === "ci");

        this.calcModelTerms(ui);
        this.filterModelTerms(ui);
    },

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
        }
    ],

    calcModelTerms: function(ui) {
        var variableList = this.cloneArray(ui.fixedFactors.value(), []);

        ui.modelSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));
        ui.plotsSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));

        var varsDiff = this.findChanges("variableList", variableList, true, FormatDef.variable);
        var termsList = this.cloneArray(ui.modelTerms.value(), []);

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

        termsList = this.getCombinations(varsDiff.added, termsList);
        termsChanged = termsChanged || varsDiff.added.length > 0;

        if (termsChanged)
            ui.modelTerms.setValue(termsList);

        this.updateContrasts(ui, variableList);
        this.updateFactorDependents(ui, variableList, termsList);
    },

    filterModelTerms : function(ui) {
        var termsList = this.cloneArray(ui.modelTerms.value(), []);

        var list = this.valuesToItems(termsList, FormatDef.term);
        ui.postHocSupplier.setValue(list);

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

        if (this.sortArraysByLength(termsList))
            changed = true;

        if (changed)
            ui.modelTerms.setValue(termsList);

        if (termsDiff.removed.length > 0) {
            itemsRemoved = false;
            var postHoc = this.cloneArray(ui.postHoc.value(), []);
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

module.exports = view;
