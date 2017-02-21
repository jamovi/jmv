
const events = {
    update: function(ui) {
        calcModelTerms(ui, this);
        filterModelTerms(ui, this);
    },

    onChange_factors: function(ui) {
        calcModelTerms(ui, this);
    },

    onChange_modelTerms: function(ui) {
        filterModelTerms(ui, this);
    },

    onChange_plotsSupplier: function(ui) {
        let values = this.itemsToValues(ui.plotsSupplier.value());
        this.checkValue(ui.plotHAxis, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepLines, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepPlots, false, values, FormatDef.variable);
    },

    onChange_postHocSupplier: function(ui) {
        let values = this.itemsToValues(ui.postHocSupplier.value());
        this.checkValue(ui.postHoc, true, values, FormatDef.term);
    },

    onEvent_modelTerms_preprocess: function(ui, data) {
        if (data.intoSelf === false)
            data.items = this.getItemCombinations(data.items);
    }
};

var calcModelTerms = function(ui, context) {
    var variableList = context.cloneArray(ui.factors.value(), []);

    ui.modelSupplier.setValue(context.valuesToItems(variableList, FormatDef.variable));
    ui.plotsSupplier.setValue(context.valuesToItems(variableList, FormatDef.variable));

    var varsDiff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    var termsList = context.cloneArray(ui.modelTerms.value(), []);

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

    termsList = context.getCombinations(varsDiff.added, termsList);
    termsChanged = termsChanged || varsDiff.added.length > 0;

    if (termsChanged)
        ui.modelTerms.setValue(termsList);

    updateContrasts(ui, variableList, context);
};

var filterModelTerms = function(ui, context) {
    var termsList = context.cloneArray(ui.modelTerms.value(), []);

    var termsDiff = context.findChanges("currentList", termsList, true, FormatDef.term);

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

    if (context.sortArraysByLength(termsList))
        changed = true;

    if (changed)
        ui.modelTerms.setValue(termsList);

    ui.postHocSupplier.setValue(context.valuesToItems(termsList, FormatDef.term));
};

var updateContrasts = function(ui, variableList, context) {
    var currentList = context.cloneArray(ui.contrasts.value(), []);

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
};

module.exports = events;
