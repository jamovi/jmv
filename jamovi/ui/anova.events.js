
const events = {
    update: function(ui) {
        ui.ciWidth.setEnabled(ui.errBarDef_ci.value());

        calcModelTerms(ui, this);
        filterModelTerms(ui, this);
    },

    onChange_errBarDef_ci: function(ui) {
        ui.ciWidth.setEnabled(ui.errBarDef_ci.value());
    },

    onChange_fixedFactors: function(ui) {
        calcModelTerms(ui, this);
    },

    onChange_modelTerms: function(ui) {
        filterModelTerms(ui, this);
    },

    onEvent_modelTerms_preprocess: function(ui, data) {
        if (data.intoSelf === false)
            data.items = this.getItemCombinations(data.items);
    }
};

var calcModelTerms = function(ui, context) {
    var variableList = context.cloneArray(ui.fixedFactors.value(), []);

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
    updateFactorDependents(ui, variableList, termsList, context);
};

var filterModelTerms = function(ui, context) {
    var termsList = context.cloneArray(ui.modelTerms.value(), []);

    var list = context.valuesToItems(termsList, FormatDef.term);
    ui.postHocSupplier.setValue(list);

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

    if (termsDiff.removed.length > 0) {
        itemsRemoved = false;
        var postHoc = context.cloneArray(ui.postHoc.value(), []);
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

var cleanDependentOption = function(ui, sourceList, dependantName, dependantIsList, format, context) {
    var dependantList = context.clone(ui[dependantName].value());
    if (dependantIsList && dependantList === null)
        dependantList = [];

    var changed = false;

    if (dependantIsList) {
        for (var i = 0; i < dependantList.length; i++) {
            if (context.listContains(sourceList, dependantList[i], format) === false) {
                dependantList.splice(i, 1);
                i -= 1;
                changed = true;
            }
        }
    }
    else if (context.listContains(sourceList, dependantList, format) === false) {
        dependantList = null;
        changed = true;
    }

    if (changed)
        ui[dependantName].setValue(dependantList);
};

var updateFactorDependents = function(ui, factorList, modelList, context) {
    cleanDependentOption(ui, factorList, "descPlotsHAxis", false, FormatDef.variable, context);
    cleanDependentOption(ui, factorList, "descPlotsSepLines", false, FormatDef.variable, context);
    cleanDependentOption(ui, factorList, "descPlotsSepPlots", false, FormatDef.variable, context);
    cleanDependentOption(ui, modelList, "postHoc", true, FormatDef.term, context);
};

module.exports = events;
