
const events = {
    update: function(ui) {
        calcModelTerms(ui, this);
        filterModelTerms(ui, this);
        updatePostHocSupplier(ui, this);
        updateModelLabels(ui.emMeans, 'Term');
    },

    onChange_factors: function(ui) {
        calcModelTerms(ui, this);
    },

    onChange_covariates: function(ui) {
        calcModelTerms(ui, this);
    },

    onChange_modelTerms: function(ui) {
        filterModelTerms(ui, this);
        updatePostHocSupplier(ui, this);
    },

    /*onChange_plotsSupplier: function(ui) {
        let values = this.itemsToValues(ui.plotsSupplier.value());
        this.checkValue(ui.plotHAxis, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepLines, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepPlots, false, values, FormatDef.variable);
    },*/

    onChange_emMeansSupplier: function(ui) {
        let values = this.itemsToValues(ui.emMeansSupplier.value());
        this.checkValue(ui.emMeans, 2, values, FormatDef.variable);
    },

    onChange_postHocSupplier: function(ui) {
        let values = this.itemsToValues(ui.postHocSupplier.value());
        this.checkValue(ui.postHoc, true, values, FormatDef.term);
    },

    onEvent_emMeans_listItemsChanged: function(ui) {
        updateModelLabels(ui.emMeans, 'Term');
    }
};

let calcMarginalMeansSupplier = function(ui, context) {

    let b1 = context.cloneArray(ui.factors.value(), []);
    b1 = context.valuesToItems(b1, FormatDef.variable);

    if (ui.emMeansSupplier)
        ui.emMeansSupplier.setValue(b1);
};

let updateModelLabels = function(list, blockName) {
    list.applyToItems(0, (item, index) => {
        item.controls[0].setPropertyValue("label", blockName + " " + (index + 1) );
    });
};

var calcModelTerms = function(ui, context) {
    var variableList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);

    var combinedList = variableList.concat(covariatesList);

    ui.modelSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    //ui.plotsSupplier.setValue(context.valuesToItems(variableList, FormatDef.variable));

    calcMarginalMeansSupplier(ui, context);

    var diff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    var diff2 = context.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
    var combinedDiff = context.findChanges("combinedList", combinedList, true, FormatDef.variable);

    var termsList = context.cloneArray(ui.modelTerms.value(), []);
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

    for (var a = 0; a < diff.added.length; a++) {
        let item = diff.added[a];
        var listLength = termsList.length;
        for (var j = 0; j < listLength; j++) {
            var newTerm = context.clone(termsList[j]);
            if (containsCovariate(newTerm, covariatesList) === false) {
                if (context.listContains(newTerm, item, FormatDef.variable) === false) {
                    newTerm.push(item)
                    if (context.listContains(termsList, newTerm , FormatDef.term) === false) {
                        termsList.push(newTerm);
                        termsChanged = true;
                    }
                }
            }
        }
        if (context.listContains(termsList, [item] , FormatDef.term) === false) {
            termsList.push([item]);
            termsChanged = true;
        }
    }

    for (var a = 0; a < diff2.added.length; a++) {
        let item = diff2.added[a];
        if (context.listContains(termsList, [item] , FormatDef.term) === false) {
            termsList.push([item]);
            termsChanged = true;
        }
    }

    if (termsChanged)
        ui.modelTerms.setValue(termsList);

    updateContrasts(ui, variableList, context);
};

var updatePostHocSupplier = function(ui, context) {
    var termsList = context.cloneArray(ui.modelTerms.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var list = [];
    for (var j = 0; j < termsList.length; j++) {
        var term = termsList[j];
        if (containsCovariate(term, covariatesList) === false)
            list.push(term)
    }
    ui.postHocSupplier.setValue(context.valuesToItems(list, FormatDef.term));
};

var filterModelTerms = function(ui, context) {
    var termsList = context.cloneArray(ui.modelTerms.value(), []);
    var diff = context.findChanges("termsList", termsList, true, FormatDef.term);

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

    if (context.sortArraysByLength(termsList))
        changed = true;

    if (changed)
        ui.modelTerms.setValue(termsList);
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

var containsCovariate = function(value, covariates) {
    for (var i = 0; i < covariates.length; i++) {
        if (FormatDef.term.contains(value, covariates[i]))
            return true;
    }

    return false;
};


module.exports = events;
