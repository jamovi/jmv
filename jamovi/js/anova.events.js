
const events = {
    update: function(ui) {
        calcModelTerms(ui, this);
        filterModelTerms(ui, this);
        updateModelLabels(ui.emMeans, 'Term');
    },

    onChange_factors: function(ui) {
        calcModelTerms(ui, this);
    },

    onChange_modelTerms: function(ui) {
        filterModelTerms(ui, this);
    },

    onUpdate_emMeansSupplier: function(ui) {
        calcMarginalMeansSupplier(ui, this);
    },

    onUpdate_modelSupplier: function(ui) {
        let variableList = this.cloneArray(ui.factors.value(), []);
        ui.modelSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));
    },

    onUpdate_postHocSupplier: function(ui) {
        let termsList = this.cloneArray(ui.modelTerms.value(), []);
        ui.postHocSupplier.setValue(this.valuesToItems(termsList, FormatDef.term));
    },

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

    ui.modelSupplier.setValue(context.valuesToItems(variableList, FormatDef.variable));
    //ui.plotsSupplier.setValue(context.valuesToItems(variableList, FormatDef.variable));

    calcMarginalMeansSupplier(ui, context);

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

    //Remove common terms
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
    /////////////////////

    //Sort terms
    if (context.sortArraysByLength(termsList))
        changed = true;
    ////////////

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
