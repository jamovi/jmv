
const events = {
    update: function(ui) {
        ui.ciWidth.setEnabled(ui.ci.value());

        calcModelTerms(ui, this);
        filterModelTerms(ui, this);
    },

    onChange_ci: function(ui) {
        ui.ciWidth.setEnabled(ui.ci.value());
    },

    onChange_factors: function(ui) {
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
    var variableList = context.cloneArray(ui.factors.value(), []);

    ui.modelSupplier.setValue(context.valuesToItems(variableList, FormatDef.variable));

    var diff = context.findChanges("variableList", variableList, true, FormatDef.variable);

    var currentList = context.cloneArray(ui.modelTerms.value(), []);

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

    currentList = context.getCombinations(diff.added, currentList);
    termsChanged = termsChanged || diff.added.length > 0;

    if (termsChanged)
        ui.modelTerms.setValue(currentList);
};

var filterModelTerms = function(ui, context) {
    var currentList = context.cloneArray(ui.modelTerms.value(), []);

    var diff = context.findChanges("currentList", currentList, true, FormatDef.term);

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

    if (context.sortArraysByLength(currentList))
        changed = true;

    if (changed)
        ui.modelTerms.setValue(currentList);
};

module.exports = events;
