
var rma_cell = require('./rmacell');

const events = {
    update: function(ui) {
        this._factorCells = null;

        this.initializeValue(ui.rmTerms, [["RM Factor 1"]]);

        var value = ui.dispErrBars.value();
        ui.errBarDef_se.setEnabled(value);
        ui.errBarDef_ci.setEnabled(value);

        var value = ui.spherCorrs.value();
        ui.spherCorrNone.setEnabled(value);
        ui.spherCorrGreenGsser.setEnabled(value);
        ui.spherCorrHuyFdt.setEnabled(value);

        ui.ciWidth.setEnabled(ui.dispErrBars.value() && ui.errBarDef.value() === "ci");

        updateFactorCells(ui, this);
        updateModelTerms(ui, this);
        filterModelRMTerms(ui, this);
        filterModelTerms(ui, this);
    },

    onChange_spherCorrs: function(ui) {
        var value = ui.spherCorrs.value();
        ui.spherCorrNone.setEnabled(value);
        ui.spherCorrGreenGsser.setEnabled(value);
        ui.spherCorrHuyFdt.setEnabled(value);
    },

    onChange_dispErrBars: function(ui) {
        var value = ui.dispErrBars.value();
        ui.errBarDef_se.setEnabled(value);
        ui.errBarDef_ci.setEnabled(value);
        ui.ciWidth.setEnabled(value && ui.errBarDef_ci.value());
    },

    onChange_errBarDef_ci: function(ui) {
        ui.ciWidth.setEnabled(ui.dispErrBars.value() && ui.errBarDef_ci.value());
    },

    onChange_rm: function(ui) {
        updateFactorCells(ui, this);
        updateModelTerms(ui, this);
    },

    onChange_rmCells: function(ui) {
        filterCells(ui, this);
    },

    onChange_bs: function(ui) {
        updateModelTerms(ui, this);
    },

    onChange_cov: function(ui) {
        updateModelTerms(ui, this);
    },

    onChange_rmTerms: function(ui) {
        filterModelRMTerms(ui, this);
    },

    onChange_bsTerms: function(ui) {
        filterModelTerms(ui, this);
    },

    onChange_plotsSupplier: function(ui) {
        let values = this.itemsToValues(ui.plotsSupplier.value());
        this.checkValue(ui.descPlotsHAxis, false, values, FormatDef.variable);
        this.checkValue(ui.descPlotsSepLines, false, values, FormatDef.variable);
        this.checkValue(ui.descPlotsSepPlots, false, values, FormatDef.variable);
    },

    onChange_postHocSupplier: function(ui) {
        let values = this.itemsToValues(ui.postHocSupplier.value());
        this.checkValue(ui.postHoc, true, values, FormatDef.term);
    },

    onEvent_rmCells_changing: function(ui, data) {
        if (data.key.length === 1 && data.value === null) {
            let oldValue = this.clone(ui.rmCells.value(data.key));
            oldValue["measure"] = null;
            data.value = oldValue;
        }
    },

    onEvent_bsTerms_preprocess: function(ui, data) {
        if (data.intoSelf === false)
            data.items = this.getItemCombinations(data.items);
    },

    onEvent_rmTerms_preprocess: function(ui, data) {
        if (data.intoSelf === false)
            data.items = this.getItemCombinations(data.items);
    }
};

var updateFactorCells = function(ui, context) {
    var value = ui.rm.value();
    if (value === null)
        return;

    var data = []
    var indices = []
    for (var i = 0; i < value.length; i++) {
        indices[i] = 0;
    }

    var end = false;
    var pos = 0;
    while (end === false) {
        var cell = []
        for (var k = 0; k < indices.length; k++) {
            cell.push(value[k].levels[indices[k]])
        }
        data.push(cell);
        pos += 1;
        var zeroCount = 0;

        var r = indices.length - 1;
        if (r < 0)
            end = true;
        while (r >= 0) {
            indices[r] = (indices[r] + 1) % value[r].levels.length;
            if (indices[r] === 0)
                r -= 1;
            else
                break;

            if (r === -1)
                end = true;
        }
    }

    context._factorCells = data;
    filterCells(ui, context);
};

var calcRMTerms = function(ui, factorList, context) {

    var diff = context.findChanges("factorList", factorList, true, FormatDef.term);

    var termsList = ui.rmTerms.value();
    termsList = context.clone(termsList);
    var termsChanged = false;

    for (var i = 0; i < diff.removed.length; i++) {
        for (var j = 0; j < termsList.length; j++) {
            if (FormatDef.term.contains(termsList[j], diff.removed[i])) {
                termsList.splice(j, 1);
                termsChanged = true;
                j -= 1;
            }
        }
    }

    termsList = context.getCombinations(diff.added, termsList);
    termsChanged = termsChanged || diff.added.length > 0;

    if (termsChanged)
        ui.rmTerms.setValue(termsList);
};

var updateModelTerms = function(ui, context) {
    var variableList = context.cloneArray(ui.bs.value(), []);
    var covariatesList = context.cloneArray(ui.cov.value(), []);

    var factorList = context.cloneArray(ui.rm.value(), []);
    for(let i = 0; i < factorList.length; i++)
        factorList[i] = factorList[i].label;

    var combinedList = variableList.concat(covariatesList);
    var combinedList2 = factorList.concat(variableList);

    ui.rmcModelSupplier.setValue(context.valuesToItems(factorList, FormatDef.variable))
    ui.bscModelSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.plotsSupplier.setValue(context.valuesToItems(combinedList2, FormatDef.variable));

    calcRMTerms(ui, factorList, context);

    var diff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    var diff2 = context.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
    var combinedDiff = context.findChanges("combinedList", combinedList, true, FormatDef.variable);


    var termsList = context.cloneArray(ui.bsTerms.value(), []);
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
            var newTerm = context.clone(termsList[j]);
            if (containsCovariate(newTerm, covariatesList) === false) {
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
        ui.bsTerms.setValue(termsList);

    updateContrasts(ui, combinedList2, context);
};

var updatePostHocSupplier = function(ui, context) {
    var bsTerms = context.cloneArray(ui.bsTerms.value(), []);
    var rmTerms = context.cloneArray(ui.rmTerms.value(), []);
    var combinedTermsList = rmTerms.concat(bsTerms);

    ui.postHocSupplier.setValue(context.valuesToItems(combinedTermsList, FormatDef.term));
};

var filterModelTerms = function(ui, context) {
    var termsList = context.cloneArray(ui.bsTerms.value(), []);
    var diff = context.findChanges("bsTerms", termsList, true, FormatDef.term);

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
        ui.bsTerms.setValue(termsList);

    updatePostHocSupplier(ui, context);
};

var filterModelRMTerms = function(ui, context) {
    var termsList = context.clone(ui.rmTerms.value());
    var diff = context.findChanges("rmTerms", termsList, true, FormatDef.term);

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
        ui.rmTerms.setValue(termsList);

    updatePostHocSupplier(ui, context);
};

var containsCovariate = function(value, covariates) {
    for (var i = 0; i < covariates.length; i++) {
        if (FormatDef.term.contains(value, covariates[i]))
            return true;
    }

    return false;
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

var filterCells = function(ui, context) {
    if (context._factorCells === null)
        return;

    var cells = context.cloneArray(ui.rmCells.value(), []);

    var factorCells = context.clone(context._factorCells);

    var changed = false;
    for (var j = 0; j < factorCells.length; j++) {
        if (j < cells.length && cells[j] !== null) {
            if (rma_cell.isEqual(cells[j].cell, factorCells[j]) === false) {
                cells[j].cell = factorCells[j];
                changed = true;
            }
        }
        else {
            cells[j] = { measure: null, cell: factorCells[j] };
            changed = true;
        }
    }

    if (cells.length > factorCells.length) {
        cells.splice(factorCells.length, cells.length - factorCells.length);
        changed = true;
    }

    if (changed) {
        ui.rmCells.setValue(cells);
        ui.rmCells.setPropertyValue("maxItemCount", cells.length);
    }
};


module.exports = events;
