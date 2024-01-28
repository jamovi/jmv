
let rma_cell = require('./rmacell');

module.exports = {
    view_updated: function(ui) {
        this._factorCells = null;

        ui.rmTerms.setValue(this.rmTerms_init(ui));
        ui.contrasts.setValue(this.contrasts_init(ui));
        this.updateFactorCells(ui);
        this.updateRMModelTerms(ui);
        this.updateModelTerms(ui);
        this.filterModelRMTerms(ui);
        this.filterModelTerms(ui);
        this.updateModelLabels(ui.emMeans, _('Term {0}'));
    },

    rm_changed: function(ui) {
        this.updateFactorCells(ui);
        this.updateRMModelTerms(ui);
    },

    rmCells_changed: function (ui) {
        if (ui.rmCells.value() === null) {  // needs initialization
            let cells = this.rmCells_init(ui);
            ui.rmCells.setPropertyValue("maxItemCount", cells.length);
            ui.rmCells.setValue(cells);
        }
        else
            this.filterCells(ui);
    },

    contrasts_changed: function (ui) {
        if (ui.contrasts.value() === null) // needs initialization
            ui.contrasts.setValue(this.contrasts_init(ui));
    },

    bs_changed: function(ui) {
        this.updateModelTerms(ui);
    },

    cov_changed: function(ui) {
        this.updateModelTerms(ui);
    },

    rmTerms_changed: function (ui) {
        if (ui.rmTerms.value() === null) // needs initialization
            ui.rmTerms.setValue(this.rmTerms_init(ui));
        else
            this.filterModelRMTerms(ui);
    },

    bsTerms_changed: function(ui) {
        this.filterModelTerms(ui);
    },

    emMeansSupplier_updated: function(ui) {
        this.calcMarginalMeansSupplier(ui);
    },

    rmcModelSupplier_updated: function(ui) {
        let factorList = utils.clone(ui.rm.value(), []);
        for(let i = 0; i < factorList.length; i++)
            factorList[i] = factorList[i].label;
        ui.rmcModelSupplier.setValue(utils.valuesToItems(factorList, FormatDef.variable));
    },

    bscModelSupplier_updated: function(ui) {
        let variableList = utils.clone(ui.bs.value(), []);
        let covariatesList = utils.clone(ui.cov.value(), []);
        let combinedList = variableList.concat(covariatesList);
        ui.bscModelSupplier.setValue(utils.valuesToItems(combinedList, FormatDef.variable));
    },

    postHocSupplier_updated: function(ui) {
        this.updatePostHocSupplier(ui);
    },

    emMeansSupplier_changed: function(ui) {
        let values = utils.itemsToValues(ui.emMeansSupplier.value());
        utils.checkValue(ui.emMeans, 2, values, FormatDef.variable);
    },

    postHocSupplier_changed: function(ui) {
        let values = utils.itemsToValues(ui.postHocSupplier.value());
        utils.checkValue(ui.postHoc, true, values, FormatDef.term);
    },

    emMeans_listItemAdded: function (ui) {
        this.onEvent_emMeans_listItemsChanged(ui);
    },

    emMeans_listItemRemoved: function (ui) {
        this.onEvent_emMeans_listItemsChanged(ui);
    },

    onEvent_emMeans_listItemsChanged: function(ui) {
        this.updateModelLabels(ui.emMeans, _('Term {0}'));
    },

    rmTerms_init: function (ui) {
        let factorList = utils.clone(ui.rm.value(), []);
        for (let i = 0; i < factorList.length; i++)
            factorList[i] = factorList[i].label;

        return utils.getCombinations(factorList, []);
    },

    rmCells_init: function (ui) {
        if (this._factorCells === null)
            return [];

        let cells = [];
        let factorCells = utils.clone(this._factorCells);
        for (let factorCell of factorCells)
            cells.push({ measure: null, cell: factorCell });

        return cells;
    },

    contrasts_init: function (ui) {
        let variableList = utils.clone(ui.bs.value(), []);
        let factorList = utils.clone(ui.rm.value(), []);
        for (let i = 0; i < factorList.length; i++)
            factorList[i] = factorList[i].label;

        let combinedList = factorList.concat(variableList);

        let currentList = utils.clone(ui.contrasts.value(), []);

        let list3 = [];
        for (let i = 0; i < combinedList.length; i++) {
            let found = null;
            for (let j = 0; j < currentList.length; j++) {
                if (currentList[j].var === combinedList[i]) {
                    found = currentList[j];
                    break;
                }
            }
            if (found === null)
                list3.push({ var: combinedList[i], type: "none" });
            else
                list3.push(found);
        }

        return list3;
    },

    updateModelLabels: function (list, blockName) {
        list.applyToItems(0, (item, index) => {
            item.controls[0].setPropertyValue("label", blockName.replace('{0}', (index + 1)));
        });
    },

    calcMarginalMeansSupplier: function (ui) {

        let b1 = utils.clone(ui.bs.value(), []);
        let b2 = utils.itemsToValues(ui.rmcModelSupplier.value());

        b1 = utils.valuesToItems(b2.concat(b1), FormatDef.variable);

        if (ui.emMeansSupplier)
            ui.emMeansSupplier.setValue(b1);
    },

    updateFactorCells: function (ui) {
        let value = ui.rm.value();
        if (value === null)
            return;

        let data = []
        let indices = []
        for (let i = 0; i < value.length; i++) {
            indices[i] = 0;
        }

        let end = false;
        let pos = 0;
        while (end === false) {
            let cell = []
            for (let k = 0; k < indices.length; k++) {
                cell.push(value[k].levels[indices[k]])
            }
            data.push(cell);
            pos += 1;

            let r = indices.length - 1;
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

        this._factorCells = data;
        this.filterCells(ui);
    },

    calcRMTerms: function (ui, factorList) {

        let diff = this.findChanges("factorList", factorList, true, FormatDef.term);

        let termsList = ui.rmTerms.value();
        termsList = utils.clone(termsList, []);
        let termsChanged = false;

        for (let i = 0; i < diff.removed.length; i++) {
            for (let j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], diff.removed[i])) {
                    termsList.splice(j, 1);
                    termsChanged = true;
                    j -= 1;
                }
            }
        }

        termsList = utils.getCombinations(diff.added, termsList);
        termsChanged = termsChanged || diff.added.length > 0;

        if (termsChanged)
            ui.rmTerms.setValue(termsList);
    },

    updateRMModelTerms: function (ui, variableList, updateEMMeans) {
        if (variableList === undefined)
            variableList = utils.clone(ui.bs.value(), []);

        let factorList = utils.clone(ui.rm.value(), []);

        let customVariables = [];
        for (let i = 0; i < factorList.length; i++) {
            customVariables.push({ name: factorList[i].label, measureType: 'none', dataType: 'none', levels: [] });
            factorList[i] = factorList[i].label;
        }
        this.setCustomVariables(customVariables);

        let combinedList2 = factorList.concat(variableList);

        ui.rmcModelSupplier.setValue(utils.valuesToItems(factorList, FormatDef.variable))

        this.calcRMTerms(ui, factorList);

        if (updateEMMeans === undefined || updateEMMeans)
            this.calcMarginalMeansSupplier(ui);

        this.updateContrasts(ui, combinedList2);
    },

    updateModelTerms: function (ui) {
        let variableList = utils.clone(ui.bs.value(), []);
        let covariatesList = utils.clone(ui.cov.value(), []);

        this.updateRMModelTerms(ui, variableList, false);

        let combinedList = variableList.concat(covariatesList);
        ui.bscModelSupplier.setValue(utils.valuesToItems(combinedList, FormatDef.variable));

        this.calcMarginalMeansSupplier(ui);

        let diff = this.findChanges("variableList", variableList, true, FormatDef.variable);
        let diff2 = this.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
        let combinedDiff = this.findChanges("combinedList", combinedList, true, FormatDef.variable);

        let termsList = utils.clone(ui.bsTerms.value(), []);
        let termsChanged = false;
        for (let i = 0; i < combinedDiff.removed.length; i++) {
            for (let j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], combinedDiff.removed[i])) {
                    termsList.splice(j, 1);
                    termsChanged = true;
                    j -= 1;
                }
            }
        }

        for (let a = 0; a < diff.added.length; a++) {
            let item = diff.added[a];
            let listLength = termsList.length;
            for (let j = 0; j < listLength; j++) {
                let newTerm = utils.clone(termsList[j]);
                if (this.containsCovariate(newTerm, covariatesList) === false) {
                    if (utils.listContains(newTerm, item, FormatDef.variable) === false) {
                        newTerm.push(item)
                        if (utils.listContains(termsList, newTerm, FormatDef.term) === false) {
                            termsList.push(newTerm);
                            termsChanged = true;
                        }
                    }
                }
            }
            if (utils.listContains(termsList, [item], FormatDef.term) === false) {
                termsList.push([item]);
                termsChanged = true;
            }
        }

        for (let a = 0; a < diff2.added.length; a++) {
            let item = diff2.added[a];
            if (utils.listContains(termsList, [item], FormatDef.term) === false) {
                termsList.push([item]);
                termsChanged = true;
            }
        }

        if (termsChanged)
            ui.bsTerms.setValue(termsList);
    },

    updatePostHocSupplier: function (ui) {
        let bsTerms = utils.clone(ui.bsTerms.value(), []);
        let rmTerms = utils.clone(ui.rmTerms.value(), []);
        let combinedTermsList = rmTerms.concat(bsTerms);

        for (let i = 0; i < rmTerms.length; i++) {
            for (let j = 0; j < bsTerms.length; j++)
                combinedTermsList.push(rmTerms[i].concat(bsTerms[j]))
        }

        ui.postHocSupplier.setValue(utils.valuesToItems(combinedTermsList, FormatDef.term));
    },

    filterModelTerms: function (ui) {
        let termsList = utils.clone(ui.bsTerms.value(), []);
        let diff = this.findChanges("bsTerms", termsList, true, FormatDef.term);

        let changed = false;
        if (diff.removed.length > 0) {
            let itemsRemoved = false;
            for (let i = 0; i < diff.removed.length; i++) {
                let item = diff.removed[i];
                for (let j = 0; j < termsList.length; j++) {
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

        if (utils.sortArraysByLength(termsList))
            changed = true;

        if (changed)
            ui.bsTerms.setValue(termsList);

        this.updatePostHocSupplier(ui);
    },

    filterModelRMTerms: function (ui) {
        let termsList = utils.clone(ui.rmTerms.value(), []);
        let diff = this.findChanges("rmTerms", termsList, true, FormatDef.term);

        let changed = false;
        if (diff.removed.length > 0) {
            let itemsRemoved = false;
            for (let i = 0; i < diff.removed.length; i++) {
                let item = diff.removed[i];
                for (let j = 0; j < termsList.length; j++) {
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

        if (utils.sortArraysByLength(termsList))
            changed = true;

        if (changed)
            ui.rmTerms.setValue(termsList);

        this.updatePostHocSupplier(ui);
    },

    containsCovariate: function (value, covariates) {
        for (let i = 0; i < covariates.length; i++) {
            if (FormatDef.term.contains(value, covariates[i]))
                return true;
        }

        return false;
    },

    updateContrasts: function (ui, variableList) {
        let currentList = utils.clone(ui.contrasts.value(), []);

        let list3 = [];
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

    filterCells: function (ui) {
        if (this._factorCells === null)
            return;

        let cells = utils.clone(ui.rmCells.value(), []);

        let factorCells = utils.clone(this._factorCells);

        let changed = false;
        for (let j = 0; j < factorCells.length; j++) {
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
    }
};