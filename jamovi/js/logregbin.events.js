const events = {

    update: function(ui) {
        updateModelLabels(ui.blocks, _('Block {0}'));
        calcBlocks(ui, this);
        filterBlocks(ui, this);

        calcModelTerms(ui, this);
        updateLevelControls(ui, this);
        updateModelLabels(ui.emMeans, _('Term {0}'));
    },

    onChange_covs: function(ui) {
        calcBlocks(ui, this);
    },

    onChange_dep:  function(ui) {
        calcModelTerms(ui, this);
    },

    onUpdate_modelSupplier: function(ui) {
        let variableList = this.clone(ui.covs.value(), []);
        variableList = variableList.concat(this.clone(ui.factors.value(), []));
        ui.modelSupplier.setValue(this.valuesToItems(variableList, FormatDef.variable));
    },

    onUpdate_emMeansSupplier: function(ui) {
        calcMarginalMeansSupplier(ui, this);
    },

    onChange_emMeansSupplier: function(ui) {
        let values = this.itemsToValues(ui.emMeansSupplier.value());
        this.checkValue(ui.emMeans, 2, values, FormatDef.variable);
    },

    onChange_factors: function(ui) {
        calcBlocks(ui, this);
        calcModelTerms(ui, this);
    },

    onChange_blocks: function(ui) {
        checkForNullBlocks(ui, this);
    },

    onChange_block: function(ui) {
        filterBlocks(ui, this);
        calcMarginalMeansSupplier(ui, this);
    },

    onChange_refLevels: function(ui) {
        updateLevelControls(ui, this);
    },

    onEvent_test_listItemsChanged: function(ui) {
        updateModelLabels(ui.blocks, _('Block {0}'));
        let blocks = this.clone(ui.blocks.value(), []);
        this.workspace["blocks"] = blocks;
    },

    onEvent_emMeans_listItemsChanged: function(ui) {
        updateModelLabels(ui.emMeans, _('Term {0}'));
    }
};

let calcMarginalMeansSupplier = function(ui, context) {

    let blocks = context.clone(ui.blocks.value(), []);
    let variableList = [];
    for (let blockIndex = 0; blockIndex < blocks.length; blockIndex++)  {
        for (let term of blocks[blockIndex])
            variableList = [...new Set([...variableList, ...term])];
    }

    if (ui.emMeansSupplier)
        ui.emMeansSupplier.setValue(context.valuesToItems(variableList, FormatDef.variable));
};

var calcModelTerms = function(ui, context) {
    var variableList = context.clone(ui.factors.value(), []);
    let depVar = ui.dep.value();
    if (depVar)
        variableList.unshift(depVar);

    updateContrasts(ui, variableList, context);
};

var updateLevelControls = function(ui, context) {
    let dlist = ui.refLevels.value();
    let list = ui.refLevels.applyToItems(0, (item, index, column) => {
        if (column === 1)
            item.setPropertyValue('variable', dlist[index].var );
    });
};

var updateContrasts = function(ui, variableList, context) {
    var currentList = context.clone(ui.refLevels.value(), []);

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
            list3.push({ var: variableList[i], ref: null });
        else
            list3.push(found);
    }

    ui.refLevels.setValue(list3);
};


let updateModelLabels = function(list, blockName) {
    list.applyToItems(0, (item, index) => {
        item.controls[0].setPropertyValue("label", blockName.replace('{0}', (index + 1) ));
    });
};


let calcBlocks = function(ui, context) {
    let variableList = context.clone(ui.covs.value(), []);
    variableList = variableList.concat(context.clone(ui.factors.value(), []));

    ui.modelSupplier.setValue(context.valuesToItems(variableList, FormatDef.variable));


    let varsDiff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    let termsList = context.clone(ui.blocks.value(), []);

    var termsChanged = false;
    for (var i = 0; i < varsDiff.removed.length; i++) {
        for (var j = 0; j < termsList.length; j++) {
            if (termsList[j] === null)
                termsList[j] = [];

            for (var k = 0; k < termsList[j].length; k++) {
                if (FormatDef.term.contains(termsList[j][k], varsDiff.removed[i])) {
                    termsList[j].splice(k, 1);
                    termsChanged = true;
                    k -= 1;
                }
            }
        }
    }

    let selectedRows = ui.blocks.getSelectedRowIndices();
    if (selectedRows.length > 0) {
        if (termsList[selectedRows[selectedRows.length - 1]] === null)
            termsList[selectedRows[selectedRows.length - 1]]  = [];
        for (let i = 0; i < varsDiff.added.length; i++)
            termsList[selectedRows[selectedRows.length - 1]].push([varsDiff.added[i]]);
        //termsList[selectedRows[selectedRows.length - 1]] = context.getCombinations(varsDiff.added, termsList[selectedRows[selectedRows.length - 1]]);
        termsChanged = termsChanged || varsDiff.added.length > 0;
    }


    if (termsChanged)
        ui.blocks.setValue(termsList);
};

let inOtherBlock = function(blocks, value, blockIndex) {
    for (let b = 0; b < blocks.length; b++) {
        if (b === blockIndex)
            continue;

        let block = blocks[b];
        for (let i = 0; i < block.length; i++) {
            if (FormatDef.term.isEqual(block[i], value))
                return true;
        }
    }
    return false;
};

let checkForNullBlocks = function(ui, context) {
    let changed = false;
    let blocks = context.clone(ui.blocks.value(), []);
    for (let blockIndex = 0; blockIndex < blocks.length; blockIndex++)  {
        if (blocks[blockIndex] === null) {
            changed = true;
            blocks[blockIndex] = [];
        }
    }

    if (changed)
        ui.blocks.setValue(blocks);
};

let filterBlocks = function(ui, context) {
    let changed = false;
    let blocks = context.clone(ui.blocks.value(), []);

    let prevBlocks = context.workspace["blocks"];

    for (let blockIndex = 0; blockIndex < blocks.length; blockIndex++)  {
        if (blocks[blockIndex] === null) {
            changed = true;
            blocks[blockIndex] = [];
        }

        let termsList = blocks[blockIndex];

        if (prevBlocks !== undefined && prevBlocks.length === blocks.length) {
            //Remove common terms
            let termsDiff = context.findDifferences(prevBlocks[blockIndex], termsList, FormatDef.term);

            if (termsDiff.removed.length > 0 && termsList !== null) {
                let itemsRemoved = false;
                for (let i = 0; i < termsDiff.removed.length; i++) {
                    let item = termsDiff.removed[i];
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
            /////////////////////
        }

        //Sort terms
        if (context.sortArraysByLength(termsList))
            changed = true;
        ////////////
    }

    //remove terms that have been added that clash with existing terms
    for (let blockIndex = 0; blockIndex < blocks.length; blockIndex++)  {
        let termsList = blocks[blockIndex];

        if (prevBlocks !== undefined && prevBlocks.length === blocks.length) {
            let termsDiff = context.findDifferences(prevBlocks[blockIndex], termsList, FormatDef.term);

            if (termsDiff.added.length > 0 && termsList !== null) {
                let itemsRemoved = false;
                for (let i = 0; i < termsDiff.added.length; i++) {
                    let item = termsDiff.added[i];
                    if (inOtherBlock(blocks, item, blockIndex)) {
                        for (let j = 0; j < termsList.length; j++) {
                            if (FormatDef.term.isEqual(termsList[j], item)) {
                                termsList.splice(j, 1);
                                j -= 1;
                                itemsRemoved = true;
                            }
                        }
                    }
                }

                if (itemsRemoved)
                    changed = true;
            }
        }
    }
    /////////////////////////////////////////////////////////////////



    context.workspace["blocks"] = blocks;

    if (changed)
        ui.blocks.setValue(blocks);
};


module.exports = events;
