
const events = {

    update: function(ui) {
        updateModelLabels(ui, this);
        calcModelTerms(ui, this);
    },

    onEvent_test_listItemsAdded: function(ui, data) {
        updateModelLabels(ui, this);
        calcModelTerms(ui, this);
        setTimeout(() => {
            data.item.controls[0].$input.focus();
        }, 0);
    },

    onEvent_test_listItemsChanged: function(ui) {
        updateModelLabels(ui, this);
        calcModelTerms(ui, this);
    },

    onChange_resCovSupplier: function(ui) {
        let values = this.itemsToValues(ui.resCovSupplier.value());
        this.checkPairsValue(ui.resCov, values);
    },

    onUpdate_resCovSupplier: function(ui) {
        calcModelTerms(ui, this);
    },

    onEvent_factorNameChange : function(ui) {
        updateModelLabels(ui, this);
    }
};

const updateModelLabels = function(ui, context) {
    let list = ui.factors.applyToItems(0, (item, index) => {
        let value = item.controls[0].value();
        if ( ! value || value.trim() === '')
            item.controls[0].setValue(_('Factor {0}').replace('{0}', (index + 1)) );
    });
};

const calcModelTerms = function(ui, context) {

    let factorList = context.cloneArray(ui.factors.value(), []);

    let variables = [];
    for (let i = 0 ; i < factorList.length; i++) {
        let vars = factorList[i].vars;
        if (vars) {
            for (let y = 0; y < vars.length; y++) {
                let variable = vars[y];
                if (variable) {
                    let found = false;
                    for (let j = 0; j < variables.length; j++) {
                        if (variables[j] === variable) {
                            found = true;
                            break;
                        }
                    }
                    if (found == false)
                        variables.push(variable);
                }
            }
        }
    }

    ui.resCovSupplier.setValue(context.valuesToItems(variables, FormatDef.variable));
};

module.exports = events;
