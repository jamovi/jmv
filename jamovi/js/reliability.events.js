
const events = {
    onChange_vars: function(ui) {
        ui.revItemsSupplier.setValue(this.valuesToItems(ui.vars.value(), FormatDef.variable));
    },

    onChange_revItemsSupplier: function(ui) {
        let values = this.itemsToValues(ui.revItemsSupplier.value());
        this.checkValue(ui.revItems, true, values, FormatDef.variable);
    },

    onUpdate_revItemsSupplier: function(ui) {
        ui.revItemsSupplier.setValue(this.valuesToItems(ui.vars.value(), FormatDef.variable));
    }
};

module.exports = events;
