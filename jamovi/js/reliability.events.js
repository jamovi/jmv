
const events = {
    onChange_vars: function(ui) {
        ui.revItemsSupplier.setValue(this.valuesToItems(ui.vars.value(), FormatDef.variable));
    }
};

module.exports = events;
