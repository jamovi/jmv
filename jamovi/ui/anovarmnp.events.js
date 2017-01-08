
const events = {
    update: function(ui) {
        var value = ui.plots.value();
        ui.plotType_means.setEnabled(value);
        ui.plotType_medians.setEnabled(value);
    },

    onChange_plots: function(ui) {
        var value = ui.plots.value();
        ui.plotType_means.setEnabled(value);
        ui.plotType_medians.setEnabled(value);
    }
};

module.exports = events;
