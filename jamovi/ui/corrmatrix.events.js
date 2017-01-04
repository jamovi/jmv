
const events = {
    update: function(ui) {
        var value = ui.plots.value();
        ui.plotDens.setEnabled(value);
        ui.plotStats.setEnabled(value);

        var value = ui.ci.value();
        ui.ciWidth.setEnabled(value);
    },

    onChange_plots: function(ui) {
        var value = ui.plots.value();
        ui.plotDens.setEnabled(value);
        ui.plotStats.setEnabled(value);
    },

    onChange_ci: function(ui) {
        var value = ui.ci.value();
        ui.ciWidth.setEnabled(value);
    }
};

module.exports = events;
