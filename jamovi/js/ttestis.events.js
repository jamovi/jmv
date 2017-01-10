
const events = {
    update: function(ui) {
        ui.ciWidth.setEnabled(ui.ci.value());
        ui.bfPrior.setEnabled(ui.bf.value());
    },

    onChange_ci: function(ui) {
        ui.ciWidth.setEnabled(ui.ci.value());
    },

    onChange_bf: function(ui) {
        ui.bfPrior.setEnabled(ui.bf.value());
    }
};

module.exports = events;
