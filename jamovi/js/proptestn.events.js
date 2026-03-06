
const events = {
    update: function(ui) {
        this._updatingRatios = 0;
        this._refreshing = true;
        updateRatios(ui, this);
    },

    onChange_var: function(ui) {
        updateRatios(ui, this);
    },

    onChange_ratio: function(ui) {
        updateRatios(ui, this);
    },

    onRemoteDataChanged: function(ui, data) {
        if (data.dataType !== "columns" || data.levelChanged === false)
            return;

        updateRatios(ui, this);
    }
};

const updateRatios = function(ui, context) {

    if (context._updatingRatios > 0)
        return;

    context._updatingRatios += 1;
    let columnName = ui.var.value();
    let oldRatios = context.clone(ui.ratio.value(), []);
    let promise = context.requestData("column", { columnName: columnName, properties: ["levels"] })
    promise.then(rData => {
        let data = [];
        let levelsRemoved = 0;
        if (rData.columnFound) {
            let levels = rData.levels;
            let filteredLevels = levels.filter((level) => {
                return ! (level.treatAsMissing || level.filtered);
            });
            levelsRemoved = levels.length - filteredLevels.length;

            let totalRatio = filteredLevels.length;
            for (let i = 0; i < oldRatios.length; i++)
                totalRatio += oldRatios[i].ratio - 1;

            for (let i = 0; i < filteredLevels.length; i++) {
                const level = filteredLevels[i];
                if (level.treatAsMissing || level.filtered) {
                    levelsRemoved += 1;
                    continue;
                }

                let ratio = 1;
                if (i < oldRatios.length - levelsRemoved)
                    ratio = oldRatios[i].ratio;

                let prop = parseFloat(Math.round((ratio / totalRatio) * 1000) / 1000).toFixed(3);

                data.push({ level: level.label, ratio: ratio, proportion: prop });
            }
        }

        ui.ratio.setValue(data);
        let msg = null;
        if (levelsRemoved > 0)
            msg = n_('Active filters have excluded one level', `Active filters have excluded {n} levels`, levelsRemoved)
        ui.ratio.setPropertyValue('infoText', msg);
        context._updatingRatios -= 1;
    });
};

module.exports = events;
