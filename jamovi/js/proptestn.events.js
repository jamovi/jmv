
const events = {
    update: function(ui) {
        this._updatingRatios = 0;
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
    let oldRatios = context.cloneArray(ui.ratio.value(), []);
    let promise = context.requestData("column", { columnName: columnName, properties: ["levels"] })
    promise.then(rData => {
        let data = [];
        if (rData.columnFound) {
            let levels = rData.levels;

            let levelsChanged = levels.length !== oldRatios.length;
            if (levels.length === oldRatios.length) {
                for (let l1 = 0; l1 < levels.length; l1++) {
                    let found = false;
                    for (let l2 = 0; l2 < oldRatios.length; l2++) {
                        if (levels[l1].label === oldRatios[l2].level) {
                            found = true;
                            break;
                        }
                    }
                    if (found === false) {
                        levelsChanged = true;
                        break;
                    }
                }
            }

            let totalRatio = 0;
            if (levelsChanged === false) {
                for (let i = 0; i < oldRatios.length; i++)
                    totalRatio += oldRatios[i].ratio;
            }
            else
                totalRatio = levels.length;


            for (let i = 0; i < levels.length; i++) {
                let ratio = 1;
                if (levelsChanged === false) {
                    for (let r = 0; r < oldRatios.length; r++) {
                        if (levels[i].label === oldRatios[r].level) {
                            ratio = oldRatios[r].ratio;
                            break;
                        }
                    }
                }

                let prop = parseFloat(Math.round((ratio / totalRatio) * 1000) / 1000).toFixed(3);

                data.push({ level: levels[i].label, ratio: ratio, proportion: prop });
            }
        }

        ui.ratio.setValue(data);
        context._updatingRatios -= 1;
    });
};

module.exports = events;
