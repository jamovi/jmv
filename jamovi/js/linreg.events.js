
const events = {

    update: function(ui) {
        updateModelLabels(ui);
    },

    onEvent_test_listItemsChanged: function(ui) {
        updateModelLabels(ui);
    }
};

let updateModelLabels = function(ui) {
    let list = ui.blocks.applyToItems(0, (item, index) => {
        item.controls[0].setPropertyValue("label", "Block " + (index + 1) );
    });
};

module.exports = events;
