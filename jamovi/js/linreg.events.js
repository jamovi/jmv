const events = {

    update: function(ui) {
        updateModelLabels(ui.blocks, 'Block');
    },

    onEvent_test_listItemsChanged: function(ui) {
        updateModelLabels(ui.blocks, 'Block');
        let blocks = this.cloneArray(ui.blocks.value(), []);
        this.workspace["blocks"] = blocks;
    }
};

let updateModelLabels = function(list, blockName) {
    list.applyToItems(0, (item, index) => {
        item.controls[0].setPropertyValue("label", blockName + " " + (index + 1) );
    });
};

module.exports = events;
