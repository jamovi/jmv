
var rma_cell = require('./rmacell');
var rma_cellMeasure = require('./rmacellmeasure');


var rmaCellMeasureCtrl = function(params) {
    ListItemControl.extendTo(this, params);

    this.$el.addClass("rma-cell-measure-ctrl");

    this.registerSimpleProperty("format", rma_cellMeasure);

    this.onUpdateView = function(data, format, properties) {
        var imageClasses = 'silky-variable-type-img';
        if (properties["sub"] !== null && properties["sub"]["measure"] !== undefined && properties["sub"]["measure"].type !== undefined)
            imageClasses = imageClasses + ' silky-variable-type-' + properties["sub"]["measure"].type;
        else
            imageClasses = imageClasses + ' silky-variable-type-none';

        this.$el.find(".silky-variable-type-img").removeClass().addClass(imageClasses);
        var $label = this.$el.find(".measure-value");
        $label.empty();
        if (data !== null && data.measure !== null)
            $label.append(FormatDef.variable.toString(data.measure));

        var $cell = this.$el.find(".cell-value");
        $cell.empty();
        if (data !== null && data.cell !== null)
            $cell.append(rma_cell.toString(data.cell));
    };

    this.onRender = function(data, format, properties) {
        var imageClasses = 'silky-variable-type-img';
        if (properties["sub"] !== null && properties["sub"]["measure"] !== undefined && properties["sub"]["measure"].type !== undefined)
            imageClasses = imageClasses + ' silky-variable-type-' + properties["sub"]["measure"].type;
        else
            imageClasses = imageClasses + ' silky-variable-type-none';

        var $item = $('<div class="silky-list-item silky-format-rma_cellMeasure"></div>');
        var $measure = $('<div style="white-space: nowrap;" class="silky-format-variable"></div>');
        $measure.append('<div style="display: inline-block; overflow: hidden;" class="' + imageClasses + '"></div>');

        var displayValue = "";
        if (data.measure !== null)
            displayValue = FormatDef.variable.toString(data.measure);
        $measure.append('<div style="white-space: nowrap;  display: inline-block;" class="silky-list-item-value measure-value">' + displayValue + '</div>');
        $item.append($measure);

        var cellValue = "";
        if (data.cell !== null)
            cellValue = rma_cell.toString(data.cell);
        var $cell = $('<div style="white-space: nowrap;" class="silky-format-rma-cell cell-value">' + cellValue + '</div>');
        $item.append($cell);

        this.$el.append($item);
    };
};

module.exports = rmaCellMeasureCtrl;
