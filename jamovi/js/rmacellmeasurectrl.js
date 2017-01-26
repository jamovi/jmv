
var rma_cell = require('./rmacell');
var rma_cellMeasure = require('./rmacellmeasure');


var rmaCellMeasureCtrl = function(params) {
    ListItemControl.extendTo(this, params);
    RequestDataSupport.extendTo(this);

    this.$el.addClass("rma-cell-measure-ctrl");

    this.registerSimpleProperty("format", rma_cellMeasure);

    this._override("onDataChanged", (baseFunction, data) => {
        if (baseFunction !== null)
            baseFunction.call(this, data);

        if (data.dataType !== "columns")
            return;

        if (data.dataInfo.measureTypeChanged)
            this.render();
    });

    this.onUpdateView = function(data, format, properties) {

        let promise = this.requestData("column", { columnName: data.measure, properties: [ "measureType" ] });
        promise.then(rData => {
            let measureType = rData.measureType;
            if (measureType === undefined)
                measureType = "none";
            var imageClasses = 'silky-variable-type-img';
            if (measureType !== null && measureType !== undefined)
                imageClasses = imageClasses + ' silky-variable-type-' + measureType;
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
        });
        return promise;
    };

    this.onRender = function(data, format, properties) {

        let promise = this.requestData("column", { columnName: data.measure, properties: [ "measureType" ] });
        promise.then(rData => {
            let measureType = rData.measureType;
            if (measureType === undefined)
                measureType = "none";
            var imageClasses = 'silky-variable-type-img';
            if (measureType !== null && measureType !== undefined)
                imageClasses = imageClasses + ' silky-variable-type-' + measureType;
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
        });
        return promise;
    };
};

module.exports = rmaCellMeasureCtrl;
