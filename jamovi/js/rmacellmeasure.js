
var rma_cell = require('./rmacell');

var rma_cellMeasure =  new Format ({

    name: 'rma_cellMeasure',

    default: { measure: null, cell: rma_cell.default },
    children: { measure: FormatDef.variable, cell: rma_cell },

    toString: function(raw) {
        var r = "Measure: " + raw.measure + "; Cell: " + raw.cell[0] + ";";
        for (var i = 1; i < raw.length; i++)
            r = r + ", " + raw[i];
        return r;
    },

    parse: function(value) {
        throw "need to implement";
        return value;
    },

    isEmpty: function(raw) {
        return raw === null;
    },

    getSubFormatInfo: function() {
        return [{ format: FormatDef.variable, key:["measure"] }, { format: rma_cell, key: ["cell"] }];
    },

    isValid: function(raw) {
        if (raw === null)
            return true;

        return rma_cell.isValid(raw.cell) && (raw.measure === null || FormatDef.variable.isValid(raw.measure));
    },

    isEqual: function(raw1, raw2) {
        return rma_cell.isEqual(raw1.cell, raw2.cell) && FormatDef.variable.isEqual(raw1.measure, raw2.measure);
    }
});

module.exports = rma_cellMeasure;
