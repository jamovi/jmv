

var rma_cell = new Format ({

    name: 'rma_cell',

    default: [],

    toString: function(raw) {
        var r = raw[0];
        for (var i = 1; i < raw.length; i++)
            r = r + ", " + raw[i];
        return r;
    },

    parse: function(value) {
        throw "need to implement";
        return value;
    },

    isValid: function(raw) {
        if (raw === null)
            return true;

        if (Array.isArray(raw) === false)
            return false;

        for (var i = 0; i < raw.length; i++) {
            if (typeof(raw[i]) !== 'string')
                return false;
        }

        return true;
    },

    isEmpty: function(raw) {
        return raw === null || raw.length === 0;
    },

    isEqual: function(raw1, raw2) {

        if (raw1.length !== raw2.length)
            return false;

        for (var i = 0; i < raw1.length; i++) {
            if (raw1[i] !== raw2[i])
                return false;
        }

        return true;
    }
});


module.exports = rma_cell;
