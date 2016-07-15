'use strict';

var options = require("./binomialtest.options");


var binomialTestLayout = LayoutDef.extend({

    label: "Binomial Test",
    type: "root",
    controls: [
        {
            type: "supplier",
            persistentItems: false,
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    type:"targetlistbox",
                    name: "vars",
                    showColumnHeaders: false,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "groupbox",
            stretchFactor: 1,
            controls : [
                { type:"textbox", name: "testValue", label: "Test value", format: FormatDef.number, inputPattern: "[0-9]+" }
            ]
        },
        {
            type: "groupbox",
            label: "Hypothesis",
            stretchFactor: 1,
            level: "2",
            controls : [
                { type:"radiobutton", name: "hypothesis_notequal", optionId: "hypothesis", checkedValue: "notequal", label: "≠ Test value" },
                { type:"radiobutton", name: "hypothesis_greater", optionId: "hypothesis", checkedValue: "greater", label: "> Test value" },
                { type:"radiobutton", name: "hypothesis_less", optionId: "hypothesis", checkedValue: "less", label: "< Test value" }
            ]
        }
    ]
});

module.exports = { LayoutDef : binomialTestLayout, options: options };
