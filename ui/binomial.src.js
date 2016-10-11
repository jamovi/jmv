'use strict';

var options = require("./binomial.options");


var binomialTestLayout = ui.extend({

    label: "Binomial Test",
    type: "root",
    stage: 1,
    controls: [
        {
            type: "supplier",
            persistentItems: false,
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    type:"variabletargetlistbox",
                    name: "vars",
                    showColumnHeaders: false,
                    variableFilter: ["nominal", "ordinal"],
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "layoutbox",
            margin: "large",
            controls : [
                { type:"checkbox", name: "areCounts", label: "Values are counts" },
                { type:"layoutbox", controls: [
                    { type:"textbox", name: "testValue", label: "Test value", format: FormatDef.number, inputPattern: "[0-9]+" },
                ]},
            ]
        },
        {
            type: "label",
            label: "Hypothesis",
            controls : [
                { type:"radiobutton", name: "hypothesis_notequal", optionId: "hypothesis", checkedValue: "notequal", label: "≠ Test value" },
                { type:"radiobutton", name: "hypothesis_greater", optionId: "hypothesis", checkedValue: "greater", label: "> Test value" },
                { type:"radiobutton", name: "hypothesis_less", optionId: "hypothesis", checkedValue: "less", label: "< Test value" }
            ]
        },
        {
            type: "label",
            label: "Additional Statistics",
            controls : [
                {
                    name: "ci", type:"checkbox", label: "Confidence intervals" ,
                    controls: [ { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" } ]
                }
            ]
        }
    ],

    actions : [
        {
            onChange: "ci", execute: function(context) {
                var disabled = context.getValue("ci") === false;
                context.set("ciWidth", "disabled", disabled);
            }
        }
    ]
});

module.exports = { ui : binomialTestLayout, options: options };
