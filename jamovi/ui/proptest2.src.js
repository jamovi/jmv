'use strict';

const options = require("./proptest2.options");

const binomialTestLayout = ui.extend({

    label: "Proportion Test (2 Outcomes)",
    type: "root",
    stage: 0,
    controls: [
        {
            type: "variablesupplier",
            suggested: ["continuous", "nominal", "ordinal"],
            persistentItems: false,
            stretchFactor: 1,
            controls: [
                {
                    type:"variabletargetlistbox",
                    name: "vars",
                    showColumnHeaders: false,
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
    ]
});

var actions = Actions.extend({
    events : [
        {
            onChange: "ci", execute: function(ui) {
                ui.ciWidth.setEnabled(ui.ci.value());
            }
        }
    ]
});

module.exports = { ui : binomialTestLayout, actions: actions, options: options };
