
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./friedman.options')

var friedmanLayout = LayoutDef.extend({

    label: "Friedman",
    type: "root",
    controls: [
        {
            type: "supplier",
            persistentItems: true,
            useVariables: true,
            stretchFactor: 1,
            controls: [
                {
                    type:"targetlistbox",
                    name: "pairs",
                    label: "Paired Variables",
                    showColumnHeaders: false,
                    columns: [
                        { name: "i1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 },
                        { name: "i2", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            type: "groupbox",
            controls : [
                { name: "paircomps", type:"checkbox", label: "Pairwise comparisons - Durbin-Conover" },
                { name: "desc", type:"checkbox", label: "Descriptives" },
                {
                    name: "plots", type:"checkbox", label: "Descriptives plot",
                    controls: [
                        { name: "plottype_means", optionId: "plottype", type:"radiobutton", checkedValue: "means", label: "Means" },
                        { name: "plottype_medians", optionId: "plottype", type:"radiobutton", checkedValue: "medians", label: "Medians" }
                    ]
                }
            ]
        }
    ],

    actions: [
        {
            onChange : "plots", execute : function(context) {
                var disabled = context.getValue("plots") === false;
                context.set("plottype_means", "disabled", disabled);
                context.set("plottype_medians", "disabled", disabled);
            }
        }
    ]
});

module.exports = { LayoutDef : friedmanLayout, options: options };
