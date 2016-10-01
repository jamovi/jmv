'use strict';

var options = require("./conttable.options");


var layout = LayoutDef.extend({

    label: "Contingency Tables",
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
                    type:"targetlistbox",
                    name: "rows",
                    label: "Rows",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 },
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "cols",
                    label: "Columns",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 },
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "counts",
                    label: "Counts",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 },
                    ]
                },
                {
                    type:"targetlistbox",
                    name: "layers",
                    label: "Layers",
                    showColumnHeaders: false,
                    columns: [
                        { type: "listitem.variablelabel", name: 'column1', label: "", format: FormatDef.variable, stretchFactor: 1 },
                    ]
                },
            ]
        },
        {
            type: "layoutbox",
            margin: "large",
            stretchFactor: 1,
            controls: [
                {
                    type: "collapsebox",
                    label: "Statistics",
                    stretchFactor: 1,
                    collapsed: true,
                    controls: [
                        {
                            type: "label",
                            cell: [0, 0],
                            controls : [
                                { type:"checkbox", name: "phi", label: "X2" },
                                { type:"checkbox", name: "phiCont", label: "X2 continuity correction" },
                                { type:"checkbox", name: "likeRatio", label: "Likelihood ratio" },
                            ]
                        },
                        {
                            type: "label",
                            cell: [1, 0],
                            controls : [
                                {
                                    type:"checkbox", name: "oddsRatio", label: "Log odds ratio (2x2 only)", controls: [
                                        { type:"textbox", name: "ciWidth", label: "Confidence level", suffix: "%", format: FormatDef.number, inputPattern: "[0-9]+" }
                                    ]
                                }
                            ]
                        },
                        {
                            type: "label",
                            label: "Nominal",
                            cell: [0, 1],
                            controls : [
                                { type:"checkbox", name: "contCoef", label: "Contingency coefficient" },
                                { type:"checkbox", name: "phiCra", label: "Phi and Cramer's V" }
                            ]
                        },
                        {
                            type: "label",
                            label: "Ordinal",
                            cell: [1, 1],
                            controls : [
                                { type:"checkbox", name: "gamma", label: "Gamma" },
                                { type:"checkbox", name: "kenTaub", label: "Kendall's tau-b" }
                            ]
                        }
                    ]
                },
                {
                    type: "collapsebox",
                    label: "Cells",
                    stretchFactor: 1,
                    collapsed: true,
                    controls : [
                        {
                            type: "label",
                            label: "Counts",
                            controls : [
                                { type:"checkbox", name: "cntsObs", label: "Observed" },
                                { type:"checkbox", name: "cntsExp", label: "Expected" }
                            ]
                        },
                        {
                            type: "label",
                            label: "Percentages",
                            cell: [1, 0],
                            controls : [
                                { type:"checkbox", name: "percRow", label: "Row" },
                                { type:"checkbox", name: "percCol", label: "Column" },
                                { type:"checkbox", name: "percTtl", label: "Total" }
                            ]
                        }
                    ]
                },
                {
                    type: "collapsebox",
                    label: "Options",
                    stretchFactor: 1,
                    collapsed: true,
                    controls : [
                        {
                            type: "label",
                            label: "Row Order",
                            controls : [
                                { type:"radiobutton", name: "rowOrder_asc", optionId: "rowOrder", checkedValue: "asc", label: "Ascending" },
                                { type:"radiobutton", name: "rowOrder_desc", optionId: "rowOrder", checkedValue: "desc", label: "Descending" }
                            ]
                        },
                        {
                            type: "label",
                            label: "Column Order",
                            cell: [1, 0],
                            controls : [
                                { type:"radiobutton", name: "colOrder_asc", optionId: "colOrder", checkedValue: "asc", label: "Ascending" },
                                { type:"radiobutton", name: "colOrder_desc", optionId: "colOrder", checkedValue: "desc", label: "Descending" }
                            ]
                        }
                    ]
                }
            ]
        }
    ],

    actions: [
        {
            onChange : "oddsRatio", execute : function(context) {
                var disabled = context.getValue("oddsRatio") === false;
                context.set("ciWidth", "disabled", disabled);
            }
        }
    ]
});

module.exports = { LayoutDef : layout, options: options };
