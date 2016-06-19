
// This file is an automatically generated template, it will not be subsequently
// overwritten by the compiler, and may be edited

var options = require('./anova.options');

var controls = new ControlManager();

var anovaLayout = LayoutDef.extend({

    label: "ANOVA",
    type: "root",
    items: [
        {
            name: "variableSupplier",
            type: "supplier",
            cell: [0, 0],
            persistentItems: false,
            useVariables: true,
            stretchFactor: 1,
            items: [
                {
                    name: "dependent",
                    type:"targetlistbox",
                    label: "Dependent Variable",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "fixedFactors",
                    type:"targetlistbox",
                    label: "Fixed Factors",
                    showColumnHeaders: false,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                },
                {
                    name: "wlsWeights",
                    type:"targetlistbox",
                    label: "WLS Weights",
                    showColumnHeaders: false,
                    maxItemCount: 1,
                    columns: [
                        { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                    ]
                }
            ]
        },
        {
            name: "group2",
            label: "Model",
            collapsed: true,
            cell: [0, 1],
            stretchFactor: 1,
            items : [
                {
                    name: "modelSupplier",
                    type: "supplier",
                    cell: [0, 0],
                    persistentItems: true,
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    items: [
                        {
                            name: "modelTerms",
                            type:"targetlistbox",
                            label: "Model Terms",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                { name: "sumOfSqu", type:"combobox", label: "Sum of squares", options: [ {label: 'Type I', value:'Type I'}, {label: 'Type II', value:'Type II'}, {label: 'Type III', value:'Type III'}] }
            ]
        },
        {
            name: "group7",
            label: "Assumption Checks",
            collapsed: true,
            cell: [0, 2],
            stretchFactor: 1,
            items : [
                { name: "homoTests", type:"checkbox", label: "Homogeneity tests" },
                { name: "qqPlotRes", type:"checkbox", label: "Q-Q plot of residuals" }
            ]
        },
        {
            name: "group6",
            label: "Contrasts",
            collapsed: true,
            cell: [0, 3],
            stretchFactor: 1,
            items : [
                {
                    name: "contrasts",
                    type:"listbox",
                    label: "Factors",
                    showColumnHeaders: false,
                    columns: [
                        { name: "var", label: "", readOnly: true, formatName: "variable", stretchFactor: 0.5 },
                        { name: "type", label: "", readOnly: false, formatName: "string", stretchFactor: 1, options: ['none', 'deviation', 'simple', 'difference', 'helmert', 'repeated', 'polynomial'] }
                    ]
                }
            ]
        },
        {
            name: "group3",
            label: "Post Hoc Tests",
            collapsed: true,
            cell: [0, 4],
            stretchFactor: 1,
            items : [
                {
                    name: "postHocSupplier",
                    type: "supplier",
                    cell: [0, 0],
                    persistentItems: false,
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    items: [
                        {
                            name: "postHocTests",
                            type:"targetlistbox",
                            label: "",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    name: "group3-2",
                    label: "Correction",
                    cell: [0, 1],
                    items: [
                        { name: "corrTukey", type:"checkbox", label: "Tukey" },
                        { name: "corrScheffe", type:"checkbox", label: "Scheffe" },
                        { name: "corrBonf", type:"checkbox", label: "Bonferroni" },
                        { name: "corrHolm", type:"checkbox", label: "Holm" }
                    ]
                }
            ]
        },
        {
            name: "group5",
            label: "Descriptive Plots",
            collapsed: true,
            cell: [0, 5],
            stretchFactor: 1,
            items : [
                {
                    name: "plotsSupplier",
                    type: "supplier",
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    cell: [0, 0],
                    persistentItems: false,
                    items: [
                        {
                            name: "descPlotsHAxis",
                            type:"targetlistbox",
                            label: "Horizontal axis",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        },
                        {
                            name: "descPlotsSepLines",
                            type:"targetlistbox",
                            label: "Separate lines",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        },
                        {
                            name: "descPlotsSepPlots",
                            type:"targetlistbox",
                            label: "Separate plots",
                            showColumnHeaders: false,
                            maxItemCount: 1,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    name: "group5-2",
                    label: "Display",
                    cell: [0, 1],
                    items: [
                        {
                            name: "groupA",
                            label: { name: "dispErrBars", type:"checkbox", label: "Error bars displaying" },
                            items: [
                                {
                                    name: "groupB",
                                    label: { name: "errBarDef_ci", optionId: "errBarDef", type:"radiobutton", checkedValue: "ci", label: "Confidence interval" },
                                    items:[
                                        { name: "ciWidth", type:"textbox", label: "Interval", suffix: "%", formatName: "number", inputPattern: "[0-9]+" }
                                    ]
                                },
                                { name: "errBarDef_se", optionId: "errBarDef", type:"radiobutton", checkedValue: "se", label: "Standard Error" }
                            ]
                        }
                    ]
                }
            ]
        },
        {
            name: "group4",
            label: "Additional Options",
            collapsed: true,
            cell: [0, 6],
            stretchFactor: 1,
            items : [
                {
                    name: "marginalMeansSupplier",
                    type: "supplier",
                    stretchFactor: 1,
                    fitToGrid: false,
                    dockContentWidth: true,
                    cell: [0, 0],
                    persistentItems: false,
                    items: [
                        {
                            name: "margMeans",
                            type:"targetlistbox",
                            label: "Marginal means",
                            showColumnHeaders: false,
                            columns: [
                                { name: "column1", label: "", readOnly: true, formatName: "variable", stretchFactor: 1 }
                            ]
                        }
                    ]
                },
                {
                    name: "groupC",
                    label: { name: "compMainEff", label: "Compare main effects", type:"checkbox" },
                    cell: [0, 1],
                    items: [
                        { name: "confIntAdj", type:"combobox", label: "Confidence interval adjustment", options: [{ label: "None", value: "None" }, { label: "Bonferroni", value: "Bonferroni" }, { label: "Sidak", value: "Sidak" }] }
                    ]
                },
                {
                    name: "group4-2",
                    label: "Display",
                    cell: [0, 2],
                    items: [
                        { name: "dispDescStats", type:"checkbox", label: "Descriptive statistics" },
                        {
                            name: "groupD",
                            label: { name: "estEffSize", label: "Estimates of effect size", type:"checkbox" },
                            items: [
                                { name: "effSizeN2", type:"checkbox", label: "n2" },
                                { name: "partEffSizeN2", type:"checkbox", label: "partial n2" },
                                { name: "effSizeW2", type:"checkbox", label: "w2" }
                            ]
                        }
                    ]
                }
            ]
        }
    ],

    actions: [
         {
            onChange: "estEffSize", execute: function(context) {
                var value = context.getValue("estEffSize");
                context.set("effSizeN2", "disabled", value === false);
                context.set("partEffSizeN2", "disabled", value === false);
                context.set("effSizeW2", "disabled", value === false);
            }
        },
        {
            onChange: "compMainEff", execute: function(context) {
                var value = context.getValue("compMainEff") ;
                context.set("confIntAdj", "disabled", value === false);
            }
        },
        {
            onChange: "dispErrBars", execute: function(context) {
                var value = context.getValue("dispErrBars");
                context.set("errBarDef_se", "disabled", value === false);
                context.set("errBarDef_ci", "disabled", value === false);
            }
        },
        {
            onChange: ["dispErrBars", "errBarDef"], execute: function(context) {
                var value = context.getValue("dispErrBars") === false || context.getValue("errBarDef") !== "ci";
                context.set("ciWidth", "disabled", value);
            }
        },
        {
            onChange: "fixedFactors", execute: function(context) {
                var variableList = this.clone(context.getValue("fixedFactors"));
                if (variableList === null)
                    variableList = [];


                context.setValue("modelSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));
                context.setValue("plotsSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));
                context.setValue("postHocSupplier", this.convertArrayToSupplierList(variableList, FormatDef.variable));


                var diff = this.findDifferences(context.data.lastVariableList, variableList);
                context.data.lastVariableList = variableList;

                var currentList = this.clone(context.getValue("modelTerms"));
                if (currentList === null)
                    currentList = [];

                for (var i = 0; i < diff.removed.length; i++) {
                    for (var j = 0; j < currentList.length; j++) {
                        if (FormatDef.variable.contains(currentList[j], diff.removed[i])) {
                            currentList.splice(j, 1);
                            j -= 1;
                        }
                    }
                }

                if (currentList === null)
                    currentList = [];

                for (var i = 0; i < diff.added.length; i++) {
                    var listLength = currentList.length;
                    for (var j = 0; j < listLength; j++) {
                        var newVar = currentList[j];
                        if (Array.isArray(newVar))
                            newVar = this.clone(newVar);
                        else
                            newVar = [newVar];
                        newVar.push(diff.added[i])
                        currentList.push(newVar);
                    }
                    currentList.push(diff.added[i]);
                }
                this.sortByLength(currentList)
                context.setValue("modelTerms", currentList);


                var list3 = [];
                for (var i = 0; i < variableList.length; i++)
                    list3.push({ var: variableList[i], type: "none" });

                context.setValue("contrasts", list3);
            }
        },
        {
            onChange: "modelTerms", execute: function(context) {
                var currentList = this.clone(context.getValue("modelTerms"));
                if (currentList === null)
                    currentList = [];
                var diff = this.findDifferences(context.data.lastCurrentList, currentList);
                context.data.lastCurrentList = currentList;

                if (diff.removed.length > 0 && currentList !== null) {
                    var itemsRemoved = false;
                    for (var i = 0; i < diff.removed.length; i++) {
                        var item = diff.removed[i];
                        for (var j = 0; j < currentList.length; j++) {
                            if (FormatDef.variable.contains(currentList[j], item)) {
                                currentList.splice(j, 1);
                                j -= 1;
                                itemsRemoved = true;
                            }
                        }
                    }

                    if (itemsRemoved)
                        context.setValue("modelTerms", currentList);
                }

                var list = this.convertArrayToSupplierList(currentList, FormatDef.variable);
                context.setValue("marginalMeansSupplier", list);
            }
        }
    ],

    sortByLength : function(list) {
        for (var i = 0; i < list.length; i++) {
            var l1 = 1;
            if (Array.isArray(list[i]))
                l1 = list[i].length;

            var l2 = 1;
            if (Array.isArray(list[i+1]))
                l2 = list[i+1].length;

            if (list.length > i + 1 && (l1 > l2)) {
                var temp = list[i+1];
                list[i+1] = list[i];
                list[i] = temp;
                if (i > 0)
                    i = i - 2
            }
        }
    },

    convertArrayToSupplierList: function(array, format) {
        var list = [];
        for (var i = 0; i < array.length; i++) {
            list.push({ value: new FormatDef.constructor(array[i], format) });
        }
        return list;
    },

    clone: function(object) {
        return JSON.parse(JSON.stringify(object));
    },

    findDifferences: function(from, to) {
        var j = 0;

        var obj = { removed: [], added: [] };

        if ((from === null || _.isUndefined(from)) && (to === null || _.isUndefined(to)))
            return obj;
        else if (from === null || _.isUndefined(from)) {
            for (j = 0; j < to.length; j++)
                obj.added.push(to[j]);
        }
        else if (to === null || _.isUndefined(to)) {
            for (j = 0; j < from.length; j++)
                obj.removed.push(from[j]);
        }
        else {
            for (j = 0; j < from.length; j++) {
                if (this.listContains(to, from[j]) === false)
                    obj.removed.push(from[j]);
            }

            for (j = 0; j < to.length; j++) {
                if (this.listContains(from, to[j]) === false)
                    obj.added.push(to[j]);
            }
        }

        return obj;
    },

    listContains: function(list, value) {
        for (var i = 0; i < list.length; i++) {
            if (FormatDef.variable.isEqual(list[i], value))
                return true;
        }

        return false;
    }
});

module.exports = { LayoutDef : anovaLayout, options: options, customControls: controls };
