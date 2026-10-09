
var vg_1 = "superannuation.vg.json";

vegaEmbed("#bar_chart", vg_1)
    .then(function(result) {
        // The chart has rendered successfully.
        console.log("Vega-Lite chart loaded successfully.");
    })
    .catch(function(error) {
        console.error("Error loading Vega-Lite chart:", error);
    });