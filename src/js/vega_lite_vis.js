
document.addEventListener("DOMContentLoaded", function () {

    const charts = [
        ["#word_cloud", "word_cloud.vg.json"],
        ["#timeline", "timeline.vg.json"],
        ["#age_map", "age_map.vg.json"],
        ["#retiree_map", "retiree_map.vg.json"],
        ["#age_profiles", "age_profiles.vg.json"],
        ["#gender_pyramid", "gender_pyramid.vg.json"],
        ["#state_balances", "state_balances.vg.json"],
        ["#accounts_radial", "accounts_radial.vg.json"],
        ["#benefits_ternary", "benefits_ternary.vg.json"],
        ["#fund_ranking", "fund_ranking.vg.json"]
    ];

    charts.forEach(([element, spec]) => {

        const options = {
            actions: spec === "word_cloud.vg.json" ? false : true,
            mode: spec === "word_cloud.vg.json" ? "vega" : "vega-lite",
            renderer: "svg"
        };

        // Apply tooltip settings only to the ternary chart.
        if (spec === "benefits_ternary.vg.json") {
            options.tooltip = {
                theme: "custom"
            };
        }

        vegaEmbed(element, spec, options)
            .catch(error => {
                console.error(`Error loading ${spec}:`, error);
            });

    });

});
