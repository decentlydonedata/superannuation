
document.addEventListener("DOMContentLoaded", function () {

    const charts = [
        ["#accounts_radial", "accounts_radial.vg.json"],
        ["#age_map", "age_map.vg.json"],
        ["#age_profiles", "age_profiles.vg.json"],
        ["#benefits_ternary", "benefits_ternary.vg.json"],
        ["#fund_ranking", "fund_ranking.vg.json"],
        ["#gender_pyramid", "gender_pyramid.vg.json"],
        ["#retiree_map", "retiree_map.vg.json"],
        ["#state_balaces", "state_balaces.vg.json"],
        ["#timeline", "timeline.vg.json"],
        ["#world_cloud", "world_cloud.vg.json"]
    ];

    charts.forEach(([element, spec]) => {

        // Use Vega mode for the word cloud; Vega-Lite for other charts
        const options = spec === "world_cloud.vg.json"
            ? { actions: false, mode: "vega" }
            : { actions: true, mode: "vega-lite" };

        vegaEmbed(element, spec, options)
            .catch(error => {
                console.error(`Error loading ${spec}:`, error);
            });

    });

});

