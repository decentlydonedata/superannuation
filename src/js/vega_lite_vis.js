
document.addEventListener("DOMContentLoaded", function () {

  const charts = [
    {
      element: "#account-distribution",
      spec: "specs/account_distribution.vg.json"
    },
    {
      element: "#benefit-payments",
      spec: "specs/benefit_payments.vg.json"
    },
    {
      element: "#fund-membership",
      spec: "specs/fund_membership.vg.json"
    },
    {
      element: "#timeline",
      spec: "specs/timeline.vg.json"
    }
  ];

  charts.forEach(function (chart) {
    vegaEmbed(chart.element, chart.spec, {
      actions: false
    }).catch(function (error) {
      console.error(
        "Could not load chart: " + chart.spec,
        error
      );
    });
  });

});
