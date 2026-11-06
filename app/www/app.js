(function () {
  "use strict";
  document.addEventListener("input", function (event) {
    if (event.target.id === "hour_slider" && window.Shiny) {
      event.target.style.setProperty("--hour-progress", (Number(event.target.value) / 23 * 100) + "%");
      event.target.setAttribute("aria-valuetext", String(event.target.value).padStart(2, "0") + ":00");
      Shiny.setInputValue("manual_hour", Number(event.target.value), { priority: "event" });
    }
  });
  document.addEventListener("click", function (event) {
    var toggle = event.target.closest("[data-panel-toggle]");
    if (toggle) {
      event.preventDefault();
      var name = toggle.dataset.panelToggle;
      var workspace = toggle.closest(".workspace");
      var panel = document.getElementById(name + "_panel");
      var collapsed = !panel.hidden;
      panel.hidden = collapsed;
      workspace.classList.toggle(name + "-collapsed", collapsed);
      workspace.querySelector("." + name + "-expand").hidden = !collapsed;
      workspace.querySelectorAll('[data-panel-toggle="' + name + '"]').forEach(function (button) {
        button.setAttribute("aria-expanded", String(!collapsed));
      });
      workspace.querySelector("." + name + (collapsed ? "-expand" : "-collapse"))
        .focus({preventScroll: true});
      return;
    }
    var pageLink = event.target.closest("[data-page-target]");
    if (pageLink && window.Shiny) {
      Shiny.setInputValue("navigate_page", pageLink.dataset.pageTarget, {priority: "event"});
      return;
    }
    var card = event.target.closest("[data-attraction-id]");
    if (card && window.Shiny) {
      Shiny.setInputValue("selected_from_list", card.dataset.attractionId, { priority: "event" });
    }
  });
  function registerHandlers() {
    Shiny.addCustomMessageHandler("set-map-hour", function (message) {
      var slider = document.getElementById("hour_slider");
      if (slider) {
        slider.value = message.hour;
        slider.setAttribute("aria-valuetext", String(message.hour).padStart(2, "0") + ":00");
        slider.style.setProperty("--hour-progress", (message.hour / 23 * 100) + "%");
      }
    });
    Shiny.addCustomMessageHandler("resize-city-map", function (message) {
      window.setTimeout(function () { window.dispatchEvent(new Event("resize")); }, 200);
    });
  }
  if (window.Shiny) registerHandlers();
  else document.addEventListener("DOMContentLoaded", registerHandlers, { once: true });
})();
