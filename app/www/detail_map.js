function(el, x) {
  var map = typeof this.getMap === "function" ? this.getMap() : this;
  if (!map || typeof map.invalidateSize !== "function") return;
  (map.getPane("detailAttraction") || map.createPane("detailAttraction")).style.zIndex = 610;
  // Dynamic renderUI can mount Leaflet before shiny-bound-output is attached.
  // Register this actual map explicitly so leafletProxy resolves the new instance.
  map.id = el.id;
  window.jQuery(el).data("leaflet-map", map);
  var frame = null, observer = null, readyTimer = null;
  function resize() {
    if (frame !== null) return;
    frame = requestAnimationFrame(function() {
      frame = null;
      if (el.clientWidth && el.clientHeight) map.invalidateSize(false);
    });
  }
  if (typeof ResizeObserver === "function") {
    observer = new ResizeObserver(resize); observer.observe(el);
  }
  map.on("unload",function(){
    if (window.jQuery(el).data("leaflet-map") === map) window.jQuery(el).removeData("leaflet-map");
    if (observer) observer.disconnect();
    if (readyTimer !== null) clearTimeout(readyTimer);
    if (frame !== null) cancelAnimationFrame(frame);
  });
  map.whenReady(function(){
    resize();
    el.dataset.mountToken = String(Date.now()) + "-" + String(Math.random());
    // Let Shiny finish binding the replacement dynamic UI before proxy replay.
    readyTimer = setTimeout(function(){
      readyTimer = null;
      Shiny.setInputValue(el.id.replace(/nearby_map$/, "map_ready"),
        el.dataset.mountToken, {priority:"event"});
    }, 0);
  });
}
