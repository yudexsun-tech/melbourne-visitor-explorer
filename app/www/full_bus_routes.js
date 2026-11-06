/* Historical GTFS previews; retain full paths connecting study-area stations. */
(function () {
  "use strict";
  window.selectStudyRoutePattern = function (route, stopId, preferredId) {
    var patterns = route && Array.isArray(route.patterns) ? route.patterns : [];
    if (preferredId) {
      var preferred = patterns.find(function (p) { return p.id === preferredId &&
        (!stopId || p.stopIds.indexOf(stopId) >= 0); });
      if (preferred) return preferred;
    }
    if (stopId) return patterns.find(function (p) { return p.stopIds.indexOf(stopId) >= 0; }) || null;
    return patterns[0] || null;
  };

  window.studyRouteDirection = function (pattern, stopId, stops) {
    if (!pattern) return {towards: "", next: ""};
    var next = [], ids = pattern.localStopIds || [];
    ids.forEach(function (id, index) {
      if (id !== stopId) return;
      for (var j = index + 1; j < ids.length; j++) {
        if (ids[j] !== stopId && stops[ids[j]]) {next.push(stops[ids[j]].name); break;}
      }
    });
    var atStop = pattern.stopHeadsigns && pattern.stopHeadsigns[stopId];
    return {towards: atStop && atStop.length ? atStop.join(" / ") : (pattern.headsign || pattern.destination || ""),
      next: Array.from(new Set(next)).join(" / ")};
  };

  window.createStudyRouteLayer = function (map) {
    var L = map.overlayPrimitives || globalThis.L;
    map.createPane("studyRouteLinePane").style.zIndex = "435";
    map.createPane("studyRouteArrowPane").style.zIndex = "450";
    var lines = L.layerGroup().addTo(map), arrows = L.layerGroup().addTo(map);
    var current = [], renderedKey = "";
    function drawArrows() {
      arrows.clearLayers();
      var remaining = map.isMapboxMain ? 256 : Infinity;
      current.forEach(function (entry) {
        var pattern = entry.pattern, color = entry.color;
        pattern.paths.forEach(function (path) {
          var travelled = 0, nextArrow = entry.role === "preview" ? 110 : 55;
          // Mapbox perspective can send points behind the camera to infinity.
          // Use the same ground-plane clipping as the visible route stroke.
          var segments = map.isMapboxMain ? map.projectVisibleLineSegments(path) :
            path.slice(1).map(function (point, i) {
              return [map.latLngToLayerPoint(path[i]), map.latLngToLayerPoint(point)];
            });
          var previous = null;
          for (var i = 0; i < segments.length && remaining > 0; i++) {
            var a = segments[i][0], b = segments[i][1];
            if (map.isMapboxMain && previous && Math.hypot(a.x-previous.x,a.y-previous.y) > 1) {
              travelled = 0; nextArrow = entry.role === "preview" ? 110 : 55;
            }
            previous = b;
            var dx = b.x - a.x, dy = b.y - a.y, length = Math.sqrt(dx * dx + dy * dy);
            if (!Number.isFinite(length) || !length) continue;
            while (nextArrow <= travelled + length && remaining > 0) {
              var t = (nextArrow - travelled) / length;
              var point = map.layerPointToLatLng(L.point(a.x + t * dx, a.y + t * dy));
              var angle = Math.atan2(dy, dx) * 180 / Math.PI;
              L.marker(point, {pane: "studyRouteArrowPane", interactive: false, keyboard: false,
                routeRole: entry.role, routePatternId: pattern.id,
                icon: L.divIcon({className: "study-route-arrow", iconSize: [16, 16], iconAnchor: [8, 8],
                  html: '<svg width="16" height="16" viewBox="-8 -8 16 16" aria-hidden="true" style="transform:rotate(' + angle + 'deg)"><path d="M -5 -5 L 3 0 L -5 5" fill="none" stroke="white" stroke-width="4" stroke-linecap="round" stroke-linejoin="round"/><path d="M -5 -5 L 3 0 L -5 5" fill="none" stroke="' + color + '" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>'})}).addTo(arrows);
              nextArrow += 145; remaining--;
            }
            travelled += length;
          }
        });
      });
    }
    function renderMany(entries) {
      entries = entries.filter(function (entry) { return !!entry.pattern; });
      var key = entries.map(function (entry) {
        return entry.pattern.id + "|" + entry.color + "|" + entry.role;
      }).join(";");
      if (key === renderedKey) return;
      current = entries; renderedKey = key;
      lines.clearLayers();
      entries.forEach(function (entry) {
        var preview = entry.role === "preview";
        entry.pattern.paths.forEach(function (path) {
          if (path.length < 2) return;
          var shared = {pane: "studyRouteLinePane", interactive: false, smoothFactor: 0,
            routeRole: entry.role, routePatternId: entry.pattern.id,
            dashArray: preview ? "8 7" : null};
          L.polyline(path, Object.assign({}, shared, {color: "#fff", weight: preview ? 5 : 8,
            opacity: .95})).addTo(lines);
          L.polyline(path, Object.assign({}, shared, {color: entry.color, weight: preview ? 3 : 5,
            opacity: 1, className: "study-route-line study-route-" + entry.role})).addTo(lines);
        });
      });
      drawArrows();
    }
    function render(pattern, color) {
      renderMany(pattern ? [{pattern: pattern, color: color, role: "pinned"}] : []);
    }
    // During camera gestures do not reproject old arrow anchors through the
    // horizon. Rebuild once after movement (also covers rotation without zoom).
    function hideMovingArrows() { map.getPane("studyRouteArrowPane").style.visibility = "hidden"; }
    function finishCameraMove() { drawArrows(); map.getPane("studyRouteArrowPane").style.visibility = ""; }
    if (map.isMapboxMain) {
      map.on("movestart", hideMovingArrows);
      map.on("moveend", finishCameraMove);
    } else map.on("zoomend", drawArrows);
    return {render: render, renderMany: renderMany, dispose: function () {
      if (map.isMapboxMain) {
        map.off("movestart", hideMovingArrows);
        map.off("moveend", finishCameraMove);
      } else map.off("zoomend", drawArrows);
      lines.clearLayers(); arrows.clearLayers();
      map.removeLayer(lines); map.removeLayer(arrows);
    }};
  };
})();
