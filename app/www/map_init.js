function (el, x, data) {
  var map = this;
  var L = map.overlayPrimitives || globalThis.L;
  var studyRing = data.studyBoundary.map(function(c) {return L.latLng(c[1],c[0]);});
  var studyBounds = L.latLngBounds(studyRing);
  function clipStudyArea() {
    // Mapbox owns the shared rounded display clip and updates it with its camera.
    if (map.isMapboxMain) return;
    var points = studyRing.map(function(ll) {var pt=map.latLngToLayerPoint(ll);return pt.x+"px "+pt.y+"px";});
    var clip = "polygon("+points.join(",")+")";
    ["tilePane","overlayPane","heatmapBackground","studyRouteLinePane","studyRouteArrowPane",
     "selectedAttraction","transportHaloPane","transportStopPane","attractionPane","tooltipPane"].forEach(function(name) {
      var pane=map.getPane(name);if(pane)pane.style.clipPath=clip;
    });
  }
  map.on("zoom viewreset resize moveend move render",clipStudyArea);
  map.on("resize",function(){
    map.setMinZoom(Math.max(13.5, map.getBoundsZoom(studyBounds, false)));
  });
  map.whenReady(function(){clipStudyArea();map.setMinZoom(Math.max(13.5,map.getBoundsZoom(studyBounds,false)));});
  map.createPane("selectedAttraction").style.zIndex = "480";
  window.mountHeatmapInterface(map, data.streetHeatMask);
  var resizeFrame;
  var resizeObserver = new ResizeObserver(function () {
    cancelAnimationFrame(resizeFrame);
    resizeFrame = requestAnimationFrame(function () {
      if (el.clientWidth && el.clientHeight) {
        map.invalidateSize({animate: false, pan: true, debounceMoveend: true});
      }
    });
  });
  resizeObserver.observe(el);
  map.on("unload", function () {
    resizeObserver.disconnect();
    cancelAnimationFrame(resizeFrame);
  });
  function updateLabels() {
    var labels = el.querySelectorAll(".offline-street-label");
    for (var i = 0; i < labels.length; i++) {
      labels[i].style.visibility = map.getZoom() < 14.25 ? "hidden" : "visible";
    }
  }
  map.on("zoomend", updateLabels);
  updateLabels();

  var attractionFrame = null;
  function updateAttractions() {
    if (!attractionFrame) return;
    attractionFrame.points.forEach(function(row) {
      var marker = map.layerManager.getLayer("marker", row.id);
      if (marker) {
        if (marker.options.icon.options.iconUrl !== row.icon) {
          marker.setIcon(L.icon({iconUrl: row.icon, iconWidth:36, iconHeight:45,
            iconSize:[36,45], iconAnchor:[18,43]}));
        }
        marker.setTooltipContent(row.tooltip);
      }
      var card = document.querySelector('.attraction-card[data-attraction-id="' + row.id + '"]');
      if (!card) return;
      var img = card.querySelector('.list-marker');
      if (img.getAttribute('src') !== row.icon) img.setAttribute('src', row.icon);
      var status = card.querySelector('.attraction-status');
      var dot = status.querySelector('.legend-dot');
      if (!dot) { dot = document.createElement('i'); status.prepend(dot); }
      dot.className = 'legend-dot ' + row.color;
      dot.hidden = !row.hasData;
      status.querySelector('span:not(.count-value)').textContent = row.status;
      status.querySelector('.count-value').textContent = row.count;
    });
  }
  Shiny.addCustomMessageHandler('attraction-frame', function(frame) {
    attractionFrame = frame;
    requestAnimationFrame(updateAttractions);
  });
  // Initial Leaflet proxy markers can arrive after the first frame.
  map.on('layeradd', function(e) {
    if (e.layer instanceof L.Marker && !(map.isMapboxMain && e.layer.options.pane === "studyRouteArrowPane")) updateAttractions();
  });

  var transport = data.transport;
  var routes = Object.create(null), stops = Object.create(null), markers = Object.create(null);
  var activeModes = ["tram", "bus", "rail"], selectedRoute = "", previewRoute = "";
  var previewPatternId = "", previewStopId = "", selectedPatternId = "", selectedStopId = "";
  var scopedStopIds = [];
  var inspectedStopId = "", hoveredStop = "", closeTimer = null, badgeTimer = null;
  var stopInspector = document.getElementById("stop_inspector");
  var routeControlsHome = document.getElementById("transport_route_controls_home");
  var routeControls = document.getElementById("transport_route_controls");
  var hoverPopup = document.createElement("div");
  hoverPopup.className = "transport-hover-popup";
  hoverPopup.setAttribute("role", "region");
  hoverPopup.setAttribute("aria-label", "Routes at hovered stop");
  hoverPopup.hidden = true;
  el.appendChild(hoverPopup);
  var inspectorTabs = document.querySelector('.attractions-panel .nav-pills');
  if (inspectorTabs) inspectorTabs.after(stopInspector);
  var modeNames = {tram: "Tram", bus: "Bus", rail: "Rail"};
  var colors = {tram: "#2577b7", bus: "#d07824", rail: "#7952a3"};
  map.createPane("transportHaloPane").style.zIndex = 440;
  map.createPane("transportStopPane").style.zIndex = 510;
  var stopLayer = L.layerGroup().addTo(map), haloLayer = L.layerGroup().addTo(map);
  var studyRoutes = window.createStudyRouteLayer(map);
  map.on("unload", studyRoutes.dispose);
  var routeContext = document.createElement("details");
  routeContext.className = "route-context";
  routeContext.open = true;
  routeContext.hidden = true;
  var routeContextSummary = document.createElement("summary");
  routeContextSummary.appendChild(textElement("span", "", "Route details"));
  routeContextSummary.appendChild(textElement("span", "panel-toggle route-context-show", "Show"));
  routeContextSummary.appendChild(textElement("span", "panel-toggle route-context-hide", "Hide"));
  var routeContextBody = document.createElement("div");
  routeContextBody.className = "route-context-body";
  routeContextBody.setAttribute("role", "status");
  routeContext.appendChild(routeContextSummary);
  routeContext.appendChild(routeContextBody);
  el.appendChild(routeContext);
  function textElement(tag, className, text) {
    var element = document.createElement(tag);
    if (className) element.className = className;
    element.textContent = text;
    return element;
  }
  transport.routes.forEach(function (route) { routes[route.id] = route; });
  transport.stops.forEach(function (stop) { stops[stop.id] = stop; });

  function stopIcon(stop) {
    return L.icon({iconUrl: "transport/" + stop.mode + ".svg" +
      (transport.transportIconVersion ? "?v=" + transport.transportIconVersion : ""),
      iconSize: [19, 19], iconAnchor: [9.5, 9.5], className: "transport-stop transport-" + stop.mode});
  }
  function decorateStop(stop) {
    var icon = markers[stop.id].getElement();
    if (!icon) return;
    icon.dataset.stopId = stop.id;
    if (!icon.dataset.transportEventsBound) {
      icon.addEventListener("focus", function () { showHoverStop(stop); });
      icon.addEventListener("blur", closeHoverSoon);
      icon.addEventListener("keydown", function (event) {
        if (event.key === "Escape") closeHover();
      });
      icon.dataset.transportEventsBound = "true";
    }
  }
  function restoreRouteControls() {
    if (routeControls && routeControlsHome && routeControls.parentNode !== routeControlsHome)
      routeControlsHome.appendChild(routeControls);
  }
  function closeHover() {
    window.clearTimeout(closeTimer);
    var previous = hoveredStop;
    hoveredStop = "";
    hoverPopup.hidden = true;
    hoverPopup.replaceChildren();
    delete hoverPopup.dataset.stopId;
    if (previewStopId && previewStopId === previous) clearPreview();
  }
  function closeHoverSoon() {
    window.clearTimeout(closeTimer);
    closeTimer = window.setTimeout(function () {
      if (hoverPopup.contains(document.activeElement)) return;
      closeHover();
    }, 220);
  }
  function closeStop() {
    var previous = inspectedStopId;
    restoreRouteControls();
    stopInspector.hidden = true;
    stopInspector.replaceChildren();
    delete stopInspector.dataset.stopId;
    inspectedStopId = "";
    if (previewStopId && previewStopId === previous) clearPreview();
  }

  // The pinned route and hover preview retain independent station contexts.
  function applySelection() {
    function entryFor(id, stopId, preferred, role) {
      var route = routes[id];
      if (!route || activeModes.indexOf(route.mode) < 0) return null;
      var pattern = window.selectStudyRoutePattern(route, stopId, preferred);
      return {route: route, pattern: pattern, stopId: stopId, role: role,
        members: pattern ? pattern.stopIds : route.stopIds};
    }
    var pinned = entryFor(selectedRoute, selectedStopId, selectedPatternId, "pinned");
    var preview = entryFor(previewRoute, previewStopId, previewPatternId, "preview");
    if (pinned && preview && pinned.route.id === preview.route.id &&
        (pinned.pattern ? pinned.pattern.id : "") === (preview.pattern ? preview.pattern.id : "")) preview = null;
    var entries = [pinned, preview].filter(Boolean);
    var focus = preview || pinned;
    haloLayer.clearLayers();
    entries.forEach(function (entry) { entry.visibleCount = 0; });
    transport.stops.forEach(function (stop) {
      var marker = markers[stop.id];
      var memberEntries = entries.filter(function (entry) { return entry.members.indexOf(stop.id) >= 0; });
      var member = memberEntries.length > 0;
      var visible = activeModes.indexOf(stop.mode) >= 0 && stop.insideStudy &&
        (scopedStopIds.indexOf(stop.id) >= 0 || member);
      if (visible && !stopLayer.hasLayer(marker)) stopLayer.addLayer(marker);
      if (!visible && stopLayer.hasLayer(marker)) stopLayer.removeLayer(marker);
      marker.setOpacity(entries.length && !member ? 0.18 : 1);
      var element = marker.getElement();
      if (element) {
        element.classList.toggle("route-member", member);
        element.classList.toggle("route-muted", !!entries.length && !member);
        element.classList.toggle("route-pinned-member", !!pinned && pinned.members.indexOf(stop.id) >= 0);
        element.classList.toggle("route-preview-member", !!preview && preview.members.indexOf(stop.id) >= 0);
      }
      if (member && visible) {
        memberEntries.forEach(function (entry) { entry.visibleCount++; });
        var color = colors[stop.mode];
        L.circleMarker([stop.lat, stop.lng], {radius: 13, color: color, weight: 2,
          fillColor: color, fillOpacity: 0.19, interactive: false,
          pane: "transportHaloPane"}).addTo(haloLayer);
      }
    });
    studyRoutes.renderMany(entries.map(function (entry) {
      return {pattern: entry.pattern, color: colors[entry.route.mode], role: entry.role};
    }));
    routeContextBody.replaceChildren();
    routeContext.hidden = !focus;
    ["routeId", "highlightedStops", "patternId", "directionDestination", "shapePointCount",
      "pinnedRouteId", "previewRouteId", "routeIds"].forEach(function (key) { delete routeContext.dataset[key]; });
    if (!focus) return;
    var direction = window.studyRouteDirection(focus.pattern, focus.stopId, stops);
    routeContext.dataset.routeId = focus.route.id;
    routeContext.dataset.highlightedStops = String(focus.visibleCount);
    routeContext.dataset.patternId = focus.pattern ? focus.pattern.id : "";
    routeContext.dataset.directionDestination = direction.towards;
    routeContext.dataset.shapePointCount = focus.pattern ? String(focus.pattern.paths.reduce(function (n, path) { return n + path.length; }, 0)) : "0";
    routeContext.dataset.pinnedRouteId = pinned ? pinned.route.id : "";
    routeContext.dataset.previewRouteId = preview ? preview.route.id : "";
    routeContext.dataset.routeIds = JSON.stringify(entries.map(function (entry) { return entry.route.id; }));
    entries.forEach(function (entry) {
      var route = entry.route, direction = window.studyRouteDirection(entry.pattern, entry.stopId, stops);
      var section = document.createElement("div");
      section.className = "route-context-entry";
      section.dataset.role = entry.role;
      section.appendChild(textElement("strong", "", (entry.role === "pinned" ? "Pinned · " : "Preview · ") +
        modeNames[route.mode] + " " + route.badge + " · Direction " + route.direction));
      section.appendChild(textElement("span", "route-direction", direction.towards ? "Towards " + direction.towards : route.name));
      section.appendChild(textElement("span", "route-count", "Showing " + entry.visibleCount + " study-area stops"));
      if (direction.next) section.appendChild(textElement("span", "route-name", "Next in map: " + direction.next));
      routeContextBody.appendChild(section);
    });
    routeContextBody.appendChild(textElement("small", "", "Solid: pinned route · Dashed: preview · Arrows: travel direction"));
  }
  function clearPreview() {
    window.clearTimeout(badgeTimer);
    previewRoute = "";
    previewPatternId = "";
    previewStopId = "";
    applySelection();
  }
  function stopContent(stop, onClose, closeLabel) {
    var content = document.createElement("section");
    content.className = "transport-flyout";
    content.dataset.stopId = stop.id;
    content.appendChild(textElement("small", "stop-mode", modeNames[stop.mode]));
    content.appendChild(textElement("strong", "stop-name", stop.name));
    if (stop.routeIds.length) content.appendChild(textElement("p", "stop-hint", "Routes"));
    var badges = document.createElement("div");
    badges.className = "route-badges";
    stop.routeIds.forEach(function (id) {
      var route = routes[id];
      if (!route) return;
      var button = document.createElement("button");
      button.type = "button";
      button.className = "route-badge route-" + route.mode;
      button.dataset.routeId = route.id;
      var badgePattern = window.selectStudyRoutePattern(route, stop.id, "");
      var badgeDirection = window.studyRouteDirection(badgePattern, stop.id, stops);
      var badgeTitle = modeNames[route.mode] + " " + route.badge + " · Direction " + route.direction +
        (badgeDirection.towards ? " · Towards " + badgeDirection.towards : "");
      button.title = badgeTitle;
      button.setAttribute("aria-label", badgeTitle);
      var icon = document.createElement("img");
      icon.src = "transport/" + route.mode + ".svg";
      icon.alt = "";
      button.appendChild(icon);
      button.appendChild(textElement("span", "", route.badge + " · " + route.direction));
      function preview() {
        window.clearTimeout(badgeTimer);
        previewRoute = route.id;
        previewStopId = stop.id;
        var candidate = window.selectStudyRoutePattern(route, stop.id, "");
        previewPatternId = candidate ? candidate.id : "";
        applySelection();
      }
      function leaveBadge() {
        badgeTimer = window.setTimeout(clearPreview, 100);
      }
      button.addEventListener("mouseenter", preview);
      button.addEventListener("focus", preview);
      button.addEventListener("mouseleave", leaveBadge);
      button.addEventListener("blur", leaveBadge);
      // Only an explicit route click changes the pin; hover remains a preview.
      button.addEventListener("click", function () {
        var candidate = window.selectStudyRoutePattern(route, stop.id, "");
        selectedRoute = route.id;
        selectedPatternId = candidate ? candidate.id : "";
        selectedStopId = stop.id;
        clearPreview();
        Shiny.setInputValue("route_from_badge", route.id, {priority: "event"});
      });
      badges.appendChild(button);
    });
    if (stop.routeIds.length) content.appendChild(badges);
    content.addEventListener("keydown", function (event) {
      if (event.key === "Escape") {
        event.preventDefault();
        event.stopPropagation();
        onClose();
      }
    });
    var close = textElement("button", "stop-inspector-close", "×");
    close.type = "button";
    close.setAttribute("aria-label", closeLabel);
    close.addEventListener("click", onClose);
    content.appendChild(close);
    return content;
  }
  function positionHover() {
    if (hoverPopup.hidden || !stops[hoveredStop]) return;
    var stop = stops[hoveredStop];
    var point = map.latLngToContainerPoint([stop.lat, stop.lng]);
    var width = hoverPopup.offsetWidth, height = hoverPopup.offsetHeight;
    var left = point.x + 18;
    if (left + width > el.clientWidth - 8) left = point.x - width - 18;
    hoverPopup.style.left = Math.max(8, Math.min(el.clientWidth - width - 8, left)) + "px";
    hoverPopup.style.top = Math.max(8, Math.min(el.clientHeight - height - 8, point.y - height / 2)) + "px";
  }
  function showHoverStop(stop) {
    window.clearTimeout(closeTimer);
    if (inspectedStopId === stop.id) { closeHover(); return; }
    if (hoveredStop === stop.id && !hoverPopup.hidden) return;
    closeHover();
    hoveredStop = stop.id;
    hoverPopup.dataset.stopId = stop.id;
    hoverPopup.replaceChildren(stopContent(stop, closeHover, "Close hovered stop information"));
    hoverPopup.hidden = false;
    positionHover();
  }
  function openStop(stop) {
    closeHover();
    if (inspectedStopId === stop.id && !stopInspector.hidden) return;
    closeStop();
    inspectedStopId = stop.id;
    stopInspector.dataset.stopId = stop.id;
    stopInspector.replaceChildren(stopContent(stop, closeStop, "Close selected stop information"));
    if (routeControls) stopInspector.appendChild(routeControls);
    stopInspector.hidden = false;
    var panel = document.getElementById("explore_panel");
    if (panel && panel.hidden) {
      var expand = document.querySelector('.explore-expand');
      if (expand) expand.click();
    }
  }
  hoverPopup.addEventListener("mouseenter", function () { window.clearTimeout(closeTimer); });
  hoverPopup.addEventListener("mouseleave", closeHoverSoon);
  hoverPopup.addEventListener("focusin", function () { window.clearTimeout(closeTimer); });
  hoverPopup.addEventListener("focusout", function (event) {
    if (!hoverPopup.contains(event.relatedTarget)) closeHoverSoon();
  });
  L.DomEvent.disableClickPropagation(hoverPopup);
  ["pointerdown", "mousedown", "dblclick", "touchstart", "wheel"].forEach(function (name) {
    hoverPopup.addEventListener(name, function (event) { event.stopPropagation(); });
  });
  map.on("move zoom resize", positionHover);
  map.on("unload", function () {
    window.clearTimeout(closeTimer);
    window.clearTimeout(badgeTimer);
    restoreRouteControls();
    stopInspector.hidden = true;
    stopInspector.replaceChildren();
    hoverPopup.remove();
  });
  transport.stops.forEach(function (stop) {
    var marker = L.marker([stop.lat, stop.lng], {
      pane: "transportStopPane", keyboard: true, riseOnHover: true,
      icon: stopIcon(stop),
      title: modeNames[stop.mode] + " · " + stop.name,
      alt: modeNames[stop.mode] + " stop " + stop.name
    });
    markers[stop.id] = marker;
    marker.on("mouseover", function () { showHoverStop(stop); });
    marker.on("mouseout", closeHoverSoon);
    marker.on("click", function () { openStop(stop); });
    marker.on("add", function () {
      decorateStop(stop);
    });
  });
  L.DomEvent.disableClickPropagation(routeContext);
  Shiny.addCustomMessageHandler("transport-settings", function (message) {
    activeModes = Array.isArray(message.modes) ? message.modes : [];
    scopedStopIds = Array.isArray(message.stopIds) ? message.stopIds : [];
    var nextRoute = routes[message.route] ? message.route : "";
    // An echo for the same direction must keep the clicked branch/headsign.
    // A new sidebar direction (or Clear route) starts a new selection.
    if (nextRoute !== selectedRoute || !nextRoute) {
      selectedPatternId = "";
      selectedStopId = "";
    }
    selectedRoute = nextRoute;
    clearPreview();
    if (inspectedStopId && !stopLayer.hasLayer(markers[inspectedStopId])) closeStop();
    if (hoveredStop && !stopLayer.hasLayer(markers[hoveredStop])) closeHover();
  });
  applySelection();
  clipStudyArea();
    Shiny.setInputValue("offline_map_ready", Date.now(), {priority: "event"});
}
