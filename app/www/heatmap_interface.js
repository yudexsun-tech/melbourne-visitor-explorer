/*
 * R keeps hourly means and divides by 60 and the estimated effective width.
 * Restore the original additive Gaussian heat, colour ramp and activity opacity.
 * Kernel weights still use width-adjusted flow. Pixels show relative heat intensity,
 * not a physical cross-section flow rate or a threshold-based crowding grade.
 */
(function () {
  "use strict";
  let current = null;
  let latestFrame = null;

  function createHeatmap(map, streetMask) {
    const L = map.overlayPrimitives || globalThis.L;
    const pane = map.getPane("heatmapBackground") || map.createPane("heatmapBackground");
    pane.style.zIndex = "420";
    pane.style.pointerEvents = "none";
    const canvas = document.createElement("canvas");
    canvas.className = "sensor-heat-canvas";
    canvas.setAttribute("aria-hidden", "true");
    canvas.hidden = true;
    pane.appendChild(canvas);
    const context = canvas.getContext("2d");
    // Keep the numerical field at its original resolution. Only the final display
    // is clipped, at screen resolution, so scaling cannot blur heat into buildings.
    const heatBuffer = document.createElement("canvas");
    const heatContext = heatBuffer.getContext("2d");
    const polygons = [];
    function collectPolygons(geometry) {
      if (!geometry) return;
      if (geometry.type === "Polygon") polygons.push(geometry.coordinates);
      else if (geometry.type === "MultiPolygon") polygons.push(...geometry.coordinates);
      else if (geometry.type === "GeometryCollection") {
        (geometry.geometries || []).forEach(collectPolygons);
      }
    }
    for (const feature of (streetMask && streetMask.features) || []) {
      collectPolygons(feature.geometry);
    }
    let streetPath = null, pathView = null;
    function projectedStreetPath(size) {
      const center = map.getCenter();
      const view = [center.lat, center.lng, map.getZoom(), map.getBearing?.() || 0, map.getPitch?.() || 0, size.x, size.y].join("|");
      if (streetPath && view === pathView) return streetPath;
      const path = new Path2D();
      for (const polygon of polygons) {
        for (const ring of polygon) {
          if (ring.length < 4) continue;
          const points = map.isMapboxMain ? map.projectGroundRing(ring) :
            ring.map(coordinate=>map.latLngToContainerPoint([coordinate[1],coordinate[0]]));
          points.forEach(function (p, index) {
            if (index === 0) path.moveTo(p.x, p.y);
            else path.lineTo(p.x, p.y);
          });
          path.closePath();
        }
      }
      streetPath = path; pathView = view;
      return path;
    }
    const palette = [[0,43,92,210],[.22,0,185,220],[.42,45,195,125],
      [.62,245,218,55],[.82,246,125,30],[1,207,35,35]];
    let latest = null, animation = null, disposed = false, moving = false;
    function queue() {
      if (animation !== null || disposed || moving) return;
      animation = requestAnimationFrame(draw);
    }
    function draw() {
      animation = null;
      if (disposed || !latest || moving) return;
      if (!latest.enabled) {
        canvas.hidden = true;
        context.clearRect(0, 0, canvas.width, canvas.height);
        canvas.dataset.state = "off";
        return;
      }
      // Missing geometry must never silently restore the old full-surface heat.
      if (!polygons.length) {
        canvas.hidden = true;
        canvas.dataset.state = "mask-unavailable";
        return;
      }
      const size = map.getSize(), step = 3;
      const width = Math.max(1, Math.ceil(size.x / step));
      const height = Math.max(1, Math.ceil(size.y / step));
      const dpr = Math.min(2, window.devicePixelRatio || 1);
      canvas.width = Math.max(1, Math.round(size.x * dpr));
      canvas.height = Math.max(1, Math.round(size.y * dpr));
      heatBuffer.width = width; heatBuffer.height = height;
      canvas.style.width = size.x + "px"; canvas.style.height = size.y + "px";
      L.DomUtil.setPosition(canvas, map.containerPointToLayerPoint([0, 0]));
      const field = new Float32Array(width * height);
      const metresPerPixel = 40075016.686 * Math.cos(map.getCenter().lat * Math.PI / 180) /
        (256 * Math.pow(2, map.getZoom()));
      const sigma = Math.max(.5, latest.radiusM / metresPerPixel / step);
      const radius = sigma * 3, divisor = 2 * sigma * sigma;
      let valid = 0, total = 0, totalFlow = 0;
      for (const point of latest.points || []) {
        if (typeof point.value !== "number" || !Number.isFinite(point.value) || point.value < 0 ||
            typeof point.flowPpmm !== "number" || !Number.isFinite(point.flowPpmm) ||
            point.flowPpmm < 0 || point.samples <= 0) continue;
        valid++; total += point.value; totalFlow += point.flowPpmm;
        if (point.flowPpmm <= 0) continue;
        const position = map.latLngToContainerPoint([point.lat, point.lng]);
        const cx = position.x / step, cy = position.y / step;
        const x0 = Math.max(0, Math.floor(cx - radius)), x1 = Math.min(width - 1, Math.ceil(cx + radius));
        const y0 = Math.max(0, Math.floor(cy - radius)), y1 = Math.min(height - 1, Math.ceil(cy + radius));
        for (let y = y0; y <= y1; y++) {
          const dy = (y - cy) * (y - cy);
          for (let x = x0; x <= x1; x++) {
            const distance = (x - cx) * (x - cx) + dy;
            if (distance <= radius * radius) {
              field[y * width + x] += point.flowPpmm * Math.exp(-distance / divisor);
            }
          }
        }
      }
      const pixels = heatContext.createImageData(width, height), rgba = pixels.data;
      for (let i = 0; i < field.length; i++) {
        if (field[i] <= 0) continue;
        const t = 1 - Math.exp(-field[i] / latest.scale);
        let k = 1;
        while (k < palette.length - 1 && t > palette[k][0]) k++;
        const left = palette[k - 1], right = palette[k];
        const f = (t - left[0]) / (right[0] - left[0]);
        for (let channel = 1; channel <= 3; channel++) {
          rgba[i * 4 + channel - 1] = Math.round(left[channel] + f * (right[channel] - left[channel]));
        }
        rgba[i * 4 + 3] = Math.round(255 * Math.min(.68, t * 1.8));
      }
      heatContext.putImageData(pixels, 0, 0);
      context.save();
      context.scale(dpr, dpr);
      // evenodd preserves courtyards, building holes and disjoint street polygons.
      context.clip(projectedStreetPath(size), "evenodd");
      context.drawImage(heatBuffer, 0, 0, size.x, size.y);
      context.restore();
      canvas.hidden = false;
      canvas.dataset.state = "ready";
      canvas.dataset.mask = "street-surfaces-minus-buildings";
      canvas.dataset.maskPolygons = String(polygons.length);
      canvas.dataset.sensorCount = String((latest.points || []).length);
      canvas.dataset.validSensorCount = String(valid);
      canvas.dataset.totalWeight = String(total);
      canvas.dataset.totalFlowPpmm = String(totalFlow);
      canvas.dataset.metric = latest.metric;
      canvas.dataset.rendering = "relative-width-adjusted-gaussian-heat";
      canvas.dataset.scale = String(latest.scale);
      canvas.dataset.hour = String(latest.hour);
      canvas.dataset.month = latest.month;
      canvas.dataset.weather = latest.weather;
      canvas.dataset.dayType = latest.dayType;
      const origin = map.containerPointToLatLng([0, 0]);
      drawnOrigin = origin; drawnZoom = map.getZoom();
      if (map.isMapboxMain) {
        canvas.style.transform = "";
        drawnSize = size;
        drawnCorners = [[0,0],[size.x,0],[size.x,size.y],[0,size.y]].map(p=>map.containerPointToLatLng(p));
        canvas.dataset.bearing = String(map.getBearing());canvas.dataset.pitch = String(map.getPitch());
      }
      canvas.dataset.originLat = String(origin.lat);
      canvas.dataset.originLng = String(origin.lng);
      canvas.dataset.zoom = String(map.getZoom());
    }
    let drawnOrigin = null, drawnZoom = null, drawnCorners = null, drawnSize = null;
    // Reproject the previously drawn ground-plane canvas during interaction.
    // A projective transform handles rotation and tilt without recomputing the
    // Gaussian field every frame. Once movement ends, redraw at full resolution.
    function canvasHomography(points, width, height) {
      const source=[[0,0],[1,0],[1,1],[0,1]], rows=[];
      source.forEach(([x,y],i)=>{
        const u=points[i].x,v=points[i].y;
        rows.push([x,y,1,0,0,0,-u*x,-u*y,u]);
        rows.push([0,0,0,x,y,1,-v*x,-v*y,v]);
      });
      for(let col=0;col<8;col++){
        let pivot=col;
        for(let row=col+1;row<8;row++)if(Math.abs(rows[row][col])>Math.abs(rows[pivot][col]))pivot=row;
        if(Math.abs(rows[pivot][col])<1e-10)return null;
        [rows[col],rows[pivot]]=[rows[pivot],rows[col]];
        const divisor=rows[col][col];for(let j=col;j<=8;j++)rows[col][j]/=divisor;
        for(let row=0;row<8;row++)if(row!==col){
          const factor=rows[row][col];for(let j=col;j<=8;j++)rows[row][j]-=factor*rows[col][j];
        }
      }
      const h=rows.map(row=>row[8]);
      if(!h.every(Number.isFinite))return null;
      return [h[0]/width,h[3]/width,0,h[6]/width,h[1]/height,h[4]/height,0,h[7]/height,0,0,1,0,h[2],h[5],0,1];
    }
    function followCamera() {
      if (!map.isMapboxMain || !drawnOrigin || canvas.hidden || !drawnCorners) return;
      const matrix=canvasHomography(drawnCorners.map(p=>map.latLngToContainerPoint(p)),drawnSize.x,drawnSize.y);
      if(matrix)canvas.style.transform="matrix3d("+matrix.join(",")+")";
      canvas.style.transformOrigin = "0 0";
    }
    if (map.isMapboxMain) map.on("move", followCamera);
    function startMove() { moving = true; }
    function endMove() { moving = false; queue(); }
    // Leaflet moves the existing canvas with its pane while dragging.
    // Reproject the mask and rebuild heat only after the camera settles.
    map.on("movestart zoomstart", startMove);
    map.on("moveend zoomend", endMove);
    map.on("resize viewreset", queue);
    return {
      update(frame) {
        latest = frame;
        queue();
      },
      dispose() {
        disposed = true;
        if (animation !== null) cancelAnimationFrame(animation);
        map.off("movestart zoomstart", startMove);
        map.off("moveend zoomend", endMove);
        map.off("resize viewreset", queue);
        if (map.isMapboxMain) map.off("move", followCamera);
        canvas.remove();
      }
    };
  }

  function register() {
    Shiny.addCustomMessageHandler("heatmap-frame", function (frame) {
      latestFrame = frame;
      if (current) current.layer.update(frame);
    });
  }
  if (window.Shiny) register();
  else document.addEventListener("DOMContentLoaded", register, {once: true});

  window.mountHeatmapInterface = function (map, streetMask) {
    if (current) current.dispose();
    const mounted = {layer: createHeatmap(map, streetMask)};
    mounted.dispose = function () {
      if (current !== mounted) return;
      mounted.layer.dispose();
      map.off("unload", mounted.dispose);
      current = null;
    };
    current = mounted;
    map.on("unload", mounted.dispose);
    if (latestFrame) mounted.layer.update(latestFrame);
  };
})();
