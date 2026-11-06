/* Main-map drawing surface: Mapbox GL camera + DOM/SVG overlays.
 * No second map or Leaflet renderer is created. The narrow drawing interface
 * lets the established transport selection logic and Gaussian field stay shared.
 */
(function () {
  'use strict';
  const poiAliases={
    ATTR001:['State Library Victoria'],
    ATTR002:['Ian Potter Centre: NGV Australia','The Ian Potter Centre: NGV Australia','NGV Australia'],
    ATTR003:['ACMI','Australian Centre for the Moving Image'],
    ATTR004:['Immigration Museum'],
    ATTR005:["Saint Paul's Cathedral","St Paul's Cathedral","St. Paul's Cathedral"],
    ATTR006:['Queen Victoria Market'], ATTR007:['Royal Arcade'],
    ATTR008:['Block Arcade','The Block Arcade'], ATTR009:['Bourke Street Mall'],
    ATTR010:['Chinatown','Melbourne Chinatown'],
    ATTR011:['Flinders Street Railway Station','Flinders Street Station','Flinders Street'],
    ATTR012:['Federation Square','Fed Square'], ATTR016:['Hosier Lane','Hosier Lane Street Art']
  };
  // Display-only positions verified against this Mapbox basemap on 2026-11-01.
  // Seed positions avoid jumps when less prominent labels appear only after zoom.
  // Live same-name POIs below refresh these positions; original data stays intact.
  const verifiedPoiPositions={"ATTR001":{"name":"State Library Victoria","coordinates":[144.96549546718597,-37.80969707067399]},"ATTR002":{"name":"Ian Potter Centre: NGV Australia","coordinates":[144.96996134519577,-37.81755632985903]},"ATTR003":{"name":"ACMI","coordinates":[144.9691043794155,-37.817468397238365]},"ATTR004":{"name":"Immigration Museum","coordinates":[144.96039927005768,-37.81912638624858]},"ATTR005":{"name":"Saint Paul’s Cathedral","coordinates":[144.96761441230774,-37.81679777513157]},"ATTR006":{"name":"Queen Victoria Market","coordinates":[144.95731472969055,-37.807344851483265]},"ATTR009":{"name":"Bourke Street Mall","coordinates":[144.96444135904312,-37.81383764204532]},"ATTR012":{"name":"Federation Square","coordinates":[144.96934980154037,-37.817905940447226]},"ATTR007":{"name":"Royal Arcade","coordinates":[144.964060485363,-37.81457927565115]},"ATTR011":{"name":"Flinders Street","coordinates":[144.96717989444733,-37.81775973985724]},"ATTR008":{"name":"Block Arcade","coordinates":[144.96443331241608,-37.815595301596254]}};
  const normalizedName=name=>String(name||'').normalize('NFKD').toLowerCase().replace(/[’‘]/g,"'").replace(/[^a-z0-9]/g,'');
  // Clip in the ground plane BEFORE perspective division. Projecting a ring
  // that crosses behind a tilted camera makes its edges fold across the screen.
  function clipGroundPolygon(subject, frame) {
    let output=subject.slice();
    const area=frame.reduce((sum,p,i)=>{const q=frame[(i+1)%frame.length];return sum+p.x*q.y-q.x*p.y;},0);
    const sign=area>=0?1:-1;
    for(let i=0;i<frame.length && output.length;i++){
      const a=frame[i],b=frame[(i+1)%frame.length];
      const side=p=>sign*((b.x-a.x)*(p.y-a.y)-(b.y-a.y)*(p.x-a.x));
      const input=output;output=[];
      let previous=input.at(-1),previousSide=side(previous);
      for(const current of input){
        const currentSide=side(current),inside=currentSide>=0,wasInside=previousSide>=0;
        if(inside!==wasInside){
          const t=previousSide/(previousSide-currentSide);
          output.push({x:previous.x+(current.x-previous.x)*t,y:previous.y+(current.y-previous.y)*t});
        }
        if(inside)output.push(current);
        previous=current;previousSide=currentSide;
      }
    }
    return output;
  }
  function clipGroundSegment(start,end,frame){
    const area=frame.reduce((sum,p,i)=>{const q=frame[(i+1)%frame.length];return sum+p.x*q.y-q.x*p.y;},0);
    const sign=area>=0?1:-1;let lo=0,hi=1;
    for(let i=0;i<frame.length;i++){
      const a=frame[i],b=frame[(i+1)%frame.length];
      const side=p=>sign*((b.x-a.x)*(p.y-a.y)-(b.y-a.y)*(p.x-a.x));
      const first=side(start),last=side(end);
      if(first<0&&last<0)return null;
      if((first<0)!==(last<0)){const t=first/(first-last);if(first<0)lo=Math.max(lo,t);else hi=Math.min(hi,t);}
    }
    if(lo>hi)return null;
    const at=t=>({x:start.x+(end.x-start.x)*t,y:start.y+(end.y-start.y)*t});
    return [at(lo),at(hi)];
  }
  // A small screen-space inset only changes the visible edge. It never
  // modifies the authoritative boundary used for data and station membership.
  function insetDisplayRing(points, pixels) {
    const ring=points.slice();
    if(ring.length>1&&Math.hypot(ring[0].x-ring.at(-1).x,ring[0].y-ring.at(-1).y)<.01)ring.pop();
    if(ring.length<3)return ring;
    const area=ring.reduce((sum,p,i)=>{const q=ring[(i+1)%ring.length];return sum+p.x*q.y-q.x*p.y;},0),sign=area>=0?1:-1;
    return ring.map((p,i)=>{
      const before=ring[(i+ring.length-1)%ring.length],after=ring[(i+1)%ring.length];
      const d1={x:p.x-before.x,y:p.y-before.y},d2={x:after.x-p.x,y:after.y-p.y};
      const l1=Math.hypot(d1.x,d1.y),l2=Math.hypot(d2.x,d2.y);
      if(l1<.01||l2<.01)return p;
      const distance=Math.min(pixels,l1*.15,l2*.15);
      const n1={x:-d1.y/l1*sign,y:d1.x/l1*sign},n2={x:-d2.y/l2*sign,y:d2.x/l2*sign};
      const denom=1+n1.x*n2.x+n1.y*n2.y;
      if(denom<.15)return {x:p.x+n1.x*distance,y:p.y+n1.y*distance};
      const dx=(n1.x+n2.x)*distance/denom,dy=(n1.y+n2.y)*distance/denom;
      const scale=Math.min(1,pixels*2/(Math.hypot(dx,dy)||1));
      return {x:p.x+dx*scale,y:p.y+dy*scale};
    });
  }
  window.createMapboxMainSurface = function (el, gl, data) {
    if (el._mainSurface) el._mainSurface.dispose();
    const panes = {}, layers = new Set(), groups = new Set(), events = {}, attractions = {};
    const svgNS = 'http://www.w3.org/2000/svg';
    // Bounded tone curves lift night shadows and soften day highlights without
    // altering semantic overlay colours. Alpha stays unchanged at the FTZ clip.
    const toneSvg=document.createElementNS(svgNS,'svg');
    toneSvg.setAttribute('width','0');toneSvg.setAttribute('height','0');
    toneSvg.setAttribute('aria-hidden','true');toneSvg.style.position='absolute';
    const defs=document.createElementNS(svgNS,'defs');
    const toneFilter=document.createElementNS(svgNS,'filter');
    toneFilter.id='main-map-tone-'+el.id;
    toneFilter.setAttribute('color-interpolation-filters','sRGB');
    const transfer=document.createElementNS(svgNS,'feComponentTransfer');
    const channels=['R','G','B'].map(channel=>{
      const fn=document.createElementNS(svgNS,'feFunc'+channel);fn.setAttribute('type','gamma');
      transfer.appendChild(fn);return fn;
    });
    const alpha=document.createElementNS(svgNS,'feFuncA');alpha.setAttribute('type','identity');transfer.appendChild(alpha);
    toneFilter.appendChild(transfer);defs.appendChild(toneFilter);toneSvg.appendChild(defs);el.appendChild(toneSvg);
    const studyClip=document.createElementNS(svgNS,'clipPath');
    studyClip.id='main-map-study-clip-'+el.id;studyClip.setAttribute('clipPathUnits','userSpaceOnUse');
    const studyClipPath=document.createElementNS(svgNS,'path');studyClip.appendChild(studyClipPath);defs.appendChild(studyClip);
    let studyPathD='';
    // One permanent basemap boundary includes every station-link corridor and
    // the filled eastern interior triangle. Selection only changes overlays.
    const basemapClip=document.createElementNS(svgNS,'clipPath');
    basemapClip.id='main-map-basemap-clip-'+el.id;basemapClip.setAttribute('clipPathUnits','userSpaceOnUse');
    const basemapPolygons=data.transport.basemapDisplayBoundary || [[data.studyBoundary]];
    const basemapPaths=basemapPolygons.map(()=>{
      const node=document.createElementNS(svgNS,'path');
      node.setAttribute('clip-rule','evenodd');node.setAttribute('fill-rule','evenodd');
      basemapClip.appendChild(node);return node;
    });
    defs.appendChild(basemapClip);
    function basemapPolygonPath(polygon){
      return polygon.map(ring=>roundedStudyPath(projectGroundRing(ring))).join(' ');
    }
    // Display-only corner rounding (8 CSS px maximum); authoritative coordinates
    // are untouched. The same path clips basemap, weather and area overlays.
    function roundedStudyPath(points){
      const vertices=points.slice();
      if(vertices.length>1 && Math.hypot(vertices[0].x-vertices.at(-1).x,vertices[0].y-vertices.at(-1).y)<.01)vertices.pop();
      if(vertices.length<3)return 'M0,0 Z';
      const corners=vertices.map((p,i)=>{
        const before=vertices[(i+vertices.length-1)%vertices.length],after=vertices[(i+1)%vertices.length];
        const incoming=Math.hypot(before.x-p.x,before.y-p.y),outgoing=Math.hypot(after.x-p.x,after.y-p.y);
        const radius=Math.min(8,incoming*.22,outgoing*.22);
        return {p,a:{x:p.x+(before.x-p.x)*radius/(incoming||1),y:p.y+(before.y-p.y)*radius/(incoming||1)},
          b:{x:p.x+(after.x-p.x)*radius/(outgoing||1),y:p.y+(after.y-p.y)*radius/(outgoing||1)}};
      });
      return corners.map((c,i)=>(i?'L':'M')+c.a.x+','+c.a.y+' Q'+c.p.x+','+c.p.y+' '+c.b.x+','+c.b.y).join(' ')+' Z';
    }
    const toneCurves={day:[.94,1,.02],dawn:[.94,.90,.02],dusk:[.92,.80,.04],night:[.90,.65,.06]};
    let activeLightPreset='night',wetWeather=false;
    function applyBasemapTone(){
      const night=activeLightPreset==='night';
      const weatherTone=wetWeather?(night?' saturate(0.82) brightness(0.94) contrast(1.03)':' saturate(0.72) brightness(0.87) contrast(1.04)'):'';
      gl.getCanvas().style.filter='url(#'+toneFilter.id+')'+weatherTone;
      el.dataset.weatherVisual=wetWeather?'overcast-rain':'clear';
    }
    Shiny.addCustomMessageHandler('main-map-lighting',frame=>{
      const curve=toneCurves[frame.preset];if(!curve)return;
      channels.forEach(fn=>{fn.setAttribute('amplitude',curve[0]);fn.setAttribute('exponent',curve[1]);fn.setAttribute('offset',curve[2]);});
      activeLightPreset=frame.preset;applyBasemapTone();
      el.dataset.lightPreset=frame.preset;el.dataset.lightMonth=frame.month;el.dataset.lightHour=frame.hour;
      el.dataset.sunrise=frame.sunrise;el.dataset.sunset=frame.sunset;
      el.dataset.solarDate=frame.representativeDate;
    });

    // Serialize user intents until the server acknowledges each one. Shiny can
    // coalesce rapid inputs while rendering; a later weather/day edit must not
    // discard the preceding month edit. This preserves the existing R contract.
    const filterPanel=document.getElementById('time_panel');
    const previousFilterHandler=filterPanel && filterPanel.onchange;
    const filterQueue=[];let filterSequence=Date.now(),filterInFlight=null;
    function sendNextFilter(){
      if(filterInFlight || !filterQueue.length || disposed)return;
      filterInFlight={...filterQueue.shift(),mainMapSequence:++filterSequence};
      Shiny.setInputValue('map_filter_intent',filterInFlight,{priority:'event'});
    }
    function submitFilterIntent(event) {
      if(!event.isTrusted)return;
      const target=event.target;
      const field=target.id==='selected_month'?'month':target.name==='weather_filter'?'weather':target.name==='day_type'?'day_type':null;
      if(field){filterQueue.push({field,value:target.value});sendNextFilter();}
    }
    Shiny.addCustomMessageHandler('main-map-filter-ack',message=>{
      if(filterInFlight && message.sequence===filterInFlight.mainMapSequence){filterInFlight=null;sendNextFilter();}
    });
    if(filterPanel){filterPanel.onchange=null;filterPanel.addEventListener('change',submitFilterIntent,true);}
    let disposed = false;
    const ll = p => Array.isArray(p) ? {lat:p[0],lng:p[1]} : p;
    const project = p => {p=ll(p); return gl.project([p.lng,p.lat]);};
    function emit(name, event) {(events[name] || []).slice().forEach(fn=>fn(event || {}));}
    function pane(name) {
      if (panes[name]) return panes[name];
      const node = document.createElement('div');
      node.className = 'main-map-pane'; node.dataset.pane = name;
      Object.assign(node.style, {position:'absolute',inset:'0',pointerEvents:'none',zIndex:'400'});
      el.appendChild(node); panes[name] = node;
      clip(); return node;
    }
    const groundOrigin=mapboxgl.MercatorCoordinate.fromLngLat(data.studyBoundary[0]);
    const groundCache=new WeakMap();let groundFrameKey='',groundFrame=[];
    function localGround(point){
      const m=mapboxgl.MercatorCoordinate.fromLngLat(point);
      return {x:(m.x-groundOrigin.x)*1e6,y:(m.y-groundOrigin.y)*1e6};
    }
    function screenGround(point){
      return gl.project(new mapboxgl.MercatorCoordinate(point.x/1e6+groundOrigin.x,point.y/1e6+groundOrigin.y).toLngLat());
    }
    function visibleGroundFrame(){
      const c=gl.getCenter(),w=el.clientWidth,h=el.clientHeight;
      const key=[c.lng,c.lat,gl.getZoom(),gl.getBearing(),gl.getPitch(),w,h].join('|');
      if(key!==groundFrameKey){
        // Overscan keeps artificial viewport corners and full icon silhouettes
        // outside the visible map. At maxPitch=60 all these rays hit the ground.
        const pad=Math.min(64,h*.15);
        groundFrame=w&&h?[[-pad,-pad],[w+pad,-pad],[w+pad,h+pad],[-pad,h+pad]].map(p=>localGround(gl.unproject(p))):[];
        groundFrameKey=key;
      }
      return groundFrame;
    }
    function projectGroundRing(ring){
      if(!groundCache.has(ring))groundCache.set(ring,ring.map(localGround));
      const frame=visibleGroundFrame();
      return frame.length?clipGroundPolygon(groundCache.get(ring),frame).map(screenGround):[];
    }
    function projectVisibleLineSegments(coordinates){
      if(!groundCache.has(coordinates))groundCache.set(coordinates,coordinates.map(p=>{p=ll(p);return localGround([p.lng,p.lat]);}));
      const points=groundCache.get(coordinates),frame=visibleGroundFrame();
      if(!frame.length)return [];
      const segments=[];
      for(let i=1;i<points.length;i++){
        const segment=clipGroundSegment(points[i-1],points[i],frame);if(!segment)continue;
        const a=screenGround(segment[0]),b=screenGround(segment[1]);
        if([a.x,a.y,b.x,b.y].every(Number.isFinite))segments.push([a,b]);
      }
      return segments;
    }
    function groundLinePath(coordinates){
      let path='',last=null;
      for(const [a,b] of projectVisibleLineSegments(coordinates)){
        if(!last||Math.hypot(last.x-a.x,last.y-a.y)>.01)path+='M'+a.x+','+a.y;
        path+='L'+b.x+','+b.y;last=b;
      }
      return path;
    }
    function clip() {
      studyPathD=roundedStudyPath(insetDisplayRing(projectGroundRing(data.studyBoundary),6));
      studyClipPath.setAttribute('d',studyPathD);
      const value='url(#'+studyClip.id+')';
      basemapPaths.forEach((node,i)=>node.setAttribute('d',basemapPolygonPath(basemapPolygons[i])));
      gl.getCanvas().style.clipPath = 'url(#'+basemapClip.id+')';
      el.dataset.basemapMaskMode='permanent-station-links';
      Object.entries(panes).forEach(([name,p])=>{p.style.clipPath=(['libraryForeground','attractionPane','transportStopPane','transportHaloPane','studyRouteLinePane','studyRouteArrowPane'].includes(name))?'none':value;});
      el.dataset.studyClip = 'ftz_boundary_2025';el.dataset.boundaryCornerRadius='8';el.dataset.boundaryInset='6';
    }
    function svgPane(name) {
      const parent = pane(name);
      if (!parent._svg) {
        const svg = document.createElementNS(svgNS, 'svg');
        svg.setAttribute('width','100%');svg.setAttribute('height','100%');
        svg.style.overflow='visible';parent.appendChild(svg);parent._svg=svg;
      }
      return parent._svg;
    }
    class Layer {
      constructor(kind,coordinates,options) {
        this.kind=kind;this.coordinates=coordinates;this.options=options || {};this.events={};this.element=null;
      }
      on(name,fn) {(this.events[name] || (this.events[name]=[])).push(fn);return this;}
      fire(name,e) {(this.events[name]||[]).forEach(fn=>fn(e));}
      addTo(group) {group.addLayer(this);return this;}
      getElement() {return this.element;}
      setOpacity(value) {this.opacity=value;if(this.element)this.element.style.opacity=value;return this;}
      setIcon(icon) {this.options.icon=icon;if(this.element)this.paintIcon();return this;}
      setTooltipContent(html) {this.tooltipHTML=html;if(this.tip)this.tip.innerHTML=html;return this;}
      paintIcon() {
        const icon=this.options.icon.options, node=this.element;
        node.className=(icon.className || '')+' main-map-marker';
        if(icon.iconUrl) {node.src=icon.iconUrl;node.alt=this.options.alt || '';}
        else node.innerHTML=icon.html || '';
        const size=icon.iconSize || [36,45];node.style.width=size[0]+'px';node.style.height=size[1]+'px';
      }
      mount() {
        if(this.element)return;
        const o=this.options;
        if(this.kind==='marker') {
          const icon=o.icon.options;
          const node=document.createElement(icon.iconUrl?'img':'div');this.element=node;this.paintIcon();
          Object.assign(node.style,{position:'absolute',pointerEvents:o.interactive===false?'none':'auto'});
          if(o.keyboard!==false) {node.tabIndex=0;node.setAttribute('role','button');}
          if(o.title) node.setAttribute('aria-label',o.title);
          ['mouseover','mouseout','click','focus','blur'].forEach(name=>node.addEventListener(name,e=>{
            if(name==='click')e.stopPropagation();this.fire(name,e);
            if(name==='mouseover'||name==='focus'){this.raised=true;this.draw();this.showTip();}
            if(name==='mouseout'||name==='blur'){this.raised=false;this.draw();this.hideTip();}
          }));
          node.addEventListener('keydown',e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();e.stopPropagation();this.fire('click',e);}});
          pane(o.pane || 'attractionPane').appendChild(node);
        } else {
          const node=document.createElementNS(svgNS,this.kind==='line'?'path':'circle');this.element=node;
          node.setAttribute('stroke',o.color || '#49663c');node.setAttribute('stroke-width',o.weight || 2);
          node.setAttribute('stroke-opacity',o.opacity == null?1:o.opacity);
          node.setAttribute('fill',this.kind==='line'?'none':(o.fillColor || o.color || '#49663c'));
          node.setAttribute('fill-opacity',o.fillOpacity == null?0:o.fillOpacity);
          node.setAttribute('stroke-linecap','round');node.setAttribute('stroke-linejoin','round');
          if(o.dashArray)node.setAttribute('stroke-dasharray',o.dashArray);
          if(o.className)node.setAttribute('class',o.className);
          svgPane(o.pane || 'overlayPane').appendChild(node);
        }
        if(o.routeRole)this.element.dataset.routeRole=o.routeRole;
        if(o.routePatternId)this.element.dataset.routePatternId=o.routePatternId;
        if(this.opacity!=null)this.setOpacity(this.opacity);
        layers.add(this);this.draw();this.fire('add');emit('layeradd',{layer:this});
      }
      draw() {
        if(!this.element)return;
        if(this.kind==='line')this.element.setAttribute('d',this.options.studyOutline?studyPathD:groundLinePath(this.coordinates));
        else {
          const coordinate=ll(this.coordinates),ground=localGround([coordinate.lng,coordinate.lat]);
          const frame=visibleGroundFrame();
          const visible=frame.length && !!clipGroundSegment(ground,ground,frame);
          this.element.style.display=visible?'':'none';
          if(!visible){this.hideTip();return;}
          const q=project(this.coordinates);
          if(this.kind==='marker') {
            const anchor=this.options.icon.options.iconAnchor || [0,0];
            this.element.style.transform='translate('+(q.x-anchor[0])+'px,'+(q.y-anchor[1])+'px)';
            this.element.style.zIndex=String(Math.round(q.y)+(this.raised && this.options.riseOnHover ? 10000 : 0));
          } else {this.element.setAttribute('cx',q.x);this.element.setAttribute('cy',q.y);this.element.setAttribute('r',this.options.radius);}
          if(this.tip)this.positionTip(q);
        }
      }
      positionTip(q) {
        const width=this.tip.offsetWidth,height=this.tip.offsetHeight;
        this.tip.style.left=Math.max(4,Math.min(el.clientWidth-width-4,q.x+22))+'px';
        this.tip.style.top=Math.max(4,Math.min(el.clientHeight-height-4,q.y-height/2))+'px';
      }
      showTip() {
        if(this.options.disableTooltip || !this.tooltipHTML || this.tip)return;
        this.tip=document.createElement('div');this.tip.className='main-map-tooltip';this.tip.innerHTML=this.tooltipHTML;
        pane('tooltipPane').appendChild(this.tip);this.positionTip(project(this.coordinates));
      }
      hideTip() {if(this.tip){this.tip.remove();this.tip=null;}}
      unmount() {this.hideTip();if(this.element)this.element.remove();this.element=null;layers.delete(this);}
    }
    class Marker extends Layer {constructor(p,o){super('marker',p,o);}}
    function group() {
      const members=new Set();const g={addTo(){groups.add(g);return g;},
        addLayer(layer){if(!members.has(layer)){members.add(layer);layer.mount();}return g;},
        removeLayer(layer){members.delete(layer);layer.unmount();return g;},hasLayer:layer=>members.has(layer),
        clearLayers(){members.forEach(layer=>layer.unmount());members.clear();return g;}};return g;
    }
    const primitives={Marker,marker:(p,o)=>new Marker(p,o),polyline:(p,o)=>new Layer('line',p,o),
      circleMarker:(p,o)=>new Layer('circle',p,o),layerGroup:group,icon:o=>({options:o}),divIcon:o=>({options:o}),
      latLng:(lat,lng)=>({lat,lng}),latLngBounds:points=>points,point:(x,y)=>({x,y}),
      DomUtil:{setPosition(node,p){node.style.left=(p.x||0)+'px';node.style.top=(p.y||0)+'px';}},
      DomEvent:{disableClickPropagation(node){node.addEventListener('click',e=>e.stopPropagation());}}};
    const surface={isMapboxMain:true,overlayPrimitives:primitives,gl,
      createPane:pane,getPane:name=>panes[name],
      latLngToLayerPoint:project,latLngToContainerPoint:project,
      getBearing:()=>gl.getBearing(),getPitch:()=>gl.getPitch(),projectGroundRing,projectVisibleLineSegments,
      layerPointToLatLng:p=>gl.unproject([p.x,p.y]),containerPointToLatLng:p=>gl.unproject(p),
      containerPointToLayerPoint:p=>({x:p[0],y:p[1]}),
      getCenter:()=>gl.getCenter(),getZoom:()=>gl.getZoom()+1,getSize:()=>({x:el.clientWidth,y:el.clientHeight}),
      getBoundsZoom(points) {
        const xs=points.map(p=>ll(p).lng),ys=points.map(p=>ll(p).lat);
        const camera=gl.cameraForBounds([[Math.min(...xs),Math.min(...ys)],[Math.max(...xs),Math.max(...ys)]]);
        return camera ? camera.zoom+1 : 13.5;
      },
      setMinZoom:z=>gl.setMinZoom(z-1),invalidateSize:()=>gl.resize(),
      whenReady(fn){fn();},
      on(names,fn){names.split(' ').forEach(n=>(events[n] || (events[n]=[])).push(fn));return surface;},
      off(names,fn){names.split(' ').forEach(n=>events[n]=(events[n]||[]).filter(f=>f!==fn));return surface;},
      removeLayer:g=>{g.clearLayers();groups.delete(g);},
      layerManager:{getLayer:(_,id)=>attractions[id]},
      addAttractions(rows) {
        const g=group().addTo(surface);pane('attractionPane').style.zIndex='500';pane('libraryForeground').style.zIndex='520';pane('tooltipPane').style.zIndex='700';
        rows.forEach(row=>{
          const marker=primitives.marker([row.lat,row.lng],{pane:row.id==='ATTR001'?'libraryForeground':'attractionPane',keyboard:true,riseOnHover:true,disableTooltip:true,
            title:row.name,alt:row.name,icon:primitives.icon({iconUrl:row.icon,iconSize:[36,45],iconAnchor:[18,43]})});
          attractions[row.id]=marker;marker.originalCoordinates=[row.lat,row.lng];
          const reference=verifiedPoiPositions[row.id];
          if(reference && inStudy(reference.coordinates) && separationMetres(marker.originalCoordinates,reference.coordinates)<=250){
            marker.coordinates=[reference.coordinates[1],reference.coordinates[0]];marker.poiReference=reference;
          }
          marker.on('click',()=>Shiny.setInputValue('map_marker_click',{id:row.id},{priority:'event'}));
          marker.addTo(g);marker.element.dataset.attractionId=row.id;marker.element.dataset.positionSource=marker.poiReference?'mapbox-reference':'original-entrance';
          if(marker.poiReference)marker.element.dataset.mapboxName=marker.poiReference.name;
        });
      },
      dispose() {
        if(disposed)return;disposed=true;emit('unload');
        if(filterPanel){filterPanel.removeEventListener('change',submitFilterIntent,true);filterPanel.onchange=previousFilterHandler;}
        Object.entries(nativeHandlers).forEach(([n,fn])=>gl.off(n,fn));
        groups.forEach(g=>g.clearLayers());Object.values(panes).forEach(p=>p.remove());
        gl.off('error',onError);gl.off('idle',syncAttractionsToPois);gl.off('moveend',queuePoiSync);gl.off('sourcedata',queuePoiSync);
        clearTimeout(poiTimer);['pointerdown','wheel','keydown'].forEach(n=>el.removeEventListener(n,markInteracted));
        status.remove();toneSvg.remove();el._mainSurface=null;
      }
    };
    // Rain and heat retain the study polygon. The basemap permanently includes
    // every station-link corridor. Foreground pins, route strokes and arrows keep
    // complete silhouettes; visible-ground clipping guards camera projection.
    pane('overlayPane');
    const boundary = new Layer('line',data.studyBoundary.map(p=>[p[1],p[0]]),{
      pane:'overlayPane',studyOutline:true,color:'#738a79',weight:1,opacity:.35});
    const boundaryGroup=group().addTo(surface);boundary.addTo(boundaryGroup);
    let selected=null;
    Shiny.addCustomMessageHandler('selected-attraction',message=>{
      if(selected){selected.unmount();selected=null;}
      const marker=attractions[message.id];
      Object.entries(attractions).forEach(([id,m])=>{if(m.element)m.element.setAttribute('aria-pressed',String(id===message.id));});
      if(marker){selected=new Layer('circle',marker.coordinates,{pane:'selectedAttraction',radius:23,color:'#49663c',weight:2,opacity:.85,fillOpacity:.07});selected.mount();}
    });
    let poiTimer=null, initialPoiFit=false, interacted=false;
    function markInteracted(){interacted=true;}
    ['pointerdown','wheel','keydown'].forEach(n=>el.addEventListener(n,markInteracted,{passive:true}));
    function inStudy(coordinate) {
      let inside=false;const ring=data.studyBoundary,x=coordinate[0],y=coordinate[1];
      for(let i=0,j=ring.length-1;i<ring.length;j=i++) {
        const a=ring[i],b=ring[j];
        if((a[1]>y)!==(b[1]>y) && x<(b[0]-a[0])*(y-a[1])/(b[1]-a[1])+a[0])inside=!inside;
      }
      return inside;
    }
    function separationMetres(a,b) {
      const lat=(a[0]+b[1])*Math.PI/360;
      return Math.hypot((a[1]-b[0])*111320*Math.cos(lat),(a[0]-b[1])*111320);
    }
    function syncAttractionsToPois() {
      if(disposed || !gl.isStyleLoaded() || !Object.keys(attractions).length)return;
      const pois=gl.queryRenderedFeatures({target:{featuresetId:'poi',importId:'basemap'}});
      // Outside native POIs can otherwise leave a half icon inside the mask.
      // Hide those complete symbols, without changing inside labels or positions.
      pois.forEach(f=>{
        if(f.geometry?.type==='Point'&&f.id!=null&&!inStudy(f.geometry.coordinates)&&!f.state?.hide)
          gl.setFeatureState({id:f.id,namespace:f.namespace,target:f.target},{hide:true});
      });
      let moved=false;
      Object.entries(attractions).forEach(([id,marker])=>{
        if(marker.poiMatch)return;
        const aliases=(poiAliases[id]||[]).map(normalizedName);
        const candidates=pois.filter(f=>f.geometry?.type==='Point' &&
          aliases.includes(normalizedName(f.properties.name)) && inStudy(f.geometry.coordinates) &&
          separationMetres(marker.originalCoordinates,f.geometry.coordinates)<=250 &&
          (normalizedName(f.properties.name)!=='flindersstreet' ||
            (f.properties.group==='transit' && f.properties.transit_mode==='metro_rail' &&
             f.properties.transit_stop_type==='entrance' && separationMetres(marker.originalCoordinates,f.geometry.coordinates)<30)));
        const unique=[...new Map(candidates.map(f=>[String(f.id)+'|'+f.namespace,f])).values()];
        if(unique.length!==1)return; // An ambiguous name must not choose another venue.
        const f=unique[0],c=f.geometry.coordinates;
        marker.coordinates=[c[1],c[0]];marker.poiMatch={id:f.id,namespace:f.namespace,target:f.target,name:f.properties.name};
        marker.draw();marker.element.dataset.positionSource='mapbox-poi';
        marker.element.dataset.mapboxName=f.properties.name;
        marker.element.dataset.mapboxFeatureId=String(f.id);
        // The custom pin replaces this same POI's native symbol, preventing a double icon.
        gl.setFeatureState({id:f.id,namespace:f.namespace,target:f.target},{hide:true});
        if(marker.element.getAttribute('aria-pressed')==='true' && selected){selected.coordinates=marker.coordinates;selected.draw();}
        moved=true;
      });
      el.dataset.mapboxAlignedAttractions=String(Object.values(attractions).filter(m=>m.poiMatch).length);
      if(moved && !initialPoiFit && !interacted) {
        initialPoiFit=true;
        const positions=Object.values(attractions).map(m=>m.coordinates);
        gl.fitBounds([[Math.min(...positions.map(p=>p[1])),Math.min(...positions.map(p=>p[0]))],
          [Math.max(...positions.map(p=>p[1])),Math.max(...positions.map(p=>p[0]))]],{
          padding:{top:65,bottom:75,left:Math.min(285,el.clientWidth*.3),right:65},retainPadding:false,duration:0});
      }
    }
    function queuePoiSync(){clearTimeout(poiTimer);poiTimer=setTimeout(syncAttractionsToPois,400);}
    gl.on('idle',syncAttractionsToPois);gl.on('moveend',queuePoiSync);gl.on('sourcedata',queuePoiSync);
    // Native 3-D precipitation is perspective-dependent in the initial top-down
    // camera. A light, uniformly tiled screen projection makes both sides of
    // the study area read as rainy. It follows the native getRain() lifecycle,
    // shares the exact FTZ clip and stays below every semantic overlay.
    const rainPane=pane('rainProjection');rainPane.style.zIndex='390';
    const rainCanvas=document.createElement('canvas');
    rainCanvas.className='main-map-rain-projection';rainCanvas.setAttribute('aria-hidden','true');
    Object.assign(rainCanvas.style,{position:'absolute',inset:'0',pointerEvents:'none'});
    rainCanvas.hidden=true;rainPane.appendChild(rainCanvas);
    const rainContext=rainCanvas.getContext('2d');let lastRainFrame=-Infinity;
    function drawRainProjection(time){
      if(!wetWeather){rainCanvas.hidden=true;return;}
      rainCanvas.hidden=false;
      if(time-lastRainFrame<32)return;lastRainFrame=time;
      const width=el.clientWidth,height=el.clientHeight,dpr=Math.min(window.devicePixelRatio||1,2);
      if(!width||!height||!rainContext)return;
      if(rainCanvas.width!==Math.round(width*dpr)||rainCanvas.height!==Math.round(height*dpr)){
        rainCanvas.width=Math.round(width*dpr);rainCanvas.height=Math.round(height*dpr);
        rainCanvas.style.width=width+'px';rainCanvas.style.height=height+'px';
      }
      rainContext.setTransform(dpr,0,0,dpr,0,0);rainContext.clearRect(0,0,width,height);
      // Equal cells, including overscan, prevent wind or resizing from moving
      // all droplets to one edge. Deterministic jitter avoids a visible grid.
      const cell=58,columns=Math.ceil(width/cell)+2,rows=Math.ceil(height/cell)+2;
      const spanX=columns*cell,spanY=rows*cell,t=time/1000;
      rainContext.beginPath();
      for(let row=0;row<rows;row++)for(let col=0;col<columns;col++){
        const seed=(row*73+col*151)%997;
        const x=((col*cell+(seed%47)+t*43)%spanX)-cell;
        const y=((row*cell+((seed*17)%47)+t*330)%spanY)-cell;
        const length=13+(seed%13);
        rainContext.moveTo(x,y);rainContext.lineTo(x+length*.20,y+length);
      }
      rainContext.lineCap='round';
      rainContext.strokeStyle='rgba(242,247,251,0.54)';rainContext.lineWidth=2.2;rainContext.stroke();
      rainContext.strokeStyle='rgba(91,119,143,0.55)';rainContext.lineWidth=.9;rainContext.stroke();
      rainCanvas.dataset.coverage='uniform-viewport';
    }
    const nativeHandlers={};
    let lastProjectedView = '';
    ['move','zoom','movestart','zoomstart','moveend','zoomend','resize','render'].forEach(name=>{
      const handler=e=>{
        const rain=gl.getRain();const raining=!!rain;
        if(raining!==wetWeather){wetWeather=raining;applyBasemapTone();if(!raining)rainCanvas.hidden=true;}
        if(name==='render')drawRainProjection(performance.now());
        const rainState=JSON.stringify(rain||null);
        if(el.dataset.rainState!==rainState)el.dataset.rainState=rainState;
        // Rain animates the GL canvas while the camera is stationary. Avoid
        // reprojecting all route geometry and rewriting DOM on every rain frame.
        if(name==='move'||name==='resize'||name==='render') {
          const c=gl.getCenter();
          const view=[c.lng,c.lat,gl.getZoom(),gl.getBearing(),gl.getPitch(),el.clientWidth,el.clientHeight].join('|');
          if(view!==lastProjectedView){lastProjectedView=view;clip();layers.forEach(layer=>layer.draw());}
          else if(name==='render')return;
          el.dataset.cameraBearing=String(gl.getBearing());el.dataset.cameraPitch=String(gl.getPitch());
          el.dataset.cameraZoom=String(gl.getZoom());el.dataset.cameraCenter=gl.getCenter().toArray().join(',');
        }
        emit(name,e);
      };nativeHandlers[name]=handler;gl.on(name,handler);
    });
    nativeHandlers.remove=()=>surface.dispose();gl.on('remove',nativeHandlers.remove);
    gl.dragRotate.enable();gl.touchZoomRotate.enableRotation();gl.touchPitch.enable();
    gl.keyboard.enableRotation();
    gl.addControl(new mapboxgl.NavigationControl({showCompass:true,visualizePitch:true}),'bottom-right');
    gl.addControl(new mapboxgl.AttributionControl({compact:true,customAttribution:
      '<a href="https://data.melbourne.vic.gov.au/" title="Street mask, CC BY 4.0">City of Melbourne</a>'}),'bottom-right');
    const status=document.createElement('div');status.className='main-map-status';status.setAttribute('role','status');
    status.textContent='Loading map…';el.appendChild(status);
    function onError(e) {
      const code=e.error && e.error.status;
      status.textContent=code===401||code===403?'Map access denied. Check the public token and its URL restrictions.':'Map could not load. Check your connection and retry.';
      status.hidden=false;el.dataset.mapboxStatus='error';
    }
    gl.on('error',onError);
    function loaded(){
      status.hidden=true;el.dataset.mapboxStatus='loaded';
      syncAttractionsToPois();
      Shiny.setInputValue('main_map_style_ready',Date.now(),{priority:'event'});
    }
    if(gl.loaded())loaded();else gl.once('load',loaded);
    const a=data.attractions;
    gl.fitBounds([[Math.min(...a.map(r=>r.lng)),Math.min(...a.map(r=>r.lat))],
      [Math.max(...a.map(r=>r.lng)),Math.max(...a.map(r=>r.lat))]],{
      padding:{top:50,bottom:75,left:Math.min(285,el.clientWidth*.3),right:65},retainPadding:false,duration:0});
    clip();el._mainSurface=surface;return surface;
  };
})();
