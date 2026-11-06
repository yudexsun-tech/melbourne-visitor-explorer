/* One persistent Tableau dashboard, filtered exclusively by the current attraction. */
(function (root) {
  'use strict';
  const SOURCE = 'https://public.tableau.com/views/ratinggraph/AttractionReview';
  const API = 'https://public.tableau.com/javascripts/api/tableau.embedding.3.latest.min.js';
  const FIELD = 'Attraction ID';
  function deadline(promise, ms) {
    let timer;
    return Promise.race([promise, new Promise((_, reject) => {
      timer = setTimeout(() => reject(new Error('Review chart timed out')), ms);
    })]).finally(() => clearTimeout(timer));
  }
  // Adapter separation lets tests exercise real async switching without a network.
  function createController(adapter, allowedIds, timeout = 60000) {
    let desired = '', active = false, applied = '', current = null, generation = 0;
    let running = false, failed = false;
    async function pump() {
      if (!active || !desired || running || failed) return;
      if (current && applied === desired) { adapter.show('ready', applied); return; }
      const mine = generation;
      running = true;
      adapter.show('loading', desired);
      try {
        if (!current) {
          const created = await deadline(adapter.create(desired), timeout);
          if (mine !== generation) { created.dispose(); return; }
          current = created;
        }
        while (mine === generation && active && desired && applied !== desired) {
          const id = desired;
          await deadline(current.filter(id), timeout);
          if (mine !== generation) return;
          applied = id;
        }
        if (mine === generation && applied === desired) adapter.show('ready', applied);
      } catch (error) {
        if (mine === generation) {
          failed = true;
          adapter.show('error', desired);
          adapter.log(error);
        }
      } finally {
        if (mine === generation) { running = false; }
      }
    }
    return {
      update(message) {
        desired = allowedIds.includes(message.attractionId) ? message.attractionId : '';
        active = message.active === true;
        adapter.link(desired ? SOURCE + '?:showVizHome=no&Attraction%20ID=' + encodeURIComponent(desired) : SOURCE);
        if (!desired) { adapter.show('idle', ''); return; }
        if (applied !== desired) adapter.show(failed ? 'error' : 'loading', desired);
        void pump();
      },
      invalidate() {
        if (running) return;
        applied = '';
        void pump();
      },
      retry() {
        generation += 1;
        if (current) current.dispose();
        current = null; applied = ''; running = false; failed = false;
        adapter.reset();
        void pump();
      }
    };
  }
  if (typeof module !== 'undefined' && module.exports) module.exports = {createController, SOURCE};
  if (!root.document) return;
  function setup() {
    const host = document.getElementById('reviews-viz-host');
    const section = document.getElementById('attraction-reviews');
    if (!host || !section) return;
    let library, liveViz, liveReady = false, adapterGeneration = 0;
    const adapter = {
      async create(id) {
        const mine = adapterGeneration;
        // Import on first detail visit only; map startup remains independent of Tableau.
        library = library || import(API).catch(error => { library = null; throw error; });
        const {TableauViz, TableauEventType, FilterUpdateType} = await library;
        if (mine !== adapterGeneration) throw new Error('Superseded review request');
        const viz = new TableauViz();
        liveViz = viz; liveReady = false;
        viz.src = SOURCE; viz.toolbar = 'hidden'; viz.hideTabs = true;
        viz.device = 'desktop'; viz.width = '1200px'; viz.height = '500px';
        viz.iframeAttributeLoading = 'eager';
        viz.addFilter(FIELD, id);
        const interactive = new Promise((resolve, reject) => {
          viz.addEventListener(TableauEventType.FirstInteractive, resolve, {once: true});
          viz.addEventListener('vizloaderror', () => reject(new Error('Tableau could not load')), {once: true});
        });
        host.replaceChildren(viz);
        await interactive;
        if (mine === adapterGeneration) liveReady = true;
        // Tableau Public may retain its toolbar even when toolbar=hidden. Revert/Undo
        // must not leave the displayed attraction different from the detail heading.
        const verifyAttraction = async () => {
          if (section.dataset.state !== 'ready' || mine !== adapterGeneration) return;
          try {
            const sheet = viz.workbook.activeSheet;
            for (const worksheet of (sheet.worksheets || [sheet])) {
              const filter = (await worksheet.getFiltersAsync()).find(item => item.fieldName === FIELD);
              if (section.dataset.state !== 'ready') return;
              if (!filter || filter.isAllSelected || filter.appliedValues?.length !== 1 ||
                  filter.appliedValues[0].value !== section.dataset.appliedAttraction) {
                controller.invalidate(); return;
              }
            }
          } catch (error) { adapter.log(error); controller.invalidate(); }
        };
        viz.addEventListener(TableauEventType.FilterChanged, verifyAttraction);
        viz.addEventListener(TableauEventType.ToolbarStateChanged, verifyAttraction);
        return {
          async filter(attractionId) {
            const sheet = viz.workbook.activeSheet;
            const worksheets = sheet.worksheets || [sheet];
            if (!worksheets.length) throw new Error('No review worksheet');
            for (const worksheet of worksheets) {
              await worksheet.applyFilterAsync(FIELD, [attractionId], FilterUpdateType.Replace);
              const filters = await worksheet.getFiltersAsync();
              const filter = filters.find(item => item.fieldName === FIELD);
              if (!filter || filter.isAllSelected || !filter.appliedValues ||
                  filter.appliedValues.length !== 1 || filter.appliedValues[0].value !== attractionId) {
                throw new Error('The review attraction filter was not applied');
              }
            }
            section.dataset.filterConfirmations = String(Number(section.dataset.filterConfirmations || 0) + 1);
            viz.resize();
          },
          dispose() { viz.remove(); }
        };
      },
      show(state, id) {
        section.dataset.state = state;
        section.dataset.appliedAttraction = state === 'ready' ? id : '';
        section.setAttribute('aria-busy', String(state === 'loading'));
        document.getElementById('reviews-message').hidden = state === 'ready';
        document.getElementById('reviews-recovery').hidden = state !== 'error';
        document.getElementById('reviews-status').textContent = state === 'error'
          ? 'Reviews could not load. Check your internet connection and try again.'
          : state === 'idle' ? 'Select an attraction to see reviews.' : 'Loading reviews…';
      },
      link(url) { document.getElementById('reviews-external').href = url; },
      reset() { adapterGeneration += 1; host.replaceChildren(); liveViz = null; liveReady = false; },
      log(error) { console.warn('Monthly reviews:', error.message); }
    };
    const controller = createController(adapter, section.dataset.allowedIds.split(','));
    document.getElementById('reviews-retry').addEventListener('click', () => controller.retry());
    const scrollArea = section.querySelector('.reviews-scroll');
    const stage = section.querySelector('.reviews-stage');
    let previousWidth = 0;
    function fitReviews() {
      const available = scrollArea.clientWidth;
      if (!available || available === previousWidth) return;
      previousWidth = available;
      const displayWidth = Math.max(720, Math.min(1200, available));
      const scale = displayWidth / 1200;
      stage.style.width = displayWidth + 'px';
      stage.style.height = (500 * scale) + 'px';
      host.style.transform = 'scale(' + scale + ')';
      if (liveReady && liveViz) liveViz.resize();
    }
    new ResizeObserver(fitReviews).observe(scrollArea);
    fitReviews();
    let registered = false;
    function connect() {
      if (!root.Shiny || typeof root.Shiny.setInputValue !== 'function' ||
          typeof root.Shiny.addCustomMessageHandler !== 'function') return;
      if (!registered) { root.Shiny.addCustomMessageHandler('review-attraction', controller.update); registered = true; }
      root.Shiny.setInputValue('reviews_ready', true, {priority: 'event'});
    }
    root.jQuery(document).on('shiny:connected', connect);
    connect();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', setup);
  else setup();
})(typeof window === 'undefined' ? globalThis : window);
