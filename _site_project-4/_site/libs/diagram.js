// Mermaid diagrams you can zoom into, in a Quarto page (calcofi4r::cc_diagram_viewer()).
//
// Quarto draws every {mermaid} block in the browser (its quarto-diagram/mermaid-init.js
// replaces each <pre class="mermaid-js"> with an <svg class="mermaid-js"> on window load).
// This script does two things around that, so a page looks and behaves like the figures of
// the CalCOFI.io docs book (docs/libs/diagram.js + docs/libs/mermaid.config.json):
//
// 1. before Quarto draws (DOMContentLoaded, after mermaid-init.js has initialized mermaid),
//    it re-initializes mermaid with the book's look: Source Sans 3 at 15 px, SVG text labels,
//    curved edges. Quarto's own options (theme, themeCSS) are kept.
// 2. once Quarto has drawn every diagram, it puts each <svg> in a viewer: svg-pan-zoom's
//    +/−/reset controls, drag to pan, double-click to zoom, a full-screen button (Esc
//    closes). The mouse wheel zooms only in full screen, so a diagram never takes over the
//    page scroll.
//
// Drawing stays in the browser, so a page rendered on any machine is the same and no
// headless browser runs at render time.
(function () {
  if (window.__ccDiagramViewer) return;
  window.__ccDiagramViewer = true;

  // the book's mermaid look (docs/libs/mermaid.config.json)
  const LOOK = {
    themeVariables: {
      // the same font as --mermaid-font-family in diagram.css (labels are measured with this)
      fontFamily: "\"Source Sans 3\", \"Source Sans Pro\", \"Segoe UI\", Helvetica, Arial, sans-serif",
      fontSize: "15px",
    },
    flowchart: { htmlLabels: false, curve: "basis" },
    htmlLabels: false,
  };
  const H_MIN = 160, H_MAX = 560;   // px, the inline viewer's height range
  const WAIT_MS = 20000;            // give up waiting for Quarto's renders after this

  function restyle() {
    const m = window.mermaid;
    if (!m || typeof m.initialize !== "function") return;
    let cur = {};
    try { cur = (m.mermaidAPI && m.mermaidAPI.getConfig && m.mermaidAPI.getConfig()) || {}; } catch (e) { cur = {}; }
    m.initialize(Object.assign({}, cur, LOOK, {
      themeVariables: Object.assign({}, cur.themeVariables || {}, LOOK.themeVariables),
      flowchart: Object.assign({}, cur.flowchart || {}, LOOK.flowchart),
    }));
  }

  function fullscreenButton(wrap, pz) {
    const b = document.createElement("button");
    b.type = "button"; b.className = "cc-diagram-full"; b.title = "Full screen (Esc to close)";
    b.setAttribute("aria-label", "Toggle full screen"); b.textContent = "⤢";
    const set = (on) => {
      wrap.classList.toggle("is-full", on);
      document.body.classList.toggle("cc-diagram-open", on);
      b.textContent = on ? "✕" : "⤢";
      if (on) pz.enableMouseWheelZoom(); else pz.disableMouseWheelZoom();
      requestAnimationFrame(() => { pz.resize(); pz.fit(); pz.center(); });
    };
    b.addEventListener("click", () => set(!wrap.classList.contains("is-full")));
    document.addEventListener("keydown", (e) => { if (e.key === "Escape" && wrap.classList.contains("is-full")) set(false); });
    return b;
  }

  function upgrade(svg) {
    if (svg.closest(".cc-diagram-wrap") || typeof window.svgPanZoom !== "function") return;
    const parent = svg.parentElement;
    if (!parent) return;
    const colW = parent.clientWidth || 800;
    const vb = svg.viewBox && svg.viewBox.baseVal;
    // a margin inside the viewBox, so svg-pan-zoom's +/−/reset icons (bottom-right) and the
    // full-screen button (top-left) sit beside the diagram, not on it; svg-pan-zoom fits
    // and resets to the viewBox, so the margin holds after a reset too
    if (vb && vb.width > 0 && vb.height > 0) {
      const mx = Math.max(vb.width * 0.06, 48), my = Math.max(vb.height * 0.06, 40);
      svg.setAttribute("viewBox", [vb.x - mx, vb.y - my, vb.width + 2 * mx, vb.height + 2 * my].join(" "));
    }
    // as tall as the diagram at its natural size (never scaled up), within the range
    let h = H_MAX;
    if (vb && vb.width > 0 && vb.height > 0)
      h = Math.min(vb.height, colW * vb.height / vb.width) + 24;
    h = Math.round(Math.min(H_MAX, Math.max(H_MIN, h)));

    const wrap = document.createElement("div");
    wrap.className = "cc-diagram-wrap";
    wrap.style.height = h + "px";
    parent.insertBefore(wrap, svg);
    wrap.appendChild(svg);
    svg.removeAttribute("height");
    svg.setAttribute("width", "100%"); svg.setAttribute("height", "100%");
    svg.style.maxWidth = "none"; svg.style.width = "100%"; svg.style.height = "100%";

    const pz = window.svgPanZoom(svg, {
      zoomEnabled: true, controlIconsEnabled: true, fit: true, center: true,
      minZoom: 0.4, maxZoom: 40, zoomScaleSensitivity: 0.35,
      mouseWheelZoomEnabled: false, dblClickZoomEnabled: true, preventMouseEventsDefault: true,
    });
    wrap.appendChild(fullscreenButton(wrap, pz));
    // refit when the column changes width (sidebar toggles, window resize)
    if (window.ResizeObserver) new ResizeObserver(() => { pz.resize(); pz.fit(); pz.center(); }).observe(wrap);
  }

  // Quarto replaces each pre.mermaid-js with an svg.mermaid-js, one await at a time; wait
  // until none is left (or a render failed and left its <pre>), then upgrade them all
  function whenDrawn() {
    const t0 = Date.now();
    (function poll() {
      const pending = document.querySelectorAll("pre.mermaid-js").length;
      if (pending && Date.now() - t0 < WAIT_MS) return setTimeout(poll, 100);
      setTimeout(() => document.querySelectorAll("svg.mermaid-js").forEach(upgrade), 0);
    })();
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", restyle);
  else restyle();
  if (document.readyState === "complete") whenDrawn();
  else window.addEventListener("load", whenDrawn);
})();
