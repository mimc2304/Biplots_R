// Pestaña Teoría: dibuja las fórmulas (LaTeX entre \( \) y \[ \]) con KaTeX,
// incluido en la carpeta www/katex (funciona sin conexión a internet).
(function () {
  function dibujar() {
    if (!window.renderMathInElement) return;
    document.querySelectorAll(".bl-teoria").forEach(function (el) {
      if (el.dataset.katex) return;
      renderMathInElement(el, {
        delimiters: [{ left: "\\[", right: "\\]", display: true },
                     { left: "\\(", right: "\\)", display: false }],
        throwOnError: false
      });
      el.dataset.katex = "1";
    });
  }
  $(document).on("shiny:connected shown.bs.tab", function () { setTimeout(dibujar, 50); });
  document.addEventListener("DOMContentLoaded", dibujar);
  setTimeout(dibujar, 800);
})();
