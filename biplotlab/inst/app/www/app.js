// Resalta en la cinta el análisis activo (estilo jamovi).
$(document).on("shiny:connected", function () {
  Shiny.addCustomMessageHandler("bl-activo", function (id) {
    $(".bl-ribbon-btn").removeClass("bl-activo");
    $(id === "datos" ? "#ir_datos" : "#abrir_" + id).addClass("bl-activo");
  });
  $("#ir_datos").addClass("bl-activo");
});
