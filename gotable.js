// When locator icon in datatable is clicked, go to that spot on the map
// type: word: show all languages with this word
// type: lng: show one language
$(document).on("click", ".go-table", function(e) {
  e.preventDefault();
  $el = $(this);
  var data = $el.data("data");
  var data2 = $el.data("data2");
  var data3 = $el.data("data3");
  var type = $el.data("type");
  
  $($("#nav a")[1]).tab("show");
  Shiny.onInputChange("goto", {
    data: data,
    data2: data2,
    data3:data3,
    type: type,
    nonce: Math.random()
  });
});