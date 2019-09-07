var $grid = $('.grid').isotope({
  itemSelector: '.element-item',
  layoutMode: 'fitRows',
  getSortData: {
   darwinCoreClass: '[darwinCoreClass]',
   dimension: '[dimension]',
   warning: '[warning]',
   output: '[output]',
   severity: '[severity]'
  },
  stagger: 50,
  transitionDuration: '0.8s'
});

// bind sort button click
$('.btn-group').on( 'click', 'button', function() {
  var sortValue = $(this).attr('data-sort-value');
  $grid.isotope({ sortBy: sortValue });
});

// change is-checked class on buttons
$('.btn-group').each( function( i, buttonGroup ) {
  var $buttonGroup = $( buttonGroup );
  $buttonGroup.on( 'click', 'button', function() {
    $buttonGroup.find('.is-checked').removeClass('is-checked');
    $( this ).addClass('is-checked');
  });
});

// $grid.on( 'click', '.element-item', function() {
// var checkbox = $("input:checkbox[value=" + this.getElementsByTagName("h4")[0].innerHTML + "]");
//  checkbox.prop("checked", !checkbox.prop("checked"));
//});

function sortFunction(attribute) {
  var elements = document.getElementsByClassName('checksListContent');
  for (var i = 0; i < elements.length; i++) {
	  elements[i].querySelector(".rightSide").innerHTML = elements[i].getAttribute(attribute);
  }
}

document.getElementById("sortBydarwinCoreClass").onclick = function(){ sortFunction("darwinCoreClass") };
document.getElementById("sortBydimension").onclick = function(){ sortFunction("dimension") };
document.getElementById("sortBywarning").onclick = function(){ sortFunction("warning") };
document.getElementById("sortByoutput").onclick = function(){ sortFunction("output") };
document.getElementById("sortByseverity").onclick = function(){ sortFunction("severity") };
