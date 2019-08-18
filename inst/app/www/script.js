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

$(window).load(function() {
  setTimeout(
    function() {
      $grid.isotope('shuffle');
    }, 2000);
});
