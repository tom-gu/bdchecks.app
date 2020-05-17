var $grid = $('.grid').isotope({
  itemSelector: '.element-item',
  layoutMode: 'fitRows',
  getSortData: {
   darwinCoreClass: '[darwinCoreClass]'
  },
  stagger: 50,
  transitionDuration: '0.8s'
});

// bind sort button click
$('.btn-group').on( 'click', 'button', function() {
  var sortValue = $(this).attr('data-sort-value');
  $grid.isotope({ filter: sortValue });
});

// change is-checked class on buttons
$('.btn-group').each( function( i, buttonGroup ) {
  var $buttonGroup = $( buttonGroup );
  $buttonGroup.on( 'click', 'button', function() {
    $buttonGroup.find('.is-checked').removeClass('is-checked');
    $( this ).addClass('is-checked');
  });
});
