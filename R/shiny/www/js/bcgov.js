$(document).ready(function() {
  // Explore population
  $('.explore-population').on('click', function() {
    $('a[data-value="Census Topics"]').click();
    setTimeout(findSpan, 500, "Population");
  });

  // Explore housing
  $('.explore-housing').on('click', function() {
    $('a[data-value="Census Topics"]').click();
    setTimeout(findSpan, 500, "Housing");
  });

  // Explore mobility
  $('.explore-mobility').on('click', function() {
    $('a[data-value="Census Topics"]').click();
    setTimeout(findSpan, 500, "Mobility");
  });

  // Explore STIR
  $('.explore-stir').on('click', function() {
    $('a[data-value="Census Topics"]').click();
    setTimeout(findSpan, 500, "STIR");
  });

  // Explore PTT
  $('.explore-ptt').on('click', function() {
    $('a[data-value="Property Transfer Tax"]').click();
  });

  // Footer about link
  $('.about-project').on('click', function() {
    $('a[data-value="Project background"]').click();
  });

  // Back to top
	$(".back-to-top").on("click", function(e) {
		e.preventDefault();
    $('html,body').animate({ scrollTop: 0 }, 'slow');
	});
});

function findSpan(spanLabel) {
  var spans = $('.leaflet-control-layers-base').find('span');
  $.each(spans, function(index) {
    if ($(this).text().trim() == spanLabel) {
      $(this).prev().click();
    }
  });
}

