document.addEventListener(
  "DOMContentLoaded",
  function() {
    console.log("here");
    
    // Instrument all links so that we can see what the user clicked.
    jQuery("a").each(function() {
      var link = this.getAttribute("href");
      if (!link)
	return;
      jQuery(this).click(function(e) {
	// Only record clicks with left or middle mouse button.
	if (e.which != 1 && e.which != 2)
	  return true;
	var go = function(result) {
	  // Middle mouse button opens new tab/window.
	  if (e.which == 1)
	    window.location.href = link;
	  else
	    window.open(link, "_blank");
	};
	jQuery.ajax({
	  url: "/wp-content/plugins/bang/visit.php?click=" +
	    encodeURIComponent(link) +
	    "&page=" + encodeURIComponent(window.location.href),
	  dataType: "json",
	  success: go,
	  error: go
	});
	e.preventDefault();
	return true;
      });
    });

    // Record that we've loaded the page.
    var title = jQuery(".entry-title").text();
    if (!title) {
      title = document.title;
      if (title)
	title = title.replace(/ +\u2013.*$/, "");
    }
    jQuery.ajax({
      url: "/wp-content/plugins/bang/visit.php?ref=" +
	encodeURIComponent(document.referrer) +
	"&page=" + encodeURIComponent(window.location.href) +
	"&title=" + encodeURIComponent(title),
      dataType: "json",
      success: function(result) {
      }
    });
  },
  false);
