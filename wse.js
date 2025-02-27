document.addEventListener(
  "DOMContentLoaded",
  function() {
    function instrumentElements() {
      // Instrument all links so that we can see what the user clicked.
      jQuery("a").each(function() {
	var link = this.getAttribute("href");
	if (!link)
	  return;
	if (this.getAttribute("data-wse"))
	  return;
	this.setAttribute("data-wse", "true");
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
	    url: "/wp-content/plugins/wse/visit.php",
	    type: "POST",
	    data: {
	      "click": link,
	      "page": window.location.href
	    },
	    dataType: "json",
	    success: go,
	    error: go
	  });
	  e.preventDefault();
	  return true;
	});
      });

      // Instrument <video> elements.
      jQuery("video").each(function() {
	var link;
	var $source = jQuery(this).find("source");
	if (!this.autoplay && $source.length == 1) {
	  if (this.getAttribute("data-wse"))
	    return;
	  this.setAttribute("data-wse", "true");
	  link = $source.attr("src");
	  jQuery(this).on("playing", function() {
	    jQuery(this).off("playing");
	    var link;
	    var $source = jQuery(this).find("source");
	    if ($source.length == 1) {
	      link = $source.attr("src");
	      jQuery.ajax({
		url: "/wp-content/plugins/wse/visit.php",
		type: "POST",
		data: {
		  "click": link,
		  "page": window.location.href
		},
		dataType: "json",
		success: function(result) {
		}
	      });
	    }
	    return true;
	  });
	}
      });

      // Instrument Lyte video elements.
      jQuery(".lyte-wrapper > div[itemprop='video']").click(function() {
	if (this.id) {
	  if (this.getAttribute("data-wse"))
	    return;
	  this.setAttribute("data-wse", "true");
	  var link = "https://www.youtube.com/watch?v=" +
	      this.id.replace(/^WYL_/, "");
	  jQuery.ajax({
	    url: "/wp-content/plugins/wse/visit.php",
	    type: "POST",
	    data: {
	      "click": link,
	      "page": window.location.href
	    },
	    dataType: "json",
	    success: function(result) {
	    }
	  });
	}
      });
    };

    instrumentElements();

    var registerVisit = function(referrer) {
      // Get the title from the entry title, but fall back on the
      // document title.
      var title;
      var $title = jQuery(".entry-title");
      // If we have more than one hit, we're probably on the front page,
      // so fall back on the document title instead.
      if ($title.length == 1)
	title = $title.text();
      if (!title) {
	title = document.title;
	if (title)
	  title = title.replace(/ +\u2013.*$/, "");
      }
      jQuery.ajax({
	url: "/wp-content/plugins/wse/visit.php",
	type: "POST",
	data: {
	  "ref": referrer,
	  "page": window.location.href,
	  "title": title
	},
	dataType: "json",
	success: function(result) {
	}
      });
    };

    // Detect when the document href changes.  This happens when using
    // "infinite scroll", particularly on the front pages.
    var oldHref = document.location.href;
    setInterval(function() {
      if (oldHref != document.location.href) {
	registerVisit(oldHref);
        oldHref = document.location.href;
      }
    }, 1000);

    // Check the document to see if we need to instrument new elements
    // that may have arrived via infinite scrolling.
    var observer = new MutationObserver(function(mutations) {
      instrumentElements();
    });
    
    observer.observe(document.querySelector('body'), {
      childList: true,
      subtree: true
    });

    // Record that we've loaded this page.
    registerVisit(document.referrer);
  },
  false);
