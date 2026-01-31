document.addEventListener(
  "DOMContentLoaded",
  function() {
    // Helper: POST like jQuery.ajax({ type:'POST', data:{...} })
    function postWSE(data) {
      return fetch("/wp-content/plugins/wse/visit.php", {
        method: "POST",
        headers: {
          "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
        },
        body: new URLSearchParams(data).toString(),
      });
    }

    function instrumentElements() {
      // Instrument all links so that we can see what the user clicked.
      document.querySelectorAll("a").forEach(function(a) {
        var link = a.getAttribute("href");
        if (!link)
	  return;
        if (a.getAttribute("data-wse"))
	  return;
        a.setAttribute("data-wse", "true");

        a.addEventListener("click", function(e) {
          // jQuery uses e.which (1=L, 2=M, 3=R); native uses e.button (0=L,1=M,2=R).
          var which = e.which ||
	      (typeof e.button === "number" ? e.button + 1 : 1);

          // Only record clicks with left or middle mouse button.
          if (which !== 1 && which !== 2)
	    return true;

          var go = function() {
            // Middle mouse button opens new tab/window.
            if (which === 1)
	      window.location.href = link;
            else
	      window.open(link, "_blank");
          };

          postWSE({
            click: link,
            page: window.location.href,
          })
            .then(go)
            .catch(go);

          e.preventDefault();
          return true;
        });
      });

      // Instrument <video> elements.
      document.querySelectorAll("video").forEach(function(video) {
        var sources = video.querySelectorAll("source");
        if (!video.autoplay && sources.length === 1) {
          if (video.getAttribute("data-wse"))
	    return;
          video.setAttribute("data-wse", "true");

          var onPlaying = function() {
            video.removeEventListener("playing", onPlaying);
            var s2 = video.querySelectorAll("source");
            if (s2.length === 1) {
              var link = s2[0].getAttribute("src");
              if (link) {
                postWSE({
                  click: link,
                  page: window.location.href,
                });
              }
            }
            return true;
          };

          video.addEventListener("playing", onPlaying);
        }
      });

      // Instrument Lyte video elements.
      document
        .querySelectorAll(".lyte-wrapper > div[itemprop='video']")
        .forEach(function(el) {
          el.addEventListener("click", function() {
            if (this.id) {
              if (this.getAttribute("data-wse"))
		return;
              this.setAttribute("data-wse", "true");
              var link =
                  "https://www.youtube.com/watch?v=" +
                  this.id.replace(/^WYL_/, "");
              postWSE({
                click: link,
                page: window.location.href,
              });
            }
          });
        });
    }

    var registerVisit = function(referrer) {
      // Get the title from the entry title, but fall back on the
      // document title.
      var title;
      var titleEls = document.querySelectorAll(".entry-title");
      // If we have more than one hit, we're probably on the front page,
      // so fall back on the document title instead.
      if (titleEls.length === 1)
	title = titleEls[0].textContent;
      if (!title) {
        title = document.title;
        if (title)
	  title = title.replace(/ +\u2013.*$/, "");
      }

      postWSE({
        ref: referrer,
        page: window.location.href,
        title: title || "",
      });
    };

    instrumentElements();

    // Detect when the document href changes. This happens when using
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
    var observer = new MutationObserver(function() {
      instrumentElements();
    });

    observer.observe(document.querySelector("body"), {
      childList: true,
      subtree: true
    });

    // Record that we've loaded this page, but do it on a delay to filter
    // out bots, hopefully.  This also filters out people who close
    // the page quickly.
    setInterval(function() {
      registerVisit(document.referrer);
    }, 3000);
  },
  false
);
