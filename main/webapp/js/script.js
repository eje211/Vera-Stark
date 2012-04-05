/* Author: Beyond the Stage

*/

/* Utility */
function clamp(val, min, max) { return Math.max(min, Math.min(max, val)); }

/* Auto-append classes onto input fields */
$.map($("input"), function(e, i) {
    if ( $(e).prop("type") == "text" ) { $(e).addClass("text"); }
    else if ( $(e).prop("type") == "button" ) { $(e).addClass("button"); }
    else if ( $(e).prop("type") == "submit" ) { $(e).addClass("button"); }
  });
$.map($("textarea"), function(e, i) {
    $(e).addClass("textarea");
  });

/* Positioning fixes on window resize events */
var windowResizeTimer;
var fixPositions = function() {
    var window_height = $(window).height();
    var header_height = $("header").height();
    var display_area_height = window_height - header_height;
    var window_width = $(window).width();
    var display_area_width = $("div#content").width();
    
    // Resize text artifact block to browser height
    if ( textsection = $("div.matwrap section") ) {
      if ( ! textsection.hasClass("static_page") ) {
        textsection.css("max-height", (display_area_height - parseInt(textsection.css("margin-top"))*8) );
        var max_width = (display_area_width - parseInt(textsection.css("margin-left"))*8);
        if ( max_width > 500 ) max_width = 500;
        textsection.css("max-width", max_width );
      }
    }
    
    // Resize and center featured artifact
    
    if ( imagemat = $("div.matwrap") ) {
      if ( $("div.matwrap section.static_page").length == 0 ) {
        $("div.matwrap img").css("max-height", (display_area_height - parseInt($("div.matwrap img").css("margin-top"))*2) * 0.9 );
        $("div.matwrap img").css("max-width", (display_area_width - parseInt($("div.matwrap img").css("margin-left"))*2) * 0.9 );
      }
      imagemat.css("top", clamp((display_area_height / 2) - (imagemat.height() / 2), 5, 100000));
    }
    
    // Reposition navigation tabs
    if ( left_tabs = $("div.flags_left") ) {
      left_tabs.css("top", display_area_height / 3 );
    }
    if ( right_tabs = $("div.flags_right") ) {
      right_tabs.css("top", display_area_height / 3 );
    }
  }
$(window).resize(function() {
    clearTimeout(windowResizeTimer);
    windowResizeTimer = setTimeout(fixPositions, 2);
  });

  
/* Add functionality to tabs */
if ( $("#flag_next").length ) {
  $("#flag_next").click(function() {
    });
  // Add global keyboard shortcut
  $(document).keydown(function(event) { // "N" or "J" for "Next"
      if (event.which == 78 || event.which == 74) { $("#flag_next").click(); }
    });
}
if ( $("#flag_prev").length ) {
  $("#flag_prev").click(function() {
    });
  // Add global keyboard shortcut
  $(document).keydown(function(event) { // "P" or "K" for "Previous"
      if (event.which == 80 || event.which == 75) { $("#flag_prev").click(); }
    });
}
if ( $("#flag_talk").length ) {
  // Click behavior
  $("#flag_talk").click(function() {
      if ( $("div#sidebar_talk").css("display") == "none" ) {
        $("div#sidebar_talk").css("display", "block");
        $("div#content_talk").removeClass("without_sidebar");
        $("div#content_talk").addClass("with_sidebar");
      }
      else {
        $("div#sidebar_talk").css("display", "none");
        $("div#content_talk").addClass("without_sidebar");
        $("div#content_talk").removeClass("with_sidebar");
      }
      fixPositions();
    });
  // Add global keyboard shortcut
  $(document).keydown(function(event) { // "T" for "Talk"
      if (event.which == 84) { $("#flag_talk").click(); }
    });
}
if ( $("#flag_edit").length ) {
  // Click behavior
  $("#flag_edit").click(function() {
      if ( $("div#sidebar_edit").css("display") == "none" ) {
        $("div#sidebar_edit").css("display", "block");
        $("div#content").removeClass("without_sidebar");
        $("div#content").addClass("with_sidebar");
      }
      else {
        $("div#sidebar_edit").css("display", "none");
        $("div#content").addClass("without_sidebar");
        $("div#content").removeClass("with_sidebar");
      }
      fixPositions();
    });
  // Add global keyboard shortcut
  $(document).keydown(function(event) { // "E" for "Edit"
      if ( $("div#sidebar_edit").css("display") == "none" )
      if (event.which == 69) { $("#flag_edit").click(); }
    });
}

  
/* Comment form behavior */
$("div#sidebar aside form textarea").keydown(function(event) {
    event.stopPropagation();
  });
  
  
// Initialization
window.onload = function() {
  fixPositions();
  $("div.matwrap").css("visibility", "visible");
}