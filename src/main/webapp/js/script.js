/* Author: Beyond the Stage
  Something
*/

/****************************************************************************/
/* Utility */
function clamp(val, min, max) { return Math.max(min, Math.min(max, val)); }

/****************************************************************************/
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
    
    // Force sidebars to be at least half the browser window height
    $.map($("div.sidebar"), function(e, i) {
      $(e).css("min-height", display_area_height - parseInt($(e).css("padding-bottom")) - 2 );
    });
    
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

/****************************************************************************/
/* Add functionality to tabs */
if ( $("#flag_next").length ) {
  $("#flag_next").click(function() {
      window.location = $(this).attr("href");
    });
  // Add global keyboard shortcut
  $(document).keydown(function(event) { // "N" or "J" for "Next"
      if (event.which == 78 || event.which == 74) { $("#flag_next").click(); }
    });
}
if ( $("#flag_prev").length ) {
  $("#flag_prev").click(function() {
      window.location = $(this).attr("href");
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
        $("div#sidebar_edit").css("display", "none");
        $("div#content").removeClass("without_sidebar");
        $("div#content").addClass("with_sidebar");
        $("#flag_talk").attr("href", "#talk");
        updateNavAnchors("#talk");
      }
      else {
        $("div#sidebar_talk").css("display", "none");
        $("div#content").addClass("without_sidebar");
        $("div#content").removeClass("with_sidebar");
        $("#flag_talk").attr("href", "#");
        updateNavAnchors("#");
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
  $("#flag_edit").click(function(event) {
      if ( $("div#sidebar_edit").css("display") == "none" ) {
        $("div#sidebar_edit").css("display", "block");
        $("div#sidebar_talk").css("display", "none");
        $("div#content").removeClass("without_sidebar");
        $("div#content").addClass("with_sidebar");
        $("#flag_edit").attr("href", "#edit");
        updateNavAnchors("#edit");
      }
      else {
        $("div#sidebar_edit").css("display", "none");
        $("div#content").addClass("without_sidebar");
        $("div#content").removeClass("with_sidebar");
        $("#flag_edit").attr("href", "#");
        updateNavAnchors("#");
      }
      fixPositions();
    });
  // Add global keyboard shortcut
  $(document).keydown(function(event) { // "E" for "Edit"
      if (event.which == 69) { $("#flag_edit").click(); }
    });
}

// Update next/previous hrefs to match page anchors
var updateNavAnchors = function(newHash) {
    if ( $("#flag_next").length ) {
      var nextHref = $("#flag_next").attr("href").replace(/#.*/,'').concat(newHash);
      $("#flag_next").attr("href", nextHref);
      }
    
    if ( $("#flag_prev").length ) {
      var prevHref = $("#flag_prev").attr("href").replace(/#.*/,'').concat(newHash);
      $("#flag_prev").attr("href", prevHref);
      }
  };

/****************************************************************************/
/* Comment form behavior */
// By default, stop event propagation on all text fields
$.map($("input"), function(e, i) {
    if ( $(e).prop("type") == "text" ) { $(e).keydown(function(event) { event.stopPropagation(); }); }
  });
$.map($("textarea"), function(e, i) {
    $(e).keydown(function(event) { event.stopPropagation(); });
  });

/* Default text in text fields */
var defaultTextFocus = function(event) {
    if ( $(this).val() == $(this).prop("title") ) {
      $(this).removeClass("default_text_active");
      $(this).val("");
    }
  }
var defaultTextBlur = function(event) {
    if ( $(this).val() == "" ) {
      $(this).addClass("default_text_active");
      $(this).val($(this).prop("title"));
    }
  }

/* Iterate through form on submit, clear fields
   where value=title? */
var defaultTextFormFix = function(event) {
  $(this).find("default_text").map(function() {
      if ( $(this).val() == $(this).prop("title") )
        $(this).val("");
    });
}

/* Select on click in text fields */
var selectTextFocus = function(event) {
    if ( $(this).val() != $(this).prop("title") ) {
      $(this).select();
    }
  }
var selectTextMouseUp = function(event) {
  event.preventDefault();
}

/* Comment box special behavior */
var commentBoxKeyDown = function(event) {
    if ( event.keyCode == 13 ) { // Enter Key
      if ( ! event.shiftKey ) { // Without Shift
        event.preventDefault();
        $("form#comment-field").submit();
      }
    }
  }

/****************************************************************************/
/* Page Initialization */
window.onload = function() {

  // Auto-append classes onto certain fields
  $.map($("input"), function(e, i) {
      if ( $(e).prop("type") == "text" ) { $(e).addClass("text"); }
      else if ( $(e).prop("type") == "file" ) { $(e).addClass("file"); }
      else if ( $(e).prop("type") == "button" ) { $(e).addClass("button"); }
      else if ( $(e).prop("type") == "submit" ) { $(e).addClass("button"); }
    });
  $.map($("textarea"), function(e, i) {
      $(e).addClass("textarea");
    });
    
  // Establish text-replace behavior on default_text fields
  $(".select_text").focus( selectTextFocus );
  $(".select_text").mouseup( selectTextMouseUp );
  $(".default_text").focus( defaultTextFocus );
  $(".default_text").blur( defaultTextBlur );
  $(".default_text").blur();
  $("form").submit(defaultTextFormFix);
  
  // Establish comment box behaviors
  $(".comment_box").keydown( commentBoxKeyDown );

  // Fix up element positions based on window size
  fixPositions();
  $("div.matwrap").css("visibility", "visible");
  
  // Default tab opening
  if ( $("#flag_talk").length && $(location).prop("hash") == "#talk" )
    $("#flag_talk").click();
  else if ( $("#flag_edit").length && $(location).prop("hash") == "#edit" )
    $("#flag_edit").click();

  // Activate datepicker
  if ( $(".apdate").datepicker )
    $(".apdate").datepicker({dateFormat: "yy/mm/dd", changeMonth: true, changeYear: true, defaultDate: "1972/05/01"})
    
  // Match nav to sidebar state
  updateNavAnchors(window.location.hash);
}
