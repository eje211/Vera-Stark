$(document).ready(function() {
  function submitFormToIframe($form, target) {
    $form
      .attr('target', target)
      .removeAttr('onsubmit')
      .attr('action', '/ajax_request/' + lift_page)
      .attr('method', 'post')
      .attr('enctype', 'multipart/form-data' )    
      .attr('encoding', 'multipart/form-data')
      .find('input:submit,button[type=submit]')
        .removeAttr('onclick')
      .end()
      .append($('<input type="hidden" name="' +
                  $form.find('input:submit').attr('name') +
                  '" value="_" />'))
      .after(
        // do not use attr() to set name. IE7 will hate this: http://stackoverflow.com/questions/2105815/weird-behaviour-of-iframe-name-attribute-set-by-jquery-in-ie
        $('<iframe id="' + target + '" name="' + target + '" />')
          .addClass('form-target')
          .css('display','none')
          .load(function() {
              $.globalEval($(this).contents().text());
          })
      );
  }
});

  // submitFormToIframe($('form:has(input[type=file])'), '/artifact/new

  /*
  $("#new_artifact").fileupload({dataType:'json', url:'/artifact/new', dropZone:$('#image'),
  done:function(e, data){$("#image").html("<img src='serving/" + data.result.name + "' height='480' width='640' />");}
  });
  */

/*
 * jQuery File Upload Plugin JS Example 6.5.1
 * https://github.com/blueimp/jQuery-File-Upload
 *
 * Copyright 2010, Sebastian Tschan
 * https://blueimp.net
 *
 * Licensed under the MIT license:
 * http://www.opensource.org/licenses/MIT
 */

/*jslint nomen: true, unparam: true, regexp: true */
/*global $, window, document */

$(function () {
    'use strict';

    // Initialize the jQuery File Upload widget:
    $('#new_artifact').fileupload();

    // Enable iframe cross-domain access via redirect option:
    $('#new_artifact').fileupload(
        'option',
        'redirect',
        window.location.href.replace(
            /\/[^\/]*$/,
            '/cors/result.html?%s'
        )
    );

    $('#new_artifact').each(function () {
        var that = this;
        $.getJSON(this.action, function (result) {
            if (result && result.length) {
                $(that).fileupload('option', 'done')
                    .call(that, null, {result: result});
            }
        });
    });

});

