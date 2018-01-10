$(document).ready(function() {

    $chunks = $('.hiddensolution');
    $chunks.wrap('<div class=\"hiddensolutioncont\"></div>');
    $chunks.before("<div class=\"showopt\">Show Solution</div><br style=\"line-height:22px;\">");
    $chunks.find('pre.r').find("code").wrap('<span class="foldedloc"></span>');
    $chunks.find('pre:not(.r)').find("code").wrap('<span class="foldedloc"></span>');
    $chunks.find("img").wrap('<span class="foldedloc"></span>');
    $chunks.find("table").wrap('<span class="foldedloc"></span>');
    $chunks.find(".html-widget").wrap('<span class="foldedloc"></span>');


    $chunks = $('.hiddencode');
    $chunks.wrap('<div class=\"hiddencodecont\"></div>');
    $chunks.before("<div class=\"showopt\">Show Code</div><br style=\"line-height:22px;\">");
    $chunks.find('pre.r').find("code").wrap('<span class="foldedloc"></span>');

    $chunks = $('.hiddenoutput');
    $chunks.wrap('<div class=\"hiddencodecont\"></div>');
    $chunks.before("<div class=\"showopt\">Show Output</div><br style=\"line-height:22px;\">");
    $chunks.find('pre:not(.r)').find("code").wrap('<span class="foldedloc"></span>');
    $chunks.find("img").wrap('<span class="foldedloc"></span>');
    $chunks.find("table").wrap('<span class="foldedloc"></span>');
    $chunks.find(".html-widget").wrap('<span class="foldedloc"></span>');

  // hide all chunks when document is loaded
    $('.foldedloc').css('display', 'none')

  // function to toggle the visibility
  $('.showopt').click(function() {
    var label = $(this).html();
    if (label.indexOf("Show") >= 0) {
      $(this).html(label.replace("Show", "Hide"));
    } else {
      $(this).html(label.replace("Hide", "Show"));
    }
      $(this).siblings().find('.foldedloc').slideToggle('fast','swing')
  });

});
