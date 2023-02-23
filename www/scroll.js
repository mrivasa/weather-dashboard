$(document).ready(function() {
    $("#show_data").change(function() {
        if($(this).is(":checked")){
            $("html, body").animate({
                scrollTop: $(
                  'html, body').get(0).scrollHeight
            }, 1000);
        }
    });
});