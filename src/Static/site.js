function RefreshImage()
{
    if ($("input[type=text]").toArray().some(x => isNaN(x.value)))
        return;

    var query = $("#preview input").toArray()
                                   .map(x => x.name + "=" + x.value)
                                   .join("&");

    query += "&w=" + /*Math.round($("#mandelbrot").width() - 10)*/ 600;
    query += "&h=" + /*Math.round($("#mandelbrot").height() - 10)*/ 400;
    query += "&s=1";

    window.console.log("/render?" + query);

    $("#render").attr("src", "/render?" + query);
    history.pushState(null, null, "/game?" + query);
}

$(function()
{
    $("input").on('input', RefreshImage);

    $("#generate").click(function(e)
    {
        var s = "ViewPort { ";
        s += "_centerX = " + $("input[name=x]").val() + ", ";
        s += "_centerY = " + $("input[name=y]").val() + ", ";
        s += "_magnification = " + $("input[name=m]").val() + ", ";
        s += "_maxIterations = " + $("input[name=i]").val() + ", ";
        s += "_rotation = 0, ";
        s += "_superScaling = 3 }";
        $("#output").val(s);
    });

    $(window).on('keyup', function(e)
    {
        if (e.target instanceof HTMLInputElement)
            return;

        if (e.which == 37)
        {
            // left
            var currentX = parseFloat($("input[name=x]").val());
            var currentM = parseFloat($("input[name=m]").val());
            var newX = currentX - ((1/8) / currentM);
            $("input[name=x]").val(newX);

            RefreshImage();
        }
        else if (e.which == 38)
        {
            // up
            var currentY = parseFloat($("input[name=y]").val());
            var currentM = parseFloat($("input[name=m]").val());
            var newY = currentY - ((1/8) / currentM);
            $("input[name=y]").val(newY);

            RefreshImage();
        }
        else if (e.which == 39)
        {
            // right
            var currentX = parseFloat($("input[name=x]").val());
            var currentM = parseFloat($("input[name=m]").val());
            var newX = currentX + ((1/8) / currentM);
            $("input[name=x]").val(newX);

            RefreshImage();
        }
        else if (e.which == 40)
        {
            // down
            var currentY = parseFloat($("input[name=y]").val());
            var currentM = parseFloat($("input[name=m]").val());
            var newY = currentY + ((1/8) / currentM);
            $("input[name=y]").val(newY);

            RefreshImage();
        }
        else if (e.which == 81)
        {
            // q
            var currentM = parseFloat($("input[name=m]").val());
            $("input[name=m]").val(currentM * 1.5);
            
            RefreshImage();
        }
        else if (e.which == 65)
        {
            // a
            var currentM = parseFloat($("input[name=m]").val());
            $("input[name=m]").val(Math.max(1, currentM / 1.5));

            RefreshImage();
        }
    });

    RefreshImage();
});