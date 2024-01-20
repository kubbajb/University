//Dodanie do klasy .text odpowiedniego tekstu
$("document").ready(() => {
    $(".test").text("<p>Ten tekst został dodany za pomocą jQuery do właściwości text</p>");
});

$("document").ready(() => {
    $(".test2").html("<p>Ten tekst został dodany za pomocą jQuery do właściwości html</p>");
});

$(document).ready(function() {
    $("h2").css(
        "font-size","55px"
        );
    $("h2:odd").css(
        'padding', '10px',
        'margin', '5px'
        );
    $('h2:last').css(
        'color', 'green',
        'backgroundColor', 'blue',
        'position', 'fixed',
        'bottom', '0px',
        'left', '45vw',
        'width', '10vw',
        'height', '10vw'
    )

    $("#zamien").click(() => $('#przyczajony').toggleClass('pokolorowany'));
    $("#ukryj").click(() => $('#przyczajony').hide());
    $("#pokaz").click(() => $('#przyczajony').show());

    //Odkomentuj by zobaczyć animację za pomocą jQuery
    $(document).ready(function(){
       animacja();
        setInterval(animacja, 2000);
        function animacja(){
            $('h2:last')
            .animate({left: '35vh'}, 1000)
            .animate({left: '55vh'}, 1000,'linear');
        }
    });

});