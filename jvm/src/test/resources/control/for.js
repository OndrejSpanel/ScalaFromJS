// different for loops, some typical, some not

function forShow() {
    var s = 0;

    for (var a = 0; a < 10; a++) yes()

    for (var b = 0; b + 1 < 10; b++) yes()

    for (var c = 0; s < 10; c++) no()

    for (var d = 0; d < 10; d++, s++) no()

    for (var i = 0, x = 2 + 3; i < x; i++) yes()

}