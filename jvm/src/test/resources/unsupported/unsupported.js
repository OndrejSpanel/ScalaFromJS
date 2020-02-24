function d() {
    var a = 123;
    debuggger;

    for (var x = 0; x < 3; x++) {
        if (x == 1) break;
    }
    return a
}