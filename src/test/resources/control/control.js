/**
 *
 * @param a {number}
 * @param b {boolean}
 * @return {number}
 */
function control(a, b) {
    if (x) {
        if (y) doY()
    } else if (z) doZ


    if (b) {
        do {
            a += 1
        } while (!b);
        while (a < 10) {
            a += 2
        }
    } else {
        if (!b) a = 100;
        for (var i=0; i < 3; i++) {
            a += 10
        }
    }
    return a;
}

function f() {
    control(10, true);
    control(15, false);
}