function define()
{
    var i, d, s; // assigned once
    var ii, di, aa; // assigned multiple times,
    i = 1;
    d = 1.2;
    s = "str";
    ii = 1; // Int + Int => Int
    ii = 2;
    di = 1.2; // Double + Int => Double
    di = 3;
    aa = 1; // Int + String => Any
    aa = "String"
}

/**
 * @param first {string}
 * @param last {string}
 * @return {string}
 * */
function concatenate(first, last)
{
    var full;
    if (first != "") {
        full = first + last;
    } else {
        full = last;
    }
    return full;
}

function secondFunction()
{
    var x;
    if (true) {
        x = concatenate("Bad","Bob")
    }
}
