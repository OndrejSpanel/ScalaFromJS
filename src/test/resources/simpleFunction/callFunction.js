/**
 * @param first {string}
 * @param last {string}
 * @return string
* */
function concatenate(first, last)
{
    var full;
    full = first + last;
    return full;
}

function secondFunction()
{
    var result;
    result = concatenate('Zara', 'Ali');
    document.write (result );
}