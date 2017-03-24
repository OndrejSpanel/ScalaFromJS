function s(a) {

    var b;

    switch (a) {
        case "A": case "B": case "C":
            return "AB";
        case "X":
            console.warn("X");
        case "D":
            return "D";
        case "E": case "F":
            b = a;
            break;
        case "G":
        default:
            b = a;

    }
}
