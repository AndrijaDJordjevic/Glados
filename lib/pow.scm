func pow(b e) {
    var result = 1;
    var i = 0;

    while (i < e) {
        result = result * b;
        i = i + 1;
    }
    return result;
}
