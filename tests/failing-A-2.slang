method decrement_and_check(x: Int): Int
    requires x >= 0
    ensures result == x - 1
{
    var y: Int := x - 1;
    // @CheckError
    assert y >= 0;       // This assertion fails when x = 0
    return y
}
