method increment_and_check(x: Int): Int
    requires x >= 0
    ensures result == x + 1
{
    var y: Int := x + 1;
    assert y > x;       // This assertion should always hold
    return y
}
