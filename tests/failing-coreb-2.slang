method decrement_below_zero(n: Int): Int
    requires n >= 0
{
    var i: Int := n;
    loop
        // Incorrect invariant, as i will drop below n
        // @CheckError
        invariant i >= n
    {
        i > 0 =>
            i := i - 1
    };
    assert i == 0;

    return i
}
