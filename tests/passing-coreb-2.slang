method count_up_to_n(n: Int): Int
    requires n >= 0
    ensures result == n
{
    var i: Int := 0;
    loop
        invariant i >= 0
        invariant i <= n
    {
        i < n =>
            i := i + 1
    };
    assert i == n;

    return i
}
