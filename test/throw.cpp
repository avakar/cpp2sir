void f1()
{
    throw;
}

int f2(int i)
{
    throw 4;
}

int f3(int i)
{
    return i? i: throw 4;
}

void f4(int i)
{
    i? throw 3: throw 4;
}

struct s { s(); s(s const &); ~s(); };

void f5()
{
    throw s();
}
