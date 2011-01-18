struct s { s(); ~s(); };

void e1();
void e2(int) throw();
void e3() throw();

void f0()
{
}

void f1()
{
    try
    {
    }
    catch (int)
    {
    }
}

void f2()
{
    try
    {
        e1();
    }
    catch (int e)
    {
    }
}

void f3()
{
    try
    {
        e1();
    }
    catch (int & e)
    {
        e2(e);
    }
}

int f4()
{
    try
    {
        throw 42;
    }
    catch (long & e)
    {
        e2(e);
    }
    catch (...)
    {
        e3();
    }

    return 2;
}

int f5()
{
    try
    {
        throw 42;
    }
    catch (long &)
    {
        e1();
    }

    return 2;
}
