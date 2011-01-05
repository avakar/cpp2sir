struct s { s(); ~s(); };

void e1();
void e2(int);
void e3();

void f1()
{
    throw 1;
}

void f2()
{
    throw s();
}

void f3()
{
    try
    {
        e1();
    }
    /*catch (int & e)
    {
        e2(e);
    }*/
    catch (long)
    {
        e3();
    }
    catch (...)
    {
        e3();
    }
}

int f4()
{
    try
    {
        return 42;
    }
    catch (int e)
    {
    }
    
    return 2;
}
