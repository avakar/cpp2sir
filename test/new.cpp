void f1()
{
    int * p = new int;
    delete p;
}

void f2()
{
    int * p = new int();
    delete p;
}

void f3()
{
    int * p = new int(42);
    delete p;
}

struct s { s(); ~s(); };

void f4()
{
    s * p = new s;
    delete p;
}

void f5()
{
    s * p = new s[42];
    delete[] p;
}
