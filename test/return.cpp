void e1() throw();

void f1(int cond)
{
    if (cond)
        return;
    e1();
}

int f2(int cond)
{
    if (cond)
        return 1;
    else
        return 0;
}

void f3()
{
}

struct s
{
    s() throw();
    ~s() throw();
};

int f4()
{
    s a;
    for (;;)
        return 1;
    return 0;
}
