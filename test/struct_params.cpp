struct s
{
    s();
    s(s const &);
    ~s();
};

void f1(s)
{
}

void f2()
{
    s a;
    f1(a);
}

void f3()
{
    s a;
    f1(a);
    f1(a);
}
