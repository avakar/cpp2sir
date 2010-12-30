struct s1
{
    virtual void f1();
    virtual void f2() = 0;
};

struct s2
{
    void f1();
    void f2();
};

void f2(s1 * p)
{
    p->f1();
    p->s1::f1();
    p->f2();
}
