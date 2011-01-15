int e1() throw();

int f1()
{
    return e1();
}

typedef int (*t1)(int);
t1 e2;

int f2(int i)
{
    return e2(i);
}
