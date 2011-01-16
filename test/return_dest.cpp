struct s
{
    s() throw();
    ~s() throw();
};

int f1()
{
    s a;
    for (;;)
        return 1;
    return 0;
}
