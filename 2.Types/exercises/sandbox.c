enum colour { red, green, blue };

int main(void)
{
    enum colour c = 42;

    c = c;

    return 0;
}
