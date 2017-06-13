
void main()
{
    int k;
    int i;
    for(i = 0; i < 100; i++) {
        k = i;
    }

    int j = -100;
    for(i = 0; i <= 100; i++) {
        j = i;
    }
    if(k <= 0) k = 42;
    if(j <= 2 || j >= 50) j = 42;
    if(k / j > 100) {
        k = 10;
        j = 10;
    }
}
