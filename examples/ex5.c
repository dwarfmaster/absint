
int i = 0;
int j = i+1;

void main()
{
    int k;
    int first = 0;
    goto skipping;

back:
    while(k + j * i <= 42) {
        i += 2 * j;
        j ++;
        k += i / j;
    }
    first = 1;
    goto again;

skipping:
    k = i + j;
again:
    k = 2 * k + 1;
    if(k <= 3) goto again;
    if(first == 0) goto back;
}
