
int i = 0;
int j = i+1;

int f(int x) {
    x *= 2;
    return x + 1;
}

void main()
{
  int k = i + j;
again:
  k = 2 * k + 1;
  if(k <= 3) goto again;
}
