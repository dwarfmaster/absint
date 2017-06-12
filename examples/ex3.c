
int i = 0;
int j = i+1;

int f(int x) {
    x *= 2;
    return x + 1;
}

void main()
{
  int k = i + j;
  k = f(k);
  k = f(k);
}
