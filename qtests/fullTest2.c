int printf();

int f(int a, int *b){
    a = 6;
    b = &a;
    return a;
}

int main(){
    int a, b, c, d, e, arr[10];
    a = 3;
    b = 4;
    c = (arr+7) - (arr+2);

    b /= a;
    d = c % a;

    a = a && b;
    printf("a = %d\n",a);

    e = f(a,arr);
    printf("e = %d\n",e);
}