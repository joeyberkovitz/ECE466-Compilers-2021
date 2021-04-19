int printf();

int testGlob;

int func1(){
	int testLoc;
	int arr[5];
	
	testGlob = 1;
	
	testLoc = testGlob + 1;
	
	testGlob = testLoc + 3;
	
	int i;
	while(i < 5){
		arr[i] = i;
		i++;
		printf("Arr[i] = %d\n", arr[i]);
	}
	
	int *ptr;
	
	ptr = &testGlob;
	
	if(*ptr > 0){
		printf("Test global value: %d\n", *ptr);
	}
	else
		printf("Error: test global <= 0\n");
}