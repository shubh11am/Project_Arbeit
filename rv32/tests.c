// This file belongs to the bachelor thesis:
// "Stack Frame Allocation inside a Compiler for the RISC-V Processor Architecture"
// and includes all the manual tests described in chapter 4
// The resulting output is found in the "tests.s" file in this folder

// Return nothing with no function arguments
void returnVoid(){
  return;
}

// Return integer with no function arguments
int returnInt(){
  return 0;
}

// Register an stack function arguments
int returnInt10Arguments(int a1,int a2,int a3,int a4,int a5,int a6,int a7,int a8,int a9,int a10){
  return 0;
}

// Force spill by using function arguments again after another function call
int add(int a1,int a2,int a3,int a4,int a5,int a6,int a7,int a8,int a9,int a10){
  int r = returnInt10Arguments(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
  int b = 1;
  b = b + a1;
  b = b + a2;
  b = b + a3;
  b = b + a4;
  b = b + a5;
  b = b + a6;
  b = b + a7;
  b = b + a8;
  b = b + a9;
  b = b + a10;
  return b;
}

// Recursive function
int fakulaet(int x){
  if(x <= 1)
    return 1;
  return x * fakulaet(x-1);
}

// Global variables
int A = 4711;
int B;

int main(){
  // Function calls
  returnVoid();
  int a = returnInt();
  int c = returnInt10Arguments(1,2,3,4,5,6,7,8,9,10);
  int d = add(1,2,3,4,5,6,7,8,9,10);

  // Global variables
  int e = A;
  B = e;

  return 0;
}
