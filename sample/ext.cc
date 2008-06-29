/////////////////////////////////////////
//   Sample for Embedding C++ into R   //
/////////////////////////////////////////
//
// I keep forgetting how to do this so here's an example.  
// If you're willing to use R's most primitive FFI interface, 
// things are incredibly simple and easy to get going.
//      -Brendan O'Connor (brenocon@gmail.com)
//
// Compile with, e.g. on mac:  
//   $ g++ -dynamic -bundle -I/Library/Frameworks/R.framework/Resources/include ext.cc -o ext.dylib
// And from R:
//   > dyn.load("ext.dylib")
//   > .C("hi")
//   Hello Earth
//   list()
//
// .C() is very low level. To pass data in and out, you get raw pointers
// to the internal vectors that R itself uses.  (or copies thereof.  or something.)
//
//   > .C("mul_2", as.integer(1:3), as.integer(3), as.integer(rep(-1,3)))
//   [[1]]
//   [1] 1 2 3
// 
//   [[2]]
//   [1] 3
// 
//   [[3]]
//   [1] 2 4 6
//
// Documentation: http://cran.r-project.org/doc/manuals/R-exts.html#Interface-functions-_002eC-and-_002eFortran


#include <R.h>
#include <iostream>
using namespace std;

extern "C" {

void hi()
{
  cout << "Hello Earth" << endl;
}

void mul_2(int* numbers, int* n, int* output)
{
  for (int i=0; i < *n; i++) {
    output[i] = numbers[i] * 2;
  }
}


}
