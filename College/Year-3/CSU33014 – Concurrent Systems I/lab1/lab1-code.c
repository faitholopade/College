//
// CSU33014 Lab 1
//

// Please examine version each of the following routines with names
// starting lab1. Where the routine can be vectorized, please
// complete the corresponding vectorized routine using SSE vector
// intrinsics.

// Note the restrict qualifier in C indicates that "only the pointer
// itself or a value directly derived from it (such as pointer + 1)
// will be used to access the object to which it points".


#include <immintrin.h>
#include <stdio.h>

#include "lab1-code.h"

/****************  routine 0 *******************/

// Here is an example routine that should be vectorized
void lab1_routine0(float * restrict a, float * restrict b,
		    float * restrict c) {
  for (int i = 0; i < 1024; i++ ) {
    a[i] = b[i] * c[i];
  }
}

// here is a vectorized solution for the example above
void lab1_vectorized0(float * restrict a, float * restrict b,
		    float * restrict c) {
  __m128 a4, b4, c4;
  
  for (int i = 0; i < 1024; i = i+4 ) {
    b4 = _mm_loadu_ps(&b[i]);
    c4 = _mm_loadu_ps(&c[i]);
    a4 = _mm_mul_ps(b4, c4);
    _mm_storeu_ps(&a[i], a4);
  }
}

/***************** routine 1 *********************/

// in the following, size can have any positive value
float lab1_routine1(float * restrict a, float * restrict b,
		     int size) {
  float sum = 0.0;
  
  for ( int i = 0; i < size; i++ ) {
    sum = sum + a[i] * b[i];
  }
  return sum;
}

// insert vectorized code for routine1 here
float lab1_vectorized1(float * restrict a, float * restrict b,
		     int size) {
    __m128 sum4 = _mm_setzero_ps();
    float sum = 0.0;
    
    int i;
    // Process in chunks of 4
    for (i = 0; i < size - (size % 4); i += 4) {
        __m128 a4 = _mm_loadu_ps(&a[i]); // Load 4 elements from a
        __m128 b4 = _mm_loadu_ps(&b[i]); // Load 4 elements from b
        __m128 prod4 = _mm_mul_ps(a4, b4); // Element-wise multiply 
        sum4 = _mm_add_ps(sum4, prod4); //Accumulate results 
    }
    
    // Handle remaining elements 
    for (; i < size; i++) {
        sum += a[i] * b[i];
    }
    
    // Accumulation of the vector elements
    float temp[4];
    _mm_storeu_ps(temp, sum4);// Store the results of vector operations to a temporary array
    for (i = 0; i < 4; i++) { // Sum up the elements 
        sum += temp[i];  
    }
    return sum;
}

/******************* routine 2 ***********************/

// in the following, size can have any positive value
void lab1_routine2(float * restrict a, float * restrict b, int size) {
  for ( int i = 0; i < size; i++ ) {
    a[i] = 1 - (1.0/(b[i]+1.0));
  }
}

// in the following, size can have any positive value
void lab1_vectorized2(float * restrict a, float * restrict b, int size) {
    __m128 one4 = _mm_set1_ps(1.0f); // Used for both adding to b[i] and subtracting from 1
    int i;
    for (i = 0; i < size - 4; i += 4) {
        __m128 b4 = _mm_loadu_ps(&b[i]); // Load 4 elements from b
        __m128 denom4 = _mm_add_ps(b4, one4); // Compute b[i] + 1.0 for each element
        __m128 div4 = _mm_div_ps(one4, denom4); // Compute 1.0 / (b[i] + 1.0) for each element
        __m128 result4 = _mm_sub_ps(one4, div4); // Subtract from 1.0 for each element
        _mm_storeu_ps(&a[i], result4); // Store the result back into a
    }
    // Handle remaining elements
    for (i=i; i < size; i++) {
        a[i] = 1 - (1.0 / (b[i] + 1.0));
    }
}

/******************** routine 3 ************************/

// in the following, size can have any positive value
void lab1_routine3(float * restrict a, float * restrict b, int size) {
  for ( int i = 0; i < size; i++ ) {
    if ( a[i] < 0.0 ) {
      a[i] = b[i];
    }
  }
}

// in the following, size can have any positive value
void lab1_vectorized3(float * restrict a, float * restrict b, int size) {
    __m128 zero = _mm_setzero_ps(); // Vector of zeros for comparison
    int i;
    for (i = 0; i < (size - (size % 4)); i += 4) {
        __m128 a4 = _mm_loadu_ps(&a[i]);
        __m128 b4 = _mm_loadu_ps(&b[i]);
        __m128 cmpMask = _mm_cmplt_ps(a4, zero); // Compare a4 < 0.0

        // Simulate blend operation using bitwise operations
        __m128 andResult = _mm_and_ps(cmpMask, b4); // True elements from b
        __m128 andnotResult = _mm_andnot_ps(cmpMask, a4); // False elements from a
        __m128 blendResult = _mm_or_ps(andResult, andnotResult); // Combine
        
        _mm_storeu_ps(&a[i], blendResult);
    }
    // Handle remaining elements
    for (; i < size; i++) {
        if (a[i] < 0.0) a[i] = b[i];
    }
}

/********************* routine 4 ***********************/

// hint: one way to vectorize the following code might use
// vector shuffle operations
void lab1_routine4(float * restrict a, float * restrict b,
		       float * restrict c) {
  for ( int i = 0; i < 2048; i = i+2  ) {
    a[i] = b[i]*c[i] - b[i+1]*c[i+1];
    a[i+1] = b[i]*c[i+1] + b[i+1]*c[i];
  }
}

void lab1_vectorized4(float * restrict a, float * restrict b, float * restrict c) {
    int i;
    for (i = 0; i < 2048; i += 4) {
    // Load 4 elements from b and c
		__m128 b4 = _mm_loadu_ps(b + i);
		__m128 c4 = _mm_loadu_ps(c + i);

    // Prepare vectors for complex mul tiplication using shuffling
		__m128 b4_shuffled1 = _mm_shuffle_ps(b4, b4, _MM_SHUFFLE(2,2,0,0)); // [b[i], b[i], b[i+2], b[i+2]]
		__m128 b4_shuffled2 = _mm_shuffle_ps(b4, b4, _MM_SHUFFLE(3,3,1,1)); // [b[i+1], b[i+1], b[i+3], b[i+3]]
		__m128 c4_shuffled = _mm_shuffle_ps(c4, c4, _MM_SHUFFLE(2,3,0,1)); // [c[i+1], c[i], c[i+3], c[i+2]]

    // Perform complex multiplication
		__m128 prod1 = _mm_mul_ps(b4_shuffled1,  c4); // [b[i]*c[i], b[i]*c[i+1], b[i+2]*c[i+2], b[i+2]*c[i+3]]
		__m128 prod2 = _mm_mul_ps(b4_shuffled2, c4_shuffled); // [b[i+1]*c[i+1], b[i+1]*c[i], b[i+3]*c[i+3], b[i+3]*c[i+2]]

		// Subtract and add for real and imaginary parts
    __m128 a4 = _mm_addsub_ps(prod1, prod2); // [b[i]*c[i] - b[i+1]*c[i+1], b[i]*c[i+1] + b[i+1]*c[i], b[i+2]*c[i+2] - b[i+3]*c[i+3], b[i+2]*c[i+3] + b[i+3]*c[i+2]]

    // Store the result back into a
		_mm_storeu_ps(a + i, a4);
	}
}


/********************* routine 5 ***********************/

// in the following, size can have any positive value
void lab1_routine5(unsigned char * restrict a,
		    unsigned char * restrict b, int size) {
  for ( int i = 0; i < size; i++ ) {
    a[i] = b[i];
  }
}

void lab1_vectorized5(unsigned char * restrict a, unsigned char * restrict b, int size) {
    int i;
    // Process 16 bytes at a time
    for (i = 0; i < size - 15; i += 16) {
        // Load 16 bytes from b into a 128-bit register
        __m128i b16 = _mm_loadu_si128((__m128i*)&b[i]);
        // Store the 128-bit register to a
        _mm_storeu_si128((__m128i*)&a[i], b16);
    }
    // Handle the remainder of the array
    for (; i < size; i++) {
        a[i] = b[i];
    }
}


/********************* routine 6 ***********************/

void lab1_routine6(float * restrict a, float * restrict b,
		       float * restrict c) {
  a[0] = 0.0;
  for ( int i = 1; i < 1023; i++ ) {
    float sum = 0.0;
    for ( int j = 0; j < 3; j++ ) {
      sum = sum +  b[i+j-1] * c[j];
    }
    a[i] = sum;
  }
  a[1023] = 0.0;
}

//source: vector programming examples from blackboard
void lab1_vectorized6(float * restrict a, float * restrict b,
		       float * restrict c) {
  a[0] = 0.0;
	__m128 c0_4 = _mm_load1_ps(&c[0]);
	__m128 c1_4 = _mm_load1_ps(&c[1]);
	__m128 c2_4 = _mm_load1_ps(&c[2]);
	int i;
  	for ( i = 1; i < 1023 - 3; i+= 4) {
		__m128 b0to3 = _mm_loadu_ps(&b[i-1]);
		__m128 b1to4 = _mm_loadu_ps(&b[i]);
		__m128 b2to5 = _mm_loadu_ps(&b[i+1]);
		__m128 product0 = _mm_mul_ps(c0_4, b0to3);
		__m128 product1 = _mm_mul_ps(c1_4, b1to4);
		__m128 product2 = _mm_mul_ps(c2_4, b2to5);
		__m128 sum4 = _mm_add_ps(product0, product1);
		sum4 = _mm_add_ps(sum4, product2);
		_mm_storeu_ps(&(a[i]), sum4);
  	}
  	for (  i = i  ; i < 1023; i++ ) {
    		float sum = 0.0;
    		sum = sum +  b[i-1] * c[0];
    		sum = sum +  b[i] * c[1];
    		sum = sum +  b[i+1] * c[2];
    		a[i] = sum;
  	}
  	a[1023] = 0.0;
          
}



