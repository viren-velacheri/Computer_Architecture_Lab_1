/*
 * CS:APP Data Lab
 *
 * <Viren Velacheri    vv6898> Note: used one slip day.
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:

  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code
  must conform to the following style:

  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>

  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.


  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 *
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* We do not support C11 <threads.h>.  */
//1
/*
 * bitXor - x^y using only ~ and &
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y) {
  /* Got this using the definition xor's definition and De Morgan's Law */
  return ~(y & x) & ~(~y & ~x);
}

/*
 * TMax - return maximum two's complement integer
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmax(void) {
  /* Just thought logically that since the inverse of the largest negative
  number or  TMin would give TMax. */
  return ~(1 << 31);
}

//2
/*
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m) {
   /* The basic idea was to extract the individual bytes by using shifts
   and then after using a mask (0xFF) to extract them, used the mask as well
   for zeroing out swtiching bits so could just use or to concatenate result */
    // This mask is used for extracting the specific bytes I want.
    int mask = 0xFF;
    // These shifts below are equivalent to 8 times byte number to get to
    // place in number where byte is located.
    int nShift = n << 3;
    int mShift = m << 3;
    // The number is shifted down by the number of calculated bits and then
   // & it with mask to isolate and get the wanted byte.
    int nByte = (x >> nShift) & mask;
    int mByte = (x >> mShift) & mask;
    // Now shifted the isolated byte down to other place byte was. In other
    // words shift with the other shift.
    int nSwitch = nByte << mShift;
    int mSwitch = mByte << nShift;
    // This puts masks in positiion to be used in maintaining original number
    int flip = ~((mask << mShift) | (mask << nShift));
    // gets back number without swapped positions.
    int nonNthBytes = flip & x;
    // these set of ors sort of concatenate this together to get
    // necessary result
    return nonNthBytes | nSwitch | mSwitch;
}

/*
 * isPositive - return 1 if x > 0, return 0 otherwise
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int isPositive(int x) {
   /* Broke down problem into finding sign and then seeing whether number
   itself is zero or not. Use an & to check both conditions in end*/
    // This extracts the sign of the number by right shifting the number
    // 31 bits down. I then inverse so that if number is negative, it becomes
    // all zeros and resulting number is 0. Likewise if number is positive,
    // I invert number and single zero becomes a 1.
    int signShift = 31;
    int sign = ~(x >> signShift);
    // The edge case is when x itself it zero. In this case, I check to make
    // sure number is a nonzero number. If not, that is why I and it with sign
    // so zero is still returned.is a nonzero number. If not, that is why I and
    // it with sign so zero is still returned.
    int notZero = !!x;
    return sign & notZero;
}

/*
 * sign - return 1 if positive, 0 if zero, and -1 if negative
 *  Examples: sign(130) = 1
 *            sign(-23) = -1
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 10
 *  Rating: 2
 */
int sign(int x) {
    // In this case don't have to invert it as signs correspond to appropriate
    // output. Pretty similar to above method except due to different return
    // output, use or in return rather than and.
    int signShift = 31;
    int sign = x >> signShift;
    // The notZero is the same idea is with above method
    int notZero = !!x;
    return (sign | notZero);
}

/*
 * allOddBits - return 1 if all odd-numbered bits in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   Examples allOddBits(0xFFFFFFFD) = 0, allOddBits(0xAAAAAAAA) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int allOddBits(int x) {
  /* Use the mask of 0xAAAAAAAA for determining if all odd-numbered bits in
  word are 1 or not pretty much. We compare that with x appropriately in return
  statement */
  // the base is this hex below as it is the one with all odd numbered digits
  // in word set to one.
  int base = 0xAA;
  int singleByteShift = 8;
  int doubleByteShift = 16;
  int invertedMask;
  // This makes the base become 0xAAAA
  int mask = base | (base << singleByteShift);
  // this last part makes mask become final form of 0xAAAAAAAA
  mask = mask + (mask << doubleByteShift);
  // This return uses mask to determine where basically unless x has numbers
  // in all odd places of word, it will return 0 and 1 if it does.
  invertedMask = ~mask + 1; // twos negation complement of mask
  return !((x & mask) + invertedMask);
}

//3
/*
 * replaceByte(x,n,c) - Replace byte n in x with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */
int replaceByte(int x, int n, int c) {
  /* Shifting of Bytes to other method where we were switching bytes.
  Except instead of swapping bytes within, we just sub out selected byte and
  put in needed byte */
  // This base is used for masking the byte to be replaced so that new byte
  // can be subbed in.
  int base = 0xFF;
  // Like in above methods, byte number times 8 or left shifting 8 gets you to
  // position of this byte.
  int nShift = n << 3;
  // Shift mask by that number of bits gets you position of old byte. Use
  // bit wise not after to set up anding in the return statement so only
  // old byte is removed.
  int mask = ~(base << nShift);
  // the prospective byte is moved into position.
  int newByte = c << nShift;
  // This is what changes x to where spot of old byte is two 0's. The rest of
  // x remains intact.
  int changedX = x & mask;
  // This or acts as a way of concatenating the two pieces to get result.
  return changedX | newByte;
}

/*
 * isGreater - if x > y  then return 1, else return 0
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y) {
    // Extract the signs of respective x and y values by shifting right 31
    // bits.
    int signShift = 31;
    int sign_x = x >> signShift;
    int sign_y = y >> signShift;
    // This variable is used to check whether signs are same and also which
    // number is bigger.
    int equal = !((sign_x & ~sign_y) | (~sign_x & sign_y)) & (~y + x >> signShift);
    // This variable checks to see whether signs are not equal.
    int notEqual = sign_x & !sign_y;
    // the nots of these anded to get result.
    return !equal & !notEqual;
}

/*
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int logicalShift(int x, int n) {
  /* The idea is to shift down x by n and then try to use the base in turning
  those ones that come up with a negative number into zeroes. */
  // First just right shift down x by n.
  int xShifted = x >> n;
  // the below number is the basis for the masking. 1 followed by 7 zeroes.
  int base = 0x80;
  // base is left shifted down to end and then right shifted by n
  int shiftToEnd = 24;
  int adjustment = 1;
  int mask = ~(((base << shiftToEnd) >> n) << adjustment);
  // Conduct & operation to eliminate ones at the beginning.
  return xShifted & mask;
}

/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x80000001) = 0x80000000 (saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {
  // This is used in shifting the corresponding base to get needed mask.
  int maskShift = 24;
  int base = 0x80; // used as base for shiting to get 0
  int mask = base << maskShift; // This gets 0
  int maxRightShift = 31;
  int fillSign = (x >> maxRightShift); // This gets sign of x.
  int maxSign = (x << 1) >> maxRightShift;
  int doubleX = x << 1; // This doubles x.
  // A lot of these below checks are done to ensure that if overflow occurs,
  // either Tmin or Tmax is returned.
  return doubleX ^ (fillSign ^ maxSign) & (doubleX ^ maxSign ^ mask);
}

//4
/*
 * bitReverse - Reverse bits in a 32-bit word
 *   Examples: bitReverse(0x80000002) = 0x40000001
 *             bitReverse(0x89ABCDEF) = 0xF7D3D591
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 45
 *   Rating: 4

 Used this as reference for approaching the problem:
 https://graphics.stanford.edu/~seander/bithacks.html#ReverseByteWith32Bits.
 */
int bitReverse(int x) {
  /* So the idea is to first swap the odd and even bits. Then swap those
  consecutive pairs, then the nybbles(4 bit pairs), bytes, and then lastly the
  2 byte pairs of bits*/
  // The below two variables are for the shifts that are used for the creation
  // of the masks as well as for the manipulating and swapping of the bits in
  // the x value themselves.
  int byteShift = 8;
  int doubleByteShift = 16;
  // The below bases are the initial part used for the creation of the masks
  // for the swapping of the bits within the x.
  int base1 = 0x55;
  int base2 = 0x33;
  int base3 = 0x0F;
  int base4 = 0xFF;
  // Below are the masks used for the swapping of even and odd bits, then the
  // consecutive pairs, nybbles, bytes, and the 16 bit pairs. The cumulation
  // of this swapping leads to the reverse form of x.
  int mask1 = (base1 << byteShift) | base1;
  int mask2 = (base2 << byteShift) | base2;
  int mask3 = (base3 << byteShift) | base3;
  int mask4 = (base4 << doubleByteShift) | base4;
  int mask5 = (base4 <<  byteShift) | base4;
  // This variable is used to temporarily store the x value that is being
  // altered. Just taking the approach that I don't want to modify the
  // parameter value directly even though it is an int.
  int temp = x;
  mask1 = (mask1 << doubleByteShift) | mask1;
  mask2 = (mask2 << doubleByteShift) | mask2;
  mask3 = (mask3 << doubleByteShift) | mask3;
  temp = ((temp >> 1) & mask1) | ((temp & mask1) << 1);
  temp = ((temp >> 2) & mask2) | ((temp & mask2) << 2);
  temp = ((temp >> 4) & mask3) | ((temp & mask3) << 4);
  temp = ((temp >> byteShift) & mask4) | ((temp & mask4) << byteShift);
  temp = ((temp >> doubleByteShift) & mask5) | (temp << doubleByteShift);
  return temp;
}

/*
 * absVal - absolute value of x
 *   Example: absVal(-1) = 1.
 *   You may assume -TMax <= x <= TMax
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4
 */
int absVal(int x) {
  /* the idea is to if number is negative to cancel out that one and make it
  positive. If not negative, should remain the same.*/
  int signShift = 31;
  // This mask pretty much gets the sign of number
  int mask = x >> signShift;
  // The above I realized is equivalent to just doing Xor so it could be
  // simplified but since this still fits in the constraints of number of
  // operations, I just kept it. Basically the below return statement gets
  // the unsigned version of a negative number and if number ain't negative
  // it just returns that unchanged number.
  return (~(x + mask) & mask) | ((x + mask) & ~mask);
}

//float
/*
 * floatAbsVal - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned floatAbsVal(unsigned uf) {
   unsigned absVal, exponent, frac;
   int mask = 0x7FFFFF;
   int signShift = 31;
   int exponentShift = 23;
   // This constant is used for extracting exponent part of unsigned float and
   // also for the check of whether number if NaN or not.
   int expBaseExtract = 0xFF;
   // The below variable helps in determining the absolute value of the
   // unsigned float
   absVal = ~(1 << signShift) & uf;
   // This extracts the exponent part of unsigned number.
   exponent = (uf >> exponentShift) & expBaseExtract;
   // This is done to get the fraction part of number.
   frac = uf & mask;
   // The below if and else check whether the number is NaN or not. If so,
   // the argument is simply returned. Otherwise, the absolute value is
   // returned
   if(exponent == expBaseExtract && frac != 0) {
     return uf;
   }
   else {
     return absVal;
   }
}

/*
 * floatInt2Float - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatInt2Float(int x) {
  /* The idea was just to go step by step from left to right like in IEEE.
  Figured out the sign bit first, then calculated E and used that to both get
  exponent as well as even the fraction. Then came up with the conditions for
  determining whether round up occurs or not. Then just added up all these parts
  at end to get result */
  unsigned exponent, signBit, frac, E, mantissa, tail,
  half, roundPart;
  unsigned unsignedX = x;
  int bias = 127;
  int bits;
  int exponentShift = 23;
  int signShift = 31;
  // First conduct a check outside to see if the number is 0. if so, , we can
  // just return 0 automatically. Else, we go through and get the sign,
  // mantissa, fraction part as well as possible roundup and add that all
  // together to get number.
  if(x != 0) {
    // X is positive so signBit is 0 and unsigned number is just x.
    // Otherwise that means it is negative so signBit is 1 and unsignedX is
    // negative version of x.
    if(x > 0) {
      signBit = 0 << signShift;
    }
    else {
      signBit = 1 << signShift;
      unsignedX = -x;
    }
    E = 0;
    mantissa = unsignedX;
    roundPart = 0;
    // This loop goes through right shifting down til we get to 1 or 0.
    // While looping, E is incremented by 1 each time.
    for(;unsignedX > 1; E++)
    {
      unsignedX = unsignedX >> 1;
    }
    exponent = (E + bias) << exponentShift; // got this formula from slides/notes.
    // this is what gets the fraction part of number.
    frac = mantissa & (~(1 << E));
    // The below line lets us know how many bits we need to round to.
      bits = E - exponentShift;
      // This is for the exception case
      if (bits <= 0) {
          frac <<= (-bits);
      }
      else {
          // the back half of fraction we use for checking whether number
          // can be rounded up.
          tail = ((1 << bits) - 1) & frac;
          // fraction part shifted accordingly.
          frac = frac >> bits;
          // the part that is considered the "halfway" mark for rounding up
          // or down
          bits--;
          half = 1 << (bits);
          // To see whether it should be rounded or not, check tail part and
          // see if it equals half and if so then see if frac is 1. if so, then
          // can round up. Otherwise can't. if tail does not equal half mark,
          // but is greater than half, then still round up by making round part
          // 1 to be added. If none of these conditions work , 0 is just set.
          // Although some repition, did it to elminate the or operator and keep
          // number of operations down.
          if(tail > half) {
            roundPart = 1;
          }
          else if (((tail == half) && (frac & 1))) {
            roundPart = 1;
          }
          else {
            roundPart = 0;
          }
      }
      // Adding same way like in IEEE
      return signBit + exponent + frac + roundPart;
  }
  return 0;
}

/*
 * floatScale2 - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatScale2(unsigned uf) {
  // This checks the conditions where if the number is either 0 or it is
  // NaN the argument itself is just returned.
  int doubleFactor = 0x00800000; // used for doubling number
  int check = 0xFF; // used for checking some of the conditions
  int exponentShift = 23; // used for right shifting down uf.
  if(uf==0 || (((uf >> exponentShift) & check) == check)) {
    return uf;
  }
  // For really small values, but non zero cases.
  else if(((uf>>exponentShift) & check) == 0x00) {
		return (uf & (1<<31)) | (uf<<1);
	}
  // This adds one to exponent which doubles value.
  else {
	return uf + doubleFactor;
   }
}
