#ifndef ERASM_X86_ADDR32_DATA32_HPP
#define ERASM_X86_ADDR32_DATA32_HPP
/* DO NOT EDIT! 
Copyright (C) 2011,2012 Makoto Nishiura.

This file is part of ERASM++.

ERASM++ is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

ERASM++ is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with ERASM++; see the file COPYING3.  If not see
 <http://www.gnu.org/licenses/>. */

#include "erasm/x86_ptr.hpp"
namespace erasm {
namespace x86 {
namespace addr32 {
namespace data32 {
/*  37        | ASCII adjust AL after addition. */
 /*  37        | ASCII adjust AL after addition. */
int aaa (code_ptr p);
/*  D5 0A        | ASCII adjust AX before division. */
 /*  D5 0A        | ASCII adjust AX before division. */
int aad (code_ptr p);
/*  D5     ib   | Adjust AX before division to number base imm8. */
 /*  D5     ib   | Adjust AX before division to number base imm8. */
int aad (code_ptr p,imm8_t imm);
/*  D4 0A        | ASCII adjust AX after multiply. */
 /*  D4 0A        | ASCII adjust AX after multiply. */
int aam (code_ptr p);
/*  D4     ib   | Adjust AX after multiply to number base imm8. */
 /*  D4     ib   | Adjust AX after multiply to number base imm8. */
int aam (code_ptr p,imm8_t imm);
/*  3F        | ASCII adjust AL after subtraction. */
 /*  3F        | ASCII adjust AL after subtraction. */
int aas (code_ptr p);
/*  15     id op32  | Add with carry imm32 to EAX. */
 /*  15     id op32  | Add with carry imm32 to EAX. */
int adc (code_ptr p,
         const RegEAX (&   unused),
         imm32_t imm);
/*  83  /2 RMBoth  ib op32  | Add with CF sign-extended imm8 into r/m32. */
 /*  83  /2 RMBoth  ib op32  | Add with CF sign-extended imm8 into r/m32. */
int adc (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm8_t imm);
/*  81  /2 RMBoth  id op32  | Add with CF imm32 to r/m32. */
 /*  81  /2 RMBoth  id op32  | Add with CF imm32 to r/m32. */
int adc (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm32_t imm);
/*  81  /2 RMBoth  id op32  | Add with CF imm32 to r/m32. */
 /*  81  /2 RMBoth  id op32  | Add with CF imm32 to r/m32. */
int adc (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm32_t imm);
/*  83  /2 RMBoth  ib op32  | Add with CF sign-extended imm8 into r/m32. */
 /*  83  /2 RMBoth  ib op32  | Add with CF sign-extended imm8 into r/m32. */
int adc (code_ptr p,
         const RegEAX (&   modrm_rm),
         imm8_t imm);
/*  83  /2 RMBoth  ib op32  | Add with CF sign-extended imm8 into r/m32. */
 /*  83  /2 RMBoth  ib op32  | Add with CF sign-extended imm8 into r/m32. */
int adc (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  15     iw op16  | Add with carry imm16 to AX. */
 /*  15     iw op16  | Add with carry imm16 to AX. */
int adc (code_ptr p,
         const RegAX (&   unused),
         imm16_t imm);
/*  83  /2 RMBoth  ib op16  | Add with CF sign-extended imm8 to r/m16. */
 /*  83  /2 RMBoth  ib op16  | Add with CF sign-extended imm8 to r/m16. */
int adc (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm8_t imm);
/*  81  /2 RMBoth  iw op16  | Add with carry imm16 to r/m16. */
 /*  81  /2 RMBoth  iw op16  | Add with carry imm16 to r/m16. */
int adc (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm16_t imm);
/*  81  /2 RMBoth  iw op16  | Add with carry imm16 to r/m16. */
 /*  81  /2 RMBoth  iw op16  | Add with carry imm16 to r/m16. */
int adc (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm16_t imm);
/*  83  /2 RMBoth  ib op16  | Add with CF sign-extended imm8 to r/m16. */
 /*  83  /2 RMBoth  ib op16  | Add with CF sign-extended imm8 to r/m16. */
int adc (code_ptr p,
         const RegAX (&   modrm_rm),
         imm8_t imm);
/*  83  /2 RMBoth  ib op16  | Add with CF sign-extended imm8 to r/m16. */
 /*  83  /2 RMBoth  ib op16  | Add with CF sign-extended imm8 to r/m16. */
int adc (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  13  /r RMBoth   op32  | Add with CF r/m32 to r32. */
 /*  13  /r RMBoth   op32  | Add with CF r/m32 to r32. */
int adc (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  13  /r RMBoth   op32  | Add with CF r/m32 to r32. */
 /*  13  /r RMBoth   op32  | Add with CF r/m32 to r32. */
int adc (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  11  /r RMBoth   op32  | Add with CF r32 to r/m32. */
 /*  11  /r RMBoth   op32  | Add with CF r32 to r/m32. */
int adc (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  13  /r RMBoth   op16  | Add with carry r/m16 to r16. */
 /*  13  /r RMBoth   op16  | Add with carry r/m16 to r16. */
int adc (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  13  /r RMBoth   op16  | Add with carry r/m16 to r16. */
 /*  13  /r RMBoth   op16  | Add with carry r/m16 to r16. */
int adc (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  11  /r RMBoth   op16  | Add with carry r16 to r/m16. */
 /*  11  /r RMBoth   op16  | Add with carry r16 to r/m16. */
int adc (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  12  /r RMBoth     | Add with carry r/m8 to byte register. */
 /*  12  /r RMBoth     | Add with carry r/m8 to byte register. */
int adc (code_ptr p,
         const ByteReg (&   modrm_reg),
         const ByteReg (&   modrm_rm));
/*  12  /r RMBoth     | Add with carry r/m8 to byte register. */
 /*  12  /r RMBoth     | Add with carry r/m8 to byte register. */
int adc (code_ptr p,
         const ByteReg (&   modrm_reg),
         const BytePtr (&   modrm_rm));
/*  10  /r RMBoth     | Add with carry byte register to r/m8. */
 /*  10  /r RMBoth     | Add with carry byte register to r/m8. */
int adc (code_ptr p,
         const BytePtr (&   modrm_rm),
         const ByteReg (&   modrm_reg));
/*  14     ib   | Add with carry imm8 to AL. */
 /*  14     ib   | Add with carry imm8 to AL. */
int adc (code_ptr p,
         const RegAL (&   unused),
         imm8_t imm);
/*  80  /2 RMBoth  ib   | Add with carry imm8 to r/m8. */
 /*  80  /2 RMBoth  ib   | Add with carry imm8 to r/m8. */
int adc (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  80  /2 RMBoth  ib   | Add with carry imm8 to r/m8. */
 /*  80  /2 RMBoth  ib   | Add with carry imm8 to r/m8. */
int adc (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  05     id op32  | Add imm32 to EAX. */
 /*  05     id op32  | Add imm32 to EAX. */
int add (code_ptr p,
         const RegEAX (&   unused),
         imm32_t imm);
/*  83  /0 RMBoth  ib op32  | Add sign-extended imm8 to r/m32. */
 /*  83  /0 RMBoth  ib op32  | Add sign-extended imm8 to r/m32. */
int add (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm8_t imm);
/*  81  /0 RMBoth  id op32  | Add imm32 to r/m32. */
 /*  81  /0 RMBoth  id op32  | Add imm32 to r/m32. */
int add (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm32_t imm);
/*  81  /0 RMBoth  id op32  | Add imm32 to r/m32. */
 /*  81  /0 RMBoth  id op32  | Add imm32 to r/m32. */
int add (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm32_t imm);
/*  83  /0 RMBoth  ib op32  | Add sign-extended imm8 to r/m32. */
 /*  83  /0 RMBoth  ib op32  | Add sign-extended imm8 to r/m32. */
int add (code_ptr p,
         const RegEAX (&   modrm_rm),
         imm8_t imm);
/*  83  /0 RMBoth  ib op32  | Add sign-extended imm8 to r/m32. */
 /*  83  /0 RMBoth  ib op32  | Add sign-extended imm8 to r/m32. */
int add (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  05     iw op16  | Add imm16 to AX. */
 /*  05     iw op16  | Add imm16 to AX. */
int add (code_ptr p,
         const RegAX (&   unused),
         imm16_t imm);
/*  83  /0 RMBoth  ib op16  | Add sign-extended imm8 to r/m16. */
 /*  83  /0 RMBoth  ib op16  | Add sign-extended imm8 to r/m16. */
int add (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm8_t imm);
/*  81  /0 RMBoth  iw op16  | Add imm16 to r/m16. */
 /*  81  /0 RMBoth  iw op16  | Add imm16 to r/m16. */
int add (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm16_t imm);
/*  81  /0 RMBoth  iw op16  | Add imm16 to r/m16. */
 /*  81  /0 RMBoth  iw op16  | Add imm16 to r/m16. */
int add (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm16_t imm);
/*  83  /0 RMBoth  ib op16  | Add sign-extended imm8 to r/m16. */
 /*  83  /0 RMBoth  ib op16  | Add sign-extended imm8 to r/m16. */
int add (code_ptr p,
         const RegAX (&   modrm_rm),
         imm8_t imm);
/*  83  /0 RMBoth  ib op16  | Add sign-extended imm8 to r/m16. */
 /*  83  /0 RMBoth  ib op16  | Add sign-extended imm8 to r/m16. */
int add (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  03  /r RMBoth   op32  | Add r/m32 to r32. */
 /*  03  /r RMBoth   op32  | Add r/m32 to r32. */
int add (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  03  /r RMBoth   op32  | Add r/m32 to r32. */
 /*  03  /r RMBoth   op32  | Add r/m32 to r32. */
int add (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  01  /r RMBoth   op32  | Add r32 to r/m32. */
 /*  01  /r RMBoth   op32  | Add r32 to r/m32. */
int add (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  03  /r RMBoth   op16  | Add r/m16 to r16. */
 /*  03  /r RMBoth   op16  | Add r/m16 to r16. */
int add (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  03  /r RMBoth   op16  | Add r/m16 to r16. */
 /*  03  /r RMBoth   op16  | Add r/m16 to r16. */
int add (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  01  /r RMBoth   op16  | Add r16 to r/m16. */
 /*  01  /r RMBoth   op16  | Add r16 to r/m16. */
int add (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  02  /r RMBoth     | Add r/m8 to r8. */
 /*  02  /r RMBoth     | Add r/m8 to r8. */
int add (code_ptr p,
         const ByteReg (&   modrm_reg),
         const ByteReg (&   modrm_rm));
/*  02  /r RMBoth     | Add r/m8 to r8. */
 /*  02  /r RMBoth     | Add r/m8 to r8. */
int add (code_ptr p,
         const ByteReg (&   modrm_reg),
         const BytePtr (&   modrm_rm));
/*  00  /r RMBoth     | Add r8 to r/m8. */
 /*  00  /r RMBoth     | Add r8 to r/m8. */
int add (code_ptr p,
         const BytePtr (&   modrm_rm),
         const ByteReg (&   modrm_reg));
/*  04     ib   | Add imm8 to AL. */
 /*  04     ib   | Add imm8 to AL. */
int add (code_ptr p,
         const RegAL (&   unused),
         imm8_t imm);
/*  80  /0 RMBoth  ib   | Add imm8 to r/m8. */
 /*  80  /0 RMBoth  ib   | Add imm8 to r/m8. */
int add (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  80  /0 RMBoth  ib   | Add imm8 to r/m8. */
 /*  80  /0 RMBoth  ib   | Add imm8 to r/m8. */
int add (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/* 66 0F 58  /r RMBoth     | Add packed double-precision floating-point values from xmm2/m128 to xmm1. */
 /* 66 0F 58  /r RMBoth     | Add packed double-precision floating-point values from xmm2/m128 to xmm1. */
int addpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 58  /r RMBoth     | Add packed double-precision floating-point values from xmm2/m128 to xmm1. */
 /* 66 0F 58  /r RMBoth     | Add packed double-precision floating-point values from xmm2/m128 to xmm1. */
int addpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 58  /r RMBoth     | Add packed single-precision floating-point values from xmm2/m128 to xmm1. */
 /*  0F 58  /r RMBoth     | Add packed single-precision floating-point values from xmm2/m128 to xmm1. */
int addps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 58  /r RMBoth     | Add packed single-precision floating-point values from xmm2/m128 to xmm1. */
 /*  0F 58  /r RMBoth     | Add packed single-precision floating-point values from xmm2/m128 to xmm1. */
int addps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* F2 0F 58  /r RMBoth     | Add the low double- precision floating-point value from xmm2/m64 to xmm1. */
 /* F2 0F 58  /r RMBoth     | Add the low double- precision floating-point value from xmm2/m64 to xmm1. */
int addsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F2 0F 58  /r RMBoth     | Add the low double- precision floating-point value from xmm2/m64 to xmm1. */
 /* F2 0F 58  /r RMBoth     | Add the low double- precision floating-point value from xmm2/m64 to xmm1. */
int addsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* F3 0F 58  /r RMBoth     | Add the low single-precision floating-point value from xmm2/m32 to xmm1. */
 /* F3 0F 58  /r RMBoth     | Add the low single-precision floating-point value from xmm2/m32 to xmm1. */
int addss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F3 0F 58  /r RMBoth     | Add the low single-precision floating-point value from xmm2/m32 to xmm1. */
 /* F3 0F 58  /r RMBoth     | Add the low single-precision floating-point value from xmm2/m32 to xmm1. */
int addss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/* 66 0F D0  /r RMBoth     | Add/subtract double- precision floating-point values from xmm2/m128 to xmm1. */
 /* 66 0F D0  /r RMBoth     | Add/subtract double- precision floating-point values from xmm2/m128 to xmm1. */
int addsubpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F D0  /r RMBoth     | Add/subtract double- precision floating-point values from xmm2/m128 to xmm1. */
 /* 66 0F D0  /r RMBoth     | Add/subtract double- precision floating-point values from xmm2/m128 to xmm1. */
int addsubpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* F2 0F D0  /r RMBoth     | Add/subtract single- precision floating-point values from xmm2/m128 to xmm1. */
 /* F2 0F D0  /r RMBoth     | Add/subtract single- precision floating-point values from xmm2/m128 to xmm1. */
int addsubps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F2 0F D0  /r RMBoth     | Add/subtract single- precision floating-point values from xmm2/m128 to xmm1. */
 /* F2 0F D0  /r RMBoth     | Add/subtract single- precision floating-point values from xmm2/m128 to xmm1. */
int addsubps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 DE  /r RMBoth     | Perform one round of an AES decryption flow, using the Equivalent Inverse Cipher, operating on a 128- bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
 /* 66 0F 38 DE  /r RMBoth     | Perform one round of an AES decryption flow, using the Equivalent Inverse Cipher, operating on a 128- bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
int aesdec (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 DE  /r RMBoth     | Perform one round of an AES decryption flow, using the Equivalent Inverse Cipher, operating on a 128- bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
 /* 66 0F 38 DE  /r RMBoth     | Perform one round of an AES decryption flow, using the Equivalent Inverse Cipher, operating on a 128- bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
int aesdec (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 DF  /r RMBoth     | Perform the last round of an AES decryption flow, using the Equivalent Inverse Cipher, operating on a 128- bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
 /* 66 0F 38 DF  /r RMBoth     | Perform the last round of an AES decryption flow, using the Equivalent Inverse Cipher, operating on a 128- bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
int aesdeclast (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/* 66 0F 38 DF  /r RMBoth     | Perform the last round of an AES decryption flow, using the Equivalent Inverse Cipher, operating on a 128- bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
 /* 66 0F 38 DF  /r RMBoth     | Perform the last round of an AES decryption flow, using the Equivalent Inverse Cipher, operating on a 128- bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
int aesdeclast (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 DC  /r RMBoth     | Perform one round of an AES encryption flow, operat- ing on a 128-bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
 /* 66 0F 38 DC  /r RMBoth     | Perform one round of an AES encryption flow, operat- ing on a 128-bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
int aesenc (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 DC  /r RMBoth     | Perform one round of an AES encryption flow, operat- ing on a 128-bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
 /* 66 0F 38 DC  /r RMBoth     | Perform one round of an AES encryption flow, operat- ing on a 128-bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
int aesenc (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 DD  /r RMBoth     | Perform the last round of an AES encryption flow, operat- ing on a 128-bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
 /* 66 0F 38 DD  /r RMBoth     | Perform the last round of an AES encryption flow, operat- ing on a 128-bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
int aesenclast (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/* 66 0F 38 DD  /r RMBoth     | Perform the last round of an AES encryption flow, operat- ing on a 128-bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
 /* 66 0F 38 DD  /r RMBoth     | Perform the last round of an AES encryption flow, operat- ing on a 128-bit data (state) from xmm1 with a 128-bit round key from xmm2/m128. */
int aesenclast (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 DB  /r RMBoth     | Perform the InvMixColumn transformation on a 128-bit round key from xmm2/m128 and store the result in xmm1. */
 /* 66 0F 38 DB  /r RMBoth     | Perform the InvMixColumn transformation on a 128-bit round key from xmm2/m128 and store the result in xmm1. */
int aesimc (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 DB  /r RMBoth     | Perform the InvMixColumn transformation on a 128-bit round key from xmm2/m128 and store the result in xmm1. */
 /* 66 0F 38 DB  /r RMBoth     | Perform the InvMixColumn transformation on a 128-bit round key from xmm2/m128 and store the result in xmm1. */
int aesimc (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 3A DF  /r RMBoth  ib   | Assist in AES round key gen- eration using an 8 bits Round Constant (RCON) specified in the immediate byte, operating on 128 bits of data specified in xmm2/m128 and stores the result in xmm1. */
 /* 66 0F 3A DF  /r RMBoth  ib   | Assist in AES round key gen- eration using an 8 bits Round Constant (RCON) specified in the immediate byte, operating on 128 bits of data specified in xmm2/m128 and stores the result in xmm1. */
int aeskeygenassist (code_ptr p,
                     const XmmReg (&   modrm_reg),
                     const XmmReg (&   modrm_rm),
                     imm8_t imm);
/* 66 0F 3A DF  /r RMBoth  ib   | Assist in AES round key gen- eration using an 8 bits Round Constant (RCON) specified in the immediate byte, operating on 128 bits of data specified in xmm2/m128 and stores the result in xmm1. */
 /* 66 0F 3A DF  /r RMBoth  ib   | Assist in AES round key gen- eration using an 8 bits Round Constant (RCON) specified in the immediate byte, operating on 128 bits of data specified in xmm2/m128 and stores the result in xmm1. */
int aeskeygenassist (code_ptr p,
                     const XmmReg (&   modrm_reg),
                     const XmmWordPtr (&   modrm_rm),
                     imm8_t imm);
/*  25     id op32  | EAX AND imm32. */
 /*  25     id op32  | EAX AND imm32. */
int and_ (code_ptr p,
          const RegEAX (&   unused),
          imm32_t imm);
/*  83  /4 RMBoth  ib op32  | r/m32 AND imm8 (sign- extended). */
 /*  83  /4 RMBoth  ib op32  | r/m32 AND imm8 (sign- extended). */
int and_ (code_ptr p,
          const DwordReg_m_EAX (&   modrm_rm),
          imm8_t imm);
/*  81  /4 RMBoth  id op32  | r/m32 AND imm32. */
 /*  81  /4 RMBoth  id op32  | r/m32 AND imm32. */
int and_ (code_ptr p,
          const DwordReg_m_EAX (&   modrm_rm),
          imm32_t imm);
/*  81  /4 RMBoth  id op32  | r/m32 AND imm32. */
 /*  81  /4 RMBoth  id op32  | r/m32 AND imm32. */
int and_ (code_ptr p,
          const DwordPtr (&   modrm_rm),
          imm32_t imm);
/*  83  /4 RMBoth  ib op32  | r/m32 AND imm8 (sign- extended). */
 /*  83  /4 RMBoth  ib op32  | r/m32 AND imm8 (sign- extended). */
int and_ (code_ptr p,
          const RegEAX (&   modrm_rm),
          imm8_t imm);
/*  83  /4 RMBoth  ib op32  | r/m32 AND imm8 (sign- extended). */
 /*  83  /4 RMBoth  ib op32  | r/m32 AND imm8 (sign- extended). */
int and_ (code_ptr p,
          const DwordPtr (&   modrm_rm),
          imm8_t imm);
/*  25     iw op16  | AX AND imm16. */
 /*  25     iw op16  | AX AND imm16. */
int and_ (code_ptr p,
          const RegAX (&   unused),
          imm16_t imm);
/*  83  /4 RMBoth  ib op16  | r/m16 AND imm8 (sign- extended). */
 /*  83  /4 RMBoth  ib op16  | r/m16 AND imm8 (sign- extended). */
int and_ (code_ptr p,
          const WordReg_m_AX (&   modrm_rm),
          imm8_t imm);
/*  81  /4 RMBoth  iw op16  | r/m16 AND imm16. */
 /*  81  /4 RMBoth  iw op16  | r/m16 AND imm16. */
int and_ (code_ptr p,
          const WordReg_m_AX (&   modrm_rm),
          imm16_t imm);
/*  81  /4 RMBoth  iw op16  | r/m16 AND imm16. */
 /*  81  /4 RMBoth  iw op16  | r/m16 AND imm16. */
int and_ (code_ptr p,
          const WordPtr (&   modrm_rm),
          imm16_t imm);
/*  83  /4 RMBoth  ib op16  | r/m16 AND imm8 (sign- extended). */
 /*  83  /4 RMBoth  ib op16  | r/m16 AND imm8 (sign- extended). */
int and_ (code_ptr p,
          const RegAX (&   modrm_rm),
          imm8_t imm);
/*  83  /4 RMBoth  ib op16  | r/m16 AND imm8 (sign- extended). */
 /*  83  /4 RMBoth  ib op16  | r/m16 AND imm8 (sign- extended). */
int and_ (code_ptr p,
          const WordPtr (&   modrm_rm),
          imm8_t imm);
/*  23  /r RMBoth   op32  | r32 AND r/m32. */
 /*  23  /r RMBoth   op32  | r32 AND r/m32. */
int and_ (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordReg (&   modrm_rm));
/*  23  /r RMBoth   op32  | r32 AND r/m32. */
 /*  23  /r RMBoth   op32  | r32 AND r/m32. */
int and_ (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordPtr (&   modrm_rm));
/*  21  /r RMBoth   op32  | r/m32 AND r32. */
 /*  21  /r RMBoth   op32  | r/m32 AND r32. */
int and_ (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/*  23  /r RMBoth   op16  | r16 AND r/m16. */
 /*  23  /r RMBoth   op16  | r16 AND r/m16. */
int and_ (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordReg (&   modrm_rm));
/*  23  /r RMBoth   op16  | r16 AND r/m16. */
 /*  23  /r RMBoth   op16  | r16 AND r/m16. */
int and_ (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordPtr (&   modrm_rm));
/*  21  /r RMBoth   op16  | r/m16 AND r16. */
 /*  21  /r RMBoth   op16  | r/m16 AND r16. */
int and_ (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  22  /r RMBoth     | r8 AND r/m8. */
 /*  22  /r RMBoth     | r8 AND r/m8. */
int and_ (code_ptr p,
          const ByteReg (&   modrm_reg),
          const ByteReg (&   modrm_rm));
/*  22  /r RMBoth     | r8 AND r/m8. */
 /*  22  /r RMBoth     | r8 AND r/m8. */
int and_ (code_ptr p,
          const ByteReg (&   modrm_reg),
          const BytePtr (&   modrm_rm));
/*  20  /r RMBoth     | r/m8 AND r8. */
 /*  20  /r RMBoth     | r/m8 AND r8. */
int and_ (code_ptr p,
          const BytePtr (&   modrm_rm),
          const ByteReg (&   modrm_reg));
/*  24     ib   | AL AND imm8. */
 /*  24     ib   | AL AND imm8. */
int and_ (code_ptr p,
          const RegAL (&   unused),
          imm8_t imm);
/*  80  /4 RMBoth  ib   | r/m8 AND imm8. */
 /*  80  /4 RMBoth  ib   | r/m8 AND imm8. */
int and_ (code_ptr p,
          const ByteReg (&   modrm_rm),
          imm8_t imm);
/*  80  /4 RMBoth  ib   | r/m8 AND imm8. */
 /*  80  /4 RMBoth  ib   | r/m8 AND imm8. */
int and_ (code_ptr p,
          const BytePtr (&   modrm_rm),
          imm8_t imm);
/* 66 0F 55  /r RMBoth     | Bitwise logical AND NOT of xmm2/m128 and xmm1. */
 /* 66 0F 55  /r RMBoth     | Bitwise logical AND NOT of xmm2/m128 and xmm1. */
int andnpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 55  /r RMBoth     | Bitwise logical AND NOT of xmm2/m128 and xmm1. */
 /* 66 0F 55  /r RMBoth     | Bitwise logical AND NOT of xmm2/m128 and xmm1. */
int andnpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 55  /r RMBoth     | Bitwise logical AND NOT of xmm2/m128 and xmm1. */
 /*  0F 55  /r RMBoth     | Bitwise logical AND NOT of xmm2/m128 and xmm1. */
int andnps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/*  0F 55  /r RMBoth     | Bitwise logical AND NOT of xmm2/m128 and xmm1. */
 /*  0F 55  /r RMBoth     | Bitwise logical AND NOT of xmm2/m128 and xmm1. */
int andnps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 54  /r RMBoth     | Bitwise logical AND of xmm2/m128 and xmm1. */
 /* 66 0F 54  /r RMBoth     | Bitwise logical AND of xmm2/m128 and xmm1. */
int andpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 54  /r RMBoth     | Bitwise logical AND of xmm2/m128 and xmm1. */
 /* 66 0F 54  /r RMBoth     | Bitwise logical AND of xmm2/m128 and xmm1. */
int andpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 54  /r RMBoth     | Bitwise logical AND of xmm2/m128 and xmm1. */
 /*  0F 54  /r RMBoth     | Bitwise logical AND of xmm2/m128 and xmm1. */
int andps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 54  /r RMBoth     | Bitwise logical AND of xmm2/m128 and xmm1. */
 /*  0F 54  /r RMBoth     | Bitwise logical AND of xmm2/m128 and xmm1. */
int andps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  63  /r RMBoth     | Adjust RPL of r/m16 to not less than RPL of r16. */
 /*  63  /r RMBoth     | Adjust RPL of r/m16 to not less than RPL of r16. */
int arpl (code_ptr p,
          const WordReg (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  63  /r RMBoth     | Adjust RPL of r/m16 to not less than RPL of r16. */
 /*  63  /r RMBoth     | Adjust RPL of r/m16 to not less than RPL of r16. */
int arpl (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg));
/* 66 0F 3A 0D  /r RMBoth  ib   | Select packed DP-FP values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
 /* 66 0F 3A 0D  /r RMBoth  ib   | Select packed DP-FP values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
int blendpd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0D  /r RMBoth  ib   | Select packed DP-FP values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
 /* 66 0F 3A 0D  /r RMBoth  ib   | Select packed DP-FP values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
int blendpd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0C  /r RMBoth  ib   | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
 /* 66 0F 3A 0C  /r RMBoth  ib   | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
int blendps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0C  /r RMBoth  ib   | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
 /* 66 0F 3A 0C  /r RMBoth  ib   | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
int blendps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/* 66 0F 38 15  /r RMBoth     | Select packed DP FP values from xmm1 and xmm2 from mask specified in XMM0 and store the values in xmm1. */
 /* 66 0F 38 15  /r RMBoth     | Select packed DP FP values from xmm1 and xmm2 from mask specified in XMM0 and store the values in xmm1. */
int blendvpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm),
              const RegXMM0 (&   unused));
/* 66 0F 38 15  /r RMBoth     | Select packed DP FP values from xmm1 and xmm2 from mask specified in XMM0 and store the values in xmm1. */
 /* 66 0F 38 15  /r RMBoth     | Select packed DP FP values from xmm1 and xmm2 from mask specified in XMM0 and store the values in xmm1. */
int blendvpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm),
              const RegXMM0 (&   unused));
/* 66 0F 38 14  /r RMBoth     | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in XMM0 and store the values into xmm1. */
 /* 66 0F 38 14  /r RMBoth     | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in XMM0 and store the values into xmm1. */
int blendvps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm),
              const RegXMM0 (&   unused));
/* 66 0F 38 14  /r RMBoth     | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in XMM0 and store the values into xmm1. */
 /* 66 0F 38 14  /r RMBoth     | Select packed single precision floating-point values from xmm1 and xmm2/m128 from mask specified in XMM0 and store the values into xmm1. */
int blendvps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm),
              const RegXMM0 (&   unused));
/*  62  /r RMMemOnly   op16  | Check if r16 (array index) is within bounds specified by m16&16. */
 /*  62  /r RMMemOnly   op16  | Check if r16 (array index) is within bounds specified by m16&16. */
int bound (code_ptr p,
           const WordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  62  /r RMMemOnly   op32  | Check if r32 (array index) is within bounds specified by m16&16. */
 /*  62  /r RMMemOnly   op32  | Check if r32 (array index) is within bounds specified by m16&16. */
int bound (code_ptr p,
           const DwordReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/*  0F BC  /r RMBoth   op16  | Bit scan forward on r/m16. */
 /*  0F BC  /r RMBoth   op16  | Bit scan forward on r/m16. */
int bsf (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  0F BC  /r RMBoth   op16  | Bit scan forward on r/m16. */
 /*  0F BC  /r RMBoth   op16  | Bit scan forward on r/m16. */
int bsf (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  0F BC  /r RMBoth   op32  | Bit scan forward on r/m32. */
 /*  0F BC  /r RMBoth   op32  | Bit scan forward on r/m32. */
int bsf (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  0F BC  /r RMBoth   op32  | Bit scan forward on r/m32. */
 /*  0F BC  /r RMBoth   op32  | Bit scan forward on r/m32. */
int bsf (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  0F BD  /r RMBoth   op16  | Bit scan reverse on r/m16. */
 /*  0F BD  /r RMBoth   op16  | Bit scan reverse on r/m16. */
int bsr (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  0F BD  /r RMBoth   op16  | Bit scan reverse on r/m16. */
 /*  0F BD  /r RMBoth   op16  | Bit scan reverse on r/m16. */
int bsr (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  0F BD  /r RMBoth   op32  | Bit scan reverse on r/m32. */
 /*  0F BD  /r RMBoth   op32  | Bit scan reverse on r/m32. */
int bsr (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  0F BD  /r RMBoth   op32  | Bit scan reverse on r/m32. */
 /*  0F BD  /r RMBoth   op32  | Bit scan reverse on r/m32. */
int bsr (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  0F C8    +rd    | Reverses the byte order of a 32-bit register. */
 /*  0F C8    +rd    | Reverses the byte order of a 32-bit register. */
int bswap (code_ptr p,
           const DwordReg (&   radd));
/*  0F BA  /4 RMBoth  ib op16  | Store selected bit in CF flag. */
 /*  0F BA  /4 RMBoth  ib op16  | Store selected bit in CF flag. */
int bt (code_ptr p,
        const WordReg (&   modrm_rm),
        imm8_t imm);
/*  0F BA  /4 RMBoth  ib op16  | Store selected bit in CF flag. */
 /*  0F BA  /4 RMBoth  ib op16  | Store selected bit in CF flag. */
int bt (code_ptr p,
        const WordPtr (&   modrm_rm),
        imm8_t imm);
/*  0F A3  /r RMBoth   op16  | Store selected bit in CF flag. */
 /*  0F A3  /r RMBoth   op16  | Store selected bit in CF flag. */
int bt (code_ptr p,
        const WordReg (&   modrm_rm),
        const WordReg (&   modrm_reg));
/*  0F A3  /r RMBoth   op16  | Store selected bit in CF flag. */
 /*  0F A3  /r RMBoth   op16  | Store selected bit in CF flag. */
int bt (code_ptr p,
        const WordPtr (&   modrm_rm),
        const WordReg (&   modrm_reg));
/*  0F BA  /4 RMBoth  ib op32  | Store selected bit in CF flag. */
 /*  0F BA  /4 RMBoth  ib op32  | Store selected bit in CF flag. */
int bt (code_ptr p,
        const DwordReg (&   modrm_rm),
        imm8_t imm);
/*  0F BA  /4 RMBoth  ib op32  | Store selected bit in CF flag. */
 /*  0F BA  /4 RMBoth  ib op32  | Store selected bit in CF flag. */
int bt (code_ptr p,
        const DwordPtr (&   modrm_rm),
        imm8_t imm);
/*  0F A3  /r RMBoth   op32  | Store selected bit in CF flag. */
 /*  0F A3  /r RMBoth   op32  | Store selected bit in CF flag. */
int bt (code_ptr p,
        const DwordReg (&   modrm_rm),
        const DwordReg (&   modrm_reg));
/*  0F A3  /r RMBoth   op32  | Store selected bit in CF flag. */
 /*  0F A3  /r RMBoth   op32  | Store selected bit in CF flag. */
int bt (code_ptr p,
        const DwordPtr (&   modrm_rm),
        const DwordReg (&   modrm_reg));
/*  0F BA  /7 RMBoth  ib op16  | Store selected bit in CF flag and complement. */
 /*  0F BA  /7 RMBoth  ib op16  | Store selected bit in CF flag and complement. */
int btc (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  0F BA  /7 RMBoth  ib op16  | Store selected bit in CF flag and complement. */
 /*  0F BA  /7 RMBoth  ib op16  | Store selected bit in CF flag and complement. */
int btc (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  0F BB  /r RMBoth   op16  | Store selected bit in CF flag and complement. */
 /*  0F BB  /r RMBoth   op16  | Store selected bit in CF flag and complement. */
int btc (code_ptr p,
         const WordReg (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  0F BB  /r RMBoth   op16  | Store selected bit in CF flag and complement. */
 /*  0F BB  /r RMBoth   op16  | Store selected bit in CF flag and complement. */
int btc (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  0F BA  /7 RMBoth  ib op32  | Store selected bit in CF flag and complement. */
 /*  0F BA  /7 RMBoth  ib op32  | Store selected bit in CF flag and complement. */
int btc (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  0F BA  /7 RMBoth  ib op32  | Store selected bit in CF flag and complement. */
 /*  0F BA  /7 RMBoth  ib op32  | Store selected bit in CF flag and complement. */
int btc (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  0F BB  /r RMBoth   op32  | Store selected bit in CF flag and complement. */
 /*  0F BB  /r RMBoth   op32  | Store selected bit in CF flag and complement. */
int btc (code_ptr p,
         const DwordReg (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  0F BB  /r RMBoth   op32  | Store selected bit in CF flag and complement. */
 /*  0F BB  /r RMBoth   op32  | Store selected bit in CF flag and complement. */
int btc (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  0F BA  /6 RMBoth  ib op16  | Store selected bit in CF flag and clear. */
 /*  0F BA  /6 RMBoth  ib op16  | Store selected bit in CF flag and clear. */
int btr (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  0F BA  /6 RMBoth  ib op16  | Store selected bit in CF flag and clear. */
 /*  0F BA  /6 RMBoth  ib op16  | Store selected bit in CF flag and clear. */
int btr (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  0F B3  /r RMBoth   op16  | Store selected bit in CF flag and clear. */
 /*  0F B3  /r RMBoth   op16  | Store selected bit in CF flag and clear. */
int btr (code_ptr p,
         const WordReg (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  0F B3  /r RMBoth   op16  | Store selected bit in CF flag and clear. */
 /*  0F B3  /r RMBoth   op16  | Store selected bit in CF flag and clear. */
int btr (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  0F BA  /6 RMBoth  ib op32  | Store selected bit in CF flag and clear. */
 /*  0F BA  /6 RMBoth  ib op32  | Store selected bit in CF flag and clear. */
int btr (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  0F BA  /6 RMBoth  ib op32  | Store selected bit in CF flag and clear. */
 /*  0F BA  /6 RMBoth  ib op32  | Store selected bit in CF flag and clear. */
int btr (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  0F B3  /r RMBoth   op32  | Store selected bit in CF flag and clear. */
 /*  0F B3  /r RMBoth   op32  | Store selected bit in CF flag and clear. */
int btr (code_ptr p,
         const DwordReg (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  0F B3  /r RMBoth   op32  | Store selected bit in CF flag and clear. */
 /*  0F B3  /r RMBoth   op32  | Store selected bit in CF flag and clear. */
int btr (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  0F BA  /5 RMBoth  ib op16  | Store selected bit in CF flag and set. */
 /*  0F BA  /5 RMBoth  ib op16  | Store selected bit in CF flag and set. */
int bts (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  0F BA  /5 RMBoth  ib op16  | Store selected bit in CF flag and set. */
 /*  0F BA  /5 RMBoth  ib op16  | Store selected bit in CF flag and set. */
int bts (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  0F AB  /r RMBoth   op16  | Store selected bit in CF flag and set. */
 /*  0F AB  /r RMBoth   op16  | Store selected bit in CF flag and set. */
int bts (code_ptr p,
         const WordReg (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  0F AB  /r RMBoth   op16  | Store selected bit in CF flag and set. */
 /*  0F AB  /r RMBoth   op16  | Store selected bit in CF flag and set. */
int bts (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  0F BA  /5 RMBoth  ib op32  | Store selected bit in CF flag and set. */
 /*  0F BA  /5 RMBoth  ib op32  | Store selected bit in CF flag and set. */
int bts (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  0F BA  /5 RMBoth  ib op32  | Store selected bit in CF flag and set. */
 /*  0F BA  /5 RMBoth  ib op32  | Store selected bit in CF flag and set. */
int bts (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  0F AB  /r RMBoth   op32  | Store selected bit in CF flag and set. */
 /*  0F AB  /r RMBoth   op32  | Store selected bit in CF flag and set. */
int bts (code_ptr p,
         const DwordReg (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  0F AB  /r RMBoth   op32  | Store selected bit in CF flag and set. */
 /*  0F AB  /r RMBoth   op32  | Store selected bit in CF flag and set. */
int bts (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  E8     cw op16  | Call near, relative, displacement relative to next instruction. */
 /*  E8     cw op16  | Call near, relative, displacement relative to next instruction. */
int call (code_ptr p,
          rel16_t imm);
/*  E8     cd op32  | Call near, relative, displacement relative to next instruction. 32-bit displacement sign extended to 64-bits in 64-bit mode. */
 /*  E8     cd op32  | Call near, relative, displacement relative to next instruction. 32-bit displacement sign extended to 64-bits in 64-bit mode. */
int call (code_ptr p,
          rel32_t imm);
/*  9A     cd op16  | Call far, absolute, address given in operand. */
 /*  9A     cd op16  | Call far, absolute, address given in operand. */
int call (code_ptr p,
          const FarPtr16 (&   imm));
/*  9A     cp op32  | Call far, absolute, address given in operand. */
 /*  9A     cp op32  | Call far, absolute, address given in operand. */
int call (code_ptr p,
          const FarPtr32 (&   imm));
/*  FF  /2 RMBoth   op16  | Call near, absolute indirect, address given in r/m16. */
 /*  FF  /2 RMBoth   op16  | Call near, absolute indirect, address given in r/m16. */
int call (code_ptr p,
          const WordReg (&   modrm_rm));
/*  FF  /2 RMBoth   op16  | Call near, absolute indirect, address given in r/m16. */
 /*  FF  /2 RMBoth   op16  | Call near, absolute indirect, address given in r/m16. */
int call (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  FF  /2 RMBoth   op32  | Call near, absolute indirect, address given in r/m32. */
 /*  FF  /2 RMBoth   op32  | Call near, absolute indirect, address given in r/m32. */
int call (code_ptr p,
          const DwordReg (&   modrm_rm));
/*  FF  /2 RMBoth   op32  | Call near, absolute indirect, address given in r/m32. */
 /*  FF  /2 RMBoth   op32  | Call near, absolute indirect, address given in r/m32. */
int call (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  FF  /3 RMMemOnly   op16  | Call far, absolute indirect address given in m16:16. In 32-bit mode: if selector points to a gate, then RIP = 32-bit zero extended displacement taken from gate; else RIP = zero extended 16-bit offset from far pointer referenced in the instruction. */
 /*  FF  /3 RMMemOnly   op16  | Call far, absolute indirect address given in m16:16. In 32-bit mode: if selector points to a gate, then RIP = 32-bit zero extended displacement taken from gate; else RIP = zero extended 16-bit offset from far pointer referenced in the instruction. */
int call (code_ptr p,
          const Far16Ptr (&   modrm_rm));
/*  FF  /3 RMMemOnly   op32  | In 64-bit mode: If selector points to a gate, then RIP = 64-bit displacement taken from gate; else RIP = zero extended 32-bit offset from far pointer referenced in the instruction. */
 /*  FF  /3 RMMemOnly   op32  | In 64-bit mode: If selector points to a gate, then RIP = 64-bit displacement taken from gate; else RIP = zero extended 32-bit offset from far pointer referenced in the instruction. */
int call (code_ptr p,
          const Far32Ptr (&   modrm_rm));
/*  98      op16  | AX ← sign-extend of AL. */
 /*  98      op16  | AX ← sign-extend of AL. */
int cbw (code_ptr p);
/*  99      op32  | EDX:EAX ← sign-extend of EAX. */
 /*  99      op32  | EDX:EAX ← sign-extend of EAX. */
int cdq (code_ptr p);
/*  F8        | Clear CF flag. */
 /*  F8        | Clear CF flag. */
int clc (code_ptr p);
/*  FC        | Clear DF flag. */
 /*  FC        | Clear DF flag. */
int cld (code_ptr p);
/*  0F AE  /7 RMMemOnly     | Flushes cache line containing m8. */
 /*  0F AE  /7 RMMemOnly     | Flushes cache line containing m8. */
int clflush (code_ptr p,
             const BytePtr (&   modrm_rm));
/*  FA        | Clear interrupt flag; interrupts disabled when interrupt flag cleared. */
 /*  FA        | Clear interrupt flag; interrupts disabled when interrupt flag cleared. */
int cli (code_ptr p);
/*  0F 06        | Clears TS flag in CR0. */
 /*  0F 06        | Clears TS flag in CR0. */
int clts (code_ptr p);
/*  F5        | Complement CF flag. */
 /*  F5        | Complement CF flag. */
int cmc (code_ptr p);
/*  0F 47  /r RMBoth   op16  | Move if above (CF=0 and ZF=0). */
 /*  0F 47  /r RMBoth   op16  | Move if above (CF=0 and ZF=0). */
int cmova (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 47  /r RMBoth   op16  | Move if above (CF=0 and ZF=0). */
 /*  0F 47  /r RMBoth   op16  | Move if above (CF=0 and ZF=0). */
int cmova (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 47  /r RMBoth   op32  | Move if above (CF=0 and ZF=0). */
 /*  0F 47  /r RMBoth   op32  | Move if above (CF=0 and ZF=0). */
int cmova (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 47  /r RMBoth   op32  | Move if above (CF=0 and ZF=0). */
 /*  0F 47  /r RMBoth   op32  | Move if above (CF=0 and ZF=0). */
int cmova (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 43  /r RMBoth   op16  | Move if above or equal (CF=0). */
 /*  0F 43  /r RMBoth   op16  | Move if above or equal (CF=0). */
int cmovae (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 43  /r RMBoth   op16  | Move if above or equal (CF=0). */
 /*  0F 43  /r RMBoth   op16  | Move if above or equal (CF=0). */
int cmovae (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 43  /r RMBoth   op32  | Move if above or equal (CF=0). */
 /*  0F 43  /r RMBoth   op32  | Move if above or equal (CF=0). */
int cmovae (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 43  /r RMBoth   op32  | Move if above or equal (CF=0). */
 /*  0F 43  /r RMBoth   op32  | Move if above or equal (CF=0). */
int cmovae (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 42  /r RMBoth   op16  | Move if below (CF=1). */
 /*  0F 42  /r RMBoth   op16  | Move if below (CF=1). */
int cmovb (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 42  /r RMBoth   op16  | Move if below (CF=1). */
 /*  0F 42  /r RMBoth   op16  | Move if below (CF=1). */
int cmovb (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 42  /r RMBoth   op32  | Move if below (CF=1). */
 /*  0F 42  /r RMBoth   op32  | Move if below (CF=1). */
int cmovb (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 42  /r RMBoth   op32  | Move if below (CF=1). */
 /*  0F 42  /r RMBoth   op32  | Move if below (CF=1). */
int cmovb (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 46  /r RMBoth   op16  | Move if below or equal (CF=1 or ZF=1). */
 /*  0F 46  /r RMBoth   op16  | Move if below or equal (CF=1 or ZF=1). */
int cmovbe (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 46  /r RMBoth   op16  | Move if below or equal (CF=1 or ZF=1). */
 /*  0F 46  /r RMBoth   op16  | Move if below or equal (CF=1 or ZF=1). */
int cmovbe (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 46  /r RMBoth   op32  | Move if below or equal (CF=1 or ZF=1). */
 /*  0F 46  /r RMBoth   op32  | Move if below or equal (CF=1 or ZF=1). */
int cmovbe (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 46  /r RMBoth   op32  | Move if below or equal (CF=1 or ZF=1). */
 /*  0F 46  /r RMBoth   op32  | Move if below or equal (CF=1 or ZF=1). */
int cmovbe (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 42  /r RMBoth   op16  | Move if carry (CF=1). */
 /*  0F 42  /r RMBoth   op16  | Move if carry (CF=1). */
int cmovc (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 42  /r RMBoth   op16  | Move if carry (CF=1). */
 /*  0F 42  /r RMBoth   op16  | Move if carry (CF=1). */
int cmovc (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 42  /r RMBoth   op32  | Move if carry (CF=1). */
 /*  0F 42  /r RMBoth   op32  | Move if carry (CF=1). */
int cmovc (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 42  /r RMBoth   op32  | Move if carry (CF=1). */
 /*  0F 42  /r RMBoth   op32  | Move if carry (CF=1). */
int cmovc (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 44  /r RMBoth   op16  | Move if equal (ZF=1). */
 /*  0F 44  /r RMBoth   op16  | Move if equal (ZF=1). */
int cmove (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 44  /r RMBoth   op16  | Move if equal (ZF=1). */
 /*  0F 44  /r RMBoth   op16  | Move if equal (ZF=1). */
int cmove (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 44  /r RMBoth   op32  | Move if equal (ZF=1). */
 /*  0F 44  /r RMBoth   op32  | Move if equal (ZF=1). */
int cmove (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 44  /r RMBoth   op32  | Move if equal (ZF=1). */
 /*  0F 44  /r RMBoth   op32  | Move if equal (ZF=1). */
int cmove (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 4F  /r RMBoth   op16  | Move if greater (ZF=0 and SF=OF). */
 /*  0F 4F  /r RMBoth   op16  | Move if greater (ZF=0 and SF=OF). */
int cmovg (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 4F  /r RMBoth   op16  | Move if greater (ZF=0 and SF=OF). */
 /*  0F 4F  /r RMBoth   op16  | Move if greater (ZF=0 and SF=OF). */
int cmovg (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 4F  /r RMBoth   op32  | Move if greater (ZF=0 and SF=OF). */
 /*  0F 4F  /r RMBoth   op32  | Move if greater (ZF=0 and SF=OF). */
int cmovg (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 4F  /r RMBoth   op32  | Move if greater (ZF=0 and SF=OF). */
 /*  0F 4F  /r RMBoth   op32  | Move if greater (ZF=0 and SF=OF). */
int cmovg (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 4D  /r RMBoth   op16  | Move if greater or equal (SF=OF). */
 /*  0F 4D  /r RMBoth   op16  | Move if greater or equal (SF=OF). */
int cmovge (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 4D  /r RMBoth   op16  | Move if greater or equal (SF=OF). */
 /*  0F 4D  /r RMBoth   op16  | Move if greater or equal (SF=OF). */
int cmovge (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 4D  /r RMBoth   op32  | Move if greater or equal (SF=OF). */
 /*  0F 4D  /r RMBoth   op32  | Move if greater or equal (SF=OF). */
int cmovge (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 4D  /r RMBoth   op32  | Move if greater or equal (SF=OF). */
 /*  0F 4D  /r RMBoth   op32  | Move if greater or equal (SF=OF). */
int cmovge (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 4C  /r RMBoth   op16  | Move if less (SF≠ OF). */
 /*  0F 4C  /r RMBoth   op16  | Move if less (SF≠ OF). */
int cmovl (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 4C  /r RMBoth   op16  | Move if less (SF≠ OF). */
 /*  0F 4C  /r RMBoth   op16  | Move if less (SF≠ OF). */
int cmovl (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 4C  /r RMBoth   op32  | Move if less (SF≠ OF). */
 /*  0F 4C  /r RMBoth   op32  | Move if less (SF≠ OF). */
int cmovl (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 4C  /r RMBoth   op32  | Move if less (SF≠ OF). */
 /*  0F 4C  /r RMBoth   op32  | Move if less (SF≠ OF). */
int cmovl (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 4E  /r RMBoth   op16  | Move if less or equal (ZF=1 or SF≠ OF). */
 /*  0F 4E  /r RMBoth   op16  | Move if less or equal (ZF=1 or SF≠ OF). */
int cmovle (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 4E  /r RMBoth   op16  | Move if less or equal (ZF=1 or SF≠ OF). */
 /*  0F 4E  /r RMBoth   op16  | Move if less or equal (ZF=1 or SF≠ OF). */
int cmovle (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 4E  /r RMBoth   op32  | Move if less or equal (ZF=1 or SF≠ OF). */
 /*  0F 4E  /r RMBoth   op32  | Move if less or equal (ZF=1 or SF≠ OF). */
int cmovle (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 4E  /r RMBoth   op32  | Move if less or equal (ZF=1 or SF≠ OF). */
 /*  0F 4E  /r RMBoth   op32  | Move if less or equal (ZF=1 or SF≠ OF). */
int cmovle (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 46  /r RMBoth   op16  | Move if not above (CF=1 or ZF=1). */
 /*  0F 46  /r RMBoth   op16  | Move if not above (CF=1 or ZF=1). */
int cmovna (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 46  /r RMBoth   op16  | Move if not above (CF=1 or ZF=1). */
 /*  0F 46  /r RMBoth   op16  | Move if not above (CF=1 or ZF=1). */
int cmovna (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 46  /r RMBoth   op32  | Move if not above (CF=1 or ZF=1). */
 /*  0F 46  /r RMBoth   op32  | Move if not above (CF=1 or ZF=1). */
int cmovna (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 46  /r RMBoth   op32  | Move if not above (CF=1 or ZF=1). */
 /*  0F 46  /r RMBoth   op32  | Move if not above (CF=1 or ZF=1). */
int cmovna (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 42  /r RMBoth   op16  | Move if not above or equal (CF=1). */
 /*  0F 42  /r RMBoth   op16  | Move if not above or equal (CF=1). */
int cmovnae (code_ptr p,
             const WordReg (&   modrm_reg),
             const WordReg (&   modrm_rm));
/*  0F 42  /r RMBoth   op16  | Move if not above or equal (CF=1). */
 /*  0F 42  /r RMBoth   op16  | Move if not above or equal (CF=1). */
int cmovnae (code_ptr p,
             const WordReg (&   modrm_reg),
             const WordPtr (&   modrm_rm));
/*  0F 42  /r RMBoth   op32  | Move if not above or equal (CF=1). */
 /*  0F 42  /r RMBoth   op32  | Move if not above or equal (CF=1). */
int cmovnae (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordReg (&   modrm_rm));
/*  0F 42  /r RMBoth   op32  | Move if not above or equal (CF=1). */
 /*  0F 42  /r RMBoth   op32  | Move if not above or equal (CF=1). */
int cmovnae (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/*  0F 43  /r RMBoth   op16  | Move if not below (CF=0). */
 /*  0F 43  /r RMBoth   op16  | Move if not below (CF=0). */
int cmovnb (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 43  /r RMBoth   op16  | Move if not below (CF=0). */
 /*  0F 43  /r RMBoth   op16  | Move if not below (CF=0). */
int cmovnb (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 43  /r RMBoth   op32  | Move if not below (CF=0). */
 /*  0F 43  /r RMBoth   op32  | Move if not below (CF=0). */
int cmovnb (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 43  /r RMBoth   op32  | Move if not below (CF=0). */
 /*  0F 43  /r RMBoth   op32  | Move if not below (CF=0). */
int cmovnb (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 47  /r RMBoth   op16  | Move if not below or equal (CF=0 and ZF=0). */
 /*  0F 47  /r RMBoth   op16  | Move if not below or equal (CF=0 and ZF=0). */
int cmovnbe (code_ptr p,
             const WordReg (&   modrm_reg),
             const WordReg (&   modrm_rm));
/*  0F 47  /r RMBoth   op16  | Move if not below or equal (CF=0 and ZF=0). */
 /*  0F 47  /r RMBoth   op16  | Move if not below or equal (CF=0 and ZF=0). */
int cmovnbe (code_ptr p,
             const WordReg (&   modrm_reg),
             const WordPtr (&   modrm_rm));
/*  0F 47  /r RMBoth   op32  | Move if not below or equal (CF=0 and ZF=0). */
 /*  0F 47  /r RMBoth   op32  | Move if not below or equal (CF=0 and ZF=0). */
int cmovnbe (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordReg (&   modrm_rm));
/*  0F 47  /r RMBoth   op32  | Move if not below or equal (CF=0 and ZF=0). */
 /*  0F 47  /r RMBoth   op32  | Move if not below or equal (CF=0 and ZF=0). */
int cmovnbe (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/*  0F 43  /r RMBoth   op16  | Move if not carry (CF=0). */
 /*  0F 43  /r RMBoth   op16  | Move if not carry (CF=0). */
int cmovnc (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 43  /r RMBoth   op16  | Move if not carry (CF=0). */
 /*  0F 43  /r RMBoth   op16  | Move if not carry (CF=0). */
int cmovnc (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 43  /r RMBoth   op32  | Move if not carry (CF=0). */
 /*  0F 43  /r RMBoth   op32  | Move if not carry (CF=0). */
int cmovnc (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 43  /r RMBoth   op32  | Move if not carry (CF=0). */
 /*  0F 43  /r RMBoth   op32  | Move if not carry (CF=0). */
int cmovnc (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 45  /r RMBoth   op16  | Move if not equal (ZF=0). */
 /*  0F 45  /r RMBoth   op16  | Move if not equal (ZF=0). */
int cmovne (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 45  /r RMBoth   op16  | Move if not equal (ZF=0). */
 /*  0F 45  /r RMBoth   op16  | Move if not equal (ZF=0). */
int cmovne (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 45  /r RMBoth   op32  | Move if not equal (ZF=0). */
 /*  0F 45  /r RMBoth   op32  | Move if not equal (ZF=0). */
int cmovne (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 45  /r RMBoth   op32  | Move if not equal (ZF=0). */
 /*  0F 45  /r RMBoth   op32  | Move if not equal (ZF=0). */
int cmovne (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 4E  /r RMBoth   op16  | Move if not greater (ZF=1 or SF≠ OF). */
 /*  0F 4E  /r RMBoth   op16  | Move if not greater (ZF=1 or SF≠ OF). */
int cmovng (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 4E  /r RMBoth   op16  | Move if not greater (ZF=1 or SF≠ OF). */
 /*  0F 4E  /r RMBoth   op16  | Move if not greater (ZF=1 or SF≠ OF). */
int cmovng (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 4E  /r RMBoth   op32  | Move if not greater (ZF=1 or SF≠ OF). */
 /*  0F 4E  /r RMBoth   op32  | Move if not greater (ZF=1 or SF≠ OF). */
int cmovng (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 4E  /r RMBoth   op32  | Move if not greater (ZF=1 or SF≠ OF). */
 /*  0F 4E  /r RMBoth   op32  | Move if not greater (ZF=1 or SF≠ OF). */
int cmovng (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 4C  /r RMBoth   op16  | Move if not greater or equal (SF≠ OF). */
 /*  0F 4C  /r RMBoth   op16  | Move if not greater or equal (SF≠ OF). */
int cmovnge (code_ptr p,
             const WordReg (&   modrm_reg),
             const WordReg (&   modrm_rm));
/*  0F 4C  /r RMBoth   op16  | Move if not greater or equal (SF≠ OF). */
 /*  0F 4C  /r RMBoth   op16  | Move if not greater or equal (SF≠ OF). */
int cmovnge (code_ptr p,
             const WordReg (&   modrm_reg),
             const WordPtr (&   modrm_rm));
/*  0F 4C  /r RMBoth   op32  | Move if not greater or equal (SF≠ OF). */
 /*  0F 4C  /r RMBoth   op32  | Move if not greater or equal (SF≠ OF). */
int cmovnge (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordReg (&   modrm_rm));
/*  0F 4C  /r RMBoth   op32  | Move if not greater or equal (SF≠ OF). */
 /*  0F 4C  /r RMBoth   op32  | Move if not greater or equal (SF≠ OF). */
int cmovnge (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/*  0F 4D  /r RMBoth   op16  | Move if not less (SF=OF). */
 /*  0F 4D  /r RMBoth   op16  | Move if not less (SF=OF). */
int cmovnl (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 4D  /r RMBoth   op16  | Move if not less (SF=OF). */
 /*  0F 4D  /r RMBoth   op16  | Move if not less (SF=OF). */
int cmovnl (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 4D  /r RMBoth   op32  | Move if not less (SF=OF). */
 /*  0F 4D  /r RMBoth   op32  | Move if not less (SF=OF). */
int cmovnl (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 4D  /r RMBoth   op32  | Move if not less (SF=OF). */
 /*  0F 4D  /r RMBoth   op32  | Move if not less (SF=OF). */
int cmovnl (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 4F  /r RMBoth   op16  | Move if not less or equal (ZF=0 and SF=OF). */
 /*  0F 4F  /r RMBoth   op16  | Move if not less or equal (ZF=0 and SF=OF). */
int cmovnle (code_ptr p,
             const WordReg (&   modrm_reg),
             const WordReg (&   modrm_rm));
/*  0F 4F  /r RMBoth   op16  | Move if not less or equal (ZF=0 and SF=OF). */
 /*  0F 4F  /r RMBoth   op16  | Move if not less or equal (ZF=0 and SF=OF). */
int cmovnle (code_ptr p,
             const WordReg (&   modrm_reg),
             const WordPtr (&   modrm_rm));
/*  0F 4F  /r RMBoth   op32  | Move if not less or equal (ZF=0 and SF=OF). */
 /*  0F 4F  /r RMBoth   op32  | Move if not less or equal (ZF=0 and SF=OF). */
int cmovnle (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordReg (&   modrm_rm));
/*  0F 4F  /r RMBoth   op32  | Move if not less or equal (ZF=0 and SF=OF). */
 /*  0F 4F  /r RMBoth   op32  | Move if not less or equal (ZF=0 and SF=OF). */
int cmovnle (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/*  0F 41  /r RMBoth   op16  | Move if not overflow (OF=0). */
 /*  0F 41  /r RMBoth   op16  | Move if not overflow (OF=0). */
int cmovno (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 41  /r RMBoth   op16  | Move if not overflow (OF=0). */
 /*  0F 41  /r RMBoth   op16  | Move if not overflow (OF=0). */
int cmovno (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 41  /r RMBoth   op32  | Move if not overflow (OF=0). */
 /*  0F 41  /r RMBoth   op32  | Move if not overflow (OF=0). */
int cmovno (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 41  /r RMBoth   op32  | Move if not overflow (OF=0). */
 /*  0F 41  /r RMBoth   op32  | Move if not overflow (OF=0). */
int cmovno (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 4B  /r RMBoth   op16  | Move if not parity (PF=0). */
 /*  0F 4B  /r RMBoth   op16  | Move if not parity (PF=0). */
int cmovnp (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 4B  /r RMBoth   op16  | Move if not parity (PF=0). */
 /*  0F 4B  /r RMBoth   op16  | Move if not parity (PF=0). */
int cmovnp (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 4B  /r RMBoth   op32  | Move if not parity (PF=0). */
 /*  0F 4B  /r RMBoth   op32  | Move if not parity (PF=0). */
int cmovnp (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 4B  /r RMBoth   op32  | Move if not parity (PF=0). */
 /*  0F 4B  /r RMBoth   op32  | Move if not parity (PF=0). */
int cmovnp (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 49  /r RMBoth   op16  | Move if not sign (SF=0). */
 /*  0F 49  /r RMBoth   op16  | Move if not sign (SF=0). */
int cmovns (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 49  /r RMBoth   op16  | Move if not sign (SF=0). */
 /*  0F 49  /r RMBoth   op16  | Move if not sign (SF=0). */
int cmovns (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 49  /r RMBoth   op32  | Move if not sign (SF=0). */
 /*  0F 49  /r RMBoth   op32  | Move if not sign (SF=0). */
int cmovns (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 49  /r RMBoth   op32  | Move if not sign (SF=0). */
 /*  0F 49  /r RMBoth   op32  | Move if not sign (SF=0). */
int cmovns (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 45  /r RMBoth   op16  | Move if not zero (ZF=0). */
 /*  0F 45  /r RMBoth   op16  | Move if not zero (ZF=0). */
int cmovnz (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 45  /r RMBoth   op16  | Move if not zero (ZF=0). */
 /*  0F 45  /r RMBoth   op16  | Move if not zero (ZF=0). */
int cmovnz (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 45  /r RMBoth   op32  | Move if not zero (ZF=0). */
 /*  0F 45  /r RMBoth   op32  | Move if not zero (ZF=0). */
int cmovnz (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 45  /r RMBoth   op32  | Move if not zero (ZF=0). */
 /*  0F 45  /r RMBoth   op32  | Move if not zero (ZF=0). */
int cmovnz (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 40  /r RMBoth   op16  | Move if overflow (OF=0). */
 /*  0F 40  /r RMBoth   op16  | Move if overflow (OF=0). */
int cmovo (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 40  /r RMBoth   op16  | Move if overflow (OF=0). */
 /*  0F 40  /r RMBoth   op16  | Move if overflow (OF=0). */
int cmovo (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 40  /r RMBoth   op32  | Move if overflow (OF=0). */
 /*  0F 40  /r RMBoth   op32  | Move if overflow (OF=0). */
int cmovo (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 40  /r RMBoth   op32  | Move if overflow (OF=0). */
 /*  0F 40  /r RMBoth   op32  | Move if overflow (OF=0). */
int cmovo (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 4A  /r RMBoth   op16  | Move if parity (PF=1). */
 /*  0F 4A  /r RMBoth   op16  | Move if parity (PF=1). */
int cmovp (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 4A  /r RMBoth   op16  | Move if parity (PF=1). */
 /*  0F 4A  /r RMBoth   op16  | Move if parity (PF=1). */
int cmovp (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 4A  /r RMBoth   op32  | Move if parity (PF=1). */
 /*  0F 4A  /r RMBoth   op32  | Move if parity (PF=1). */
int cmovp (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 4A  /r RMBoth   op32  | Move if parity (PF=1). */
 /*  0F 4A  /r RMBoth   op32  | Move if parity (PF=1). */
int cmovp (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 4A  /r RMBoth   op16  | Move if parity even (PF=1). */
 /*  0F 4A  /r RMBoth   op16  | Move if parity even (PF=1). */
int cmovpe (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 4A  /r RMBoth   op16  | Move if parity even (PF=1). */
 /*  0F 4A  /r RMBoth   op16  | Move if parity even (PF=1). */
int cmovpe (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 4A  /r RMBoth   op32  | Move if parity even (PF=1). */
 /*  0F 4A  /r RMBoth   op32  | Move if parity even (PF=1). */
int cmovpe (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 4A  /r RMBoth   op32  | Move if parity even (PF=1). */
 /*  0F 4A  /r RMBoth   op32  | Move if parity even (PF=1). */
int cmovpe (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 4B  /r RMBoth   op16  | Move if parity odd (PF=0). */
 /*  0F 4B  /r RMBoth   op16  | Move if parity odd (PF=0). */
int cmovpo (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/*  0F 4B  /r RMBoth   op16  | Move if parity odd (PF=0). */
 /*  0F 4B  /r RMBoth   op16  | Move if parity odd (PF=0). */
int cmovpo (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/*  0F 4B  /r RMBoth   op32  | Move if parity odd (PF=0). */
 /*  0F 4B  /r RMBoth   op32  | Move if parity odd (PF=0). */
int cmovpo (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/*  0F 4B  /r RMBoth   op32  | Move if parity odd (PF=0). */
 /*  0F 4B  /r RMBoth   op32  | Move if parity odd (PF=0). */
int cmovpo (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F 48  /r RMBoth   op16  | Move if sign (SF=1). */
 /*  0F 48  /r RMBoth   op16  | Move if sign (SF=1). */
int cmovs (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 48  /r RMBoth   op16  | Move if sign (SF=1). */
 /*  0F 48  /r RMBoth   op16  | Move if sign (SF=1). */
int cmovs (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 48  /r RMBoth   op32  | Move if sign (SF=1). */
 /*  0F 48  /r RMBoth   op32  | Move if sign (SF=1). */
int cmovs (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 48  /r RMBoth   op32  | Move if sign (SF=1). */
 /*  0F 48  /r RMBoth   op32  | Move if sign (SF=1). */
int cmovs (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 44  /r RMBoth   op16  | Move if zero (ZF=1). */
 /*  0F 44  /r RMBoth   op16  | Move if zero (ZF=1). */
int cmovz (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F 44  /r RMBoth   op16  | Move if zero (ZF=1). */
 /*  0F 44  /r RMBoth   op16  | Move if zero (ZF=1). */
int cmovz (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 44  /r RMBoth   op32  | Move if zero (ZF=1). */
 /*  0F 44  /r RMBoth   op32  | Move if zero (ZF=1). */
int cmovz (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/*  0F 44  /r RMBoth   op32  | Move if zero (ZF=1). */
 /*  0F 44  /r RMBoth   op32  | Move if zero (ZF=1). */
int cmovz (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  3D     id op32  | Compare imm32 with EAX. */
 /*  3D     id op32  | Compare imm32 with EAX. */
int cmp (code_ptr p,
         const RegEAX (&   unused),
         imm32_t imm);
/*  83  /7 RMBoth  ib op32  | Compare imm8 with r/m32. */
 /*  83  /7 RMBoth  ib op32  | Compare imm8 with r/m32. */
int cmp (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm8_t imm);
/*  81  /7 RMBoth  id op32  | Compare imm32 with r/m32. */
 /*  81  /7 RMBoth  id op32  | Compare imm32 with r/m32. */
int cmp (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm32_t imm);
/*  81  /7 RMBoth  id op32  | Compare imm32 with r/m32. */
 /*  81  /7 RMBoth  id op32  | Compare imm32 with r/m32. */
int cmp (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm32_t imm);
/*  83  /7 RMBoth  ib op32  | Compare imm8 with r/m32. */
 /*  83  /7 RMBoth  ib op32  | Compare imm8 with r/m32. */
int cmp (code_ptr p,
         const RegEAX (&   modrm_rm),
         imm8_t imm);
/*  83  /7 RMBoth  ib op32  | Compare imm8 with r/m32. */
 /*  83  /7 RMBoth  ib op32  | Compare imm8 with r/m32. */
int cmp (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  3D     iw op16  | Compare imm16 with AX. */
 /*  3D     iw op16  | Compare imm16 with AX. */
int cmp (code_ptr p,
         const RegAX (&   unused),
         imm16_t imm);
/*  83  /7 RMBoth  ib op16  | Compare imm8 with r/m16. */
 /*  83  /7 RMBoth  ib op16  | Compare imm8 with r/m16. */
int cmp (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm8_t imm);
/*  81  /7 RMBoth  iw op16  | Compare imm16 with r/m16. */
 /*  81  /7 RMBoth  iw op16  | Compare imm16 with r/m16. */
int cmp (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm16_t imm);
/*  81  /7 RMBoth  iw op16  | Compare imm16 with r/m16. */
 /*  81  /7 RMBoth  iw op16  | Compare imm16 with r/m16. */
int cmp (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm16_t imm);
/*  83  /7 RMBoth  ib op16  | Compare imm8 with r/m16. */
 /*  83  /7 RMBoth  ib op16  | Compare imm8 with r/m16. */
int cmp (code_ptr p,
         const RegAX (&   modrm_rm),
         imm8_t imm);
/*  83  /7 RMBoth  ib op16  | Compare imm8 with r/m16. */
 /*  83  /7 RMBoth  ib op16  | Compare imm8 with r/m16. */
int cmp (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  3B  /r RMBoth   op32  | Compare r/m32 with r32. */
 /*  3B  /r RMBoth   op32  | Compare r/m32 with r32. */
int cmp (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  3B  /r RMBoth   op32  | Compare r/m32 with r32. */
 /*  3B  /r RMBoth   op32  | Compare r/m32 with r32. */
int cmp (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  39  /r RMBoth   op32  | Compare r32 with r/m32. */
 /*  39  /r RMBoth   op32  | Compare r32 with r/m32. */
int cmp (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  3B  /r RMBoth   op16  | Compare r/m16 with r16. */
 /*  3B  /r RMBoth   op16  | Compare r/m16 with r16. */
int cmp (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  3B  /r RMBoth   op16  | Compare r/m16 with r16. */
 /*  3B  /r RMBoth   op16  | Compare r/m16 with r16. */
int cmp (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  39  /r RMBoth   op16  | Compare r16 with r/m16. */
 /*  39  /r RMBoth   op16  | Compare r16 with r/m16. */
int cmp (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  3A  /r RMBoth     | Compare r/m8 with r8. */
 /*  3A  /r RMBoth     | Compare r/m8 with r8. */
int cmp (code_ptr p,
         const ByteReg (&   modrm_reg),
         const ByteReg (&   modrm_rm));
/*  3A  /r RMBoth     | Compare r/m8 with r8. */
 /*  3A  /r RMBoth     | Compare r/m8 with r8. */
int cmp (code_ptr p,
         const ByteReg (&   modrm_reg),
         const BytePtr (&   modrm_rm));
/*  38  /r RMBoth     | Compare r8 with r/m8. */
 /*  38  /r RMBoth     | Compare r8 with r/m8. */
int cmp (code_ptr p,
         const BytePtr (&   modrm_rm),
         const ByteReg (&   modrm_reg));
/*  3C     ib   | Compare imm8 with AL. */
 /*  3C     ib   | Compare imm8 with AL. */
int cmp (code_ptr p,
         const RegAL (&   unused),
         imm8_t imm);
/*  80  /7 RMBoth  ib   | Compare imm8 with r/m8. */
 /*  80  /7 RMBoth  ib   | Compare imm8 with r/m8. */
int cmp (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  80  /7 RMBoth  ib   | Compare imm8 with r/m8. */
 /*  80  /7 RMBoth  ib   | Compare imm8 with r/m8. */
int cmp (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/* 66 0F C2  /r RMBoth  0   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  0   | alias for CMPPD */
int cmpeqpd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F C2  /r RMBoth  0   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  0   | alias for CMPPD */
int cmpeqpd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F C2  /r RMBoth  0   | alias for CMPPS */
 /*  0F C2  /r RMBoth  0   | alias for CMPPS */
int cmpeqps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/*  0F C2  /r RMBoth  0   | alias for CMPPS */
 /*  0F C2  /r RMBoth  0   | alias for CMPPS */
int cmpeqps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* F2 0F C2  /r RMBoth  0   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  0   | alias for CMPSD */
int cmpeqsd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* F2 0F C2  /r RMBoth  0   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  0   | alias for CMPSD */
int cmpeqsd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* F3 0F C2  /r RMBoth  0   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  0   | alias for CMPSS */
int cmpeqss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* F3 0F C2  /r RMBoth  0   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  0   | alias for CMPSS */
int cmpeqss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/* 66 0F C2  /r RMBoth  2   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  2   | alias for CMPPD */
int cmplepd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F C2  /r RMBoth  2   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  2   | alias for CMPPD */
int cmplepd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F C2  /r RMBoth  2   | alias for CMPPS */
 /*  0F C2  /r RMBoth  2   | alias for CMPPS */
int cmpleps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/*  0F C2  /r RMBoth  2   | alias for CMPPS */
 /*  0F C2  /r RMBoth  2   | alias for CMPPS */
int cmpleps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* F2 0F C2  /r RMBoth  2   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  2   | alias for CMPSD */
int cmplesd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* F2 0F C2  /r RMBoth  2   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  2   | alias for CMPSD */
int cmplesd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* F3 0F C2  /r RMBoth  2   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  2   | alias for CMPSS */
int cmpless (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* F3 0F C2  /r RMBoth  2   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  2   | alias for CMPSS */
int cmpless (code_ptr p,
             const XmmReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/* 66 0F C2  /r RMBoth  1   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  1   | alias for CMPPD */
int cmpltpd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F C2  /r RMBoth  1   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  1   | alias for CMPPD */
int cmpltpd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F C2  /r RMBoth  1   | alias for CMPPS */
 /*  0F C2  /r RMBoth  1   | alias for CMPPS */
int cmpltps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/*  0F C2  /r RMBoth  1   | alias for CMPPS */
 /*  0F C2  /r RMBoth  1   | alias for CMPPS */
int cmpltps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* F2 0F C2  /r RMBoth  1   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  1   | alias for CMPSD */
int cmpltsd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* F2 0F C2  /r RMBoth  1   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  1   | alias for CMPSD */
int cmpltsd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* F3 0F C2  /r RMBoth  1   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  1   | alias for CMPSS */
int cmpltss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* F3 0F C2  /r RMBoth  1   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  1   | alias for CMPSS */
int cmpltss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/* 66 0F C2  /r RMBoth  4   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  4   | alias for CMPPD */
int cmpneqpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F C2  /r RMBoth  4   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  4   | alias for CMPPD */
int cmpneqpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F C2  /r RMBoth  4   | alias for CMPPS */
 /*  0F C2  /r RMBoth  4   | alias for CMPPS */
int cmpneqps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F C2  /r RMBoth  4   | alias for CMPPS */
 /*  0F C2  /r RMBoth  4   | alias for CMPPS */
int cmpneqps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* F2 0F C2  /r RMBoth  4   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  4   | alias for CMPSD */
int cmpneqsd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F2 0F C2  /r RMBoth  4   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  4   | alias for CMPSD */
int cmpneqsd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* F3 0F C2  /r RMBoth  4   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  4   | alias for CMPSS */
int cmpneqss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F C2  /r RMBoth  4   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  4   | alias for CMPSS */
int cmpneqss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F C2  /r RMBoth  6   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  6   | alias for CMPPD */
int cmpnlepd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F C2  /r RMBoth  6   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  6   | alias for CMPPD */
int cmpnlepd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F C2  /r RMBoth  6   | alias for CMPPS */
 /*  0F C2  /r RMBoth  6   | alias for CMPPS */
int cmpnleps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F C2  /r RMBoth  6   | alias for CMPPS */
 /*  0F C2  /r RMBoth  6   | alias for CMPPS */
int cmpnleps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* F2 0F C2  /r RMBoth  6   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  6   | alias for CMPSD */
int cmpnlesd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F2 0F C2  /r RMBoth  6   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  6   | alias for CMPSD */
int cmpnlesd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* F3 0F C2  /r RMBoth  6   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  6   | alias for CMPSS */
int cmpnless (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F C2  /r RMBoth  6   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  6   | alias for CMPSS */
int cmpnless (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F C2  /r RMBoth  5   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  5   | alias for CMPPD */
int cmpnltpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F C2  /r RMBoth  5   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  5   | alias for CMPPD */
int cmpnltpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F C2  /r RMBoth  5   | alias for CMPPS */
 /*  0F C2  /r RMBoth  5   | alias for CMPPS */
int cmpnltps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F C2  /r RMBoth  5   | alias for CMPPS */
 /*  0F C2  /r RMBoth  5   | alias for CMPPS */
int cmpnltps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* F2 0F C2  /r RMBoth  5   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  5   | alias for CMPSD */
int cmpnltsd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F2 0F C2  /r RMBoth  5   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  5   | alias for CMPSD */
int cmpnltsd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* F3 0F C2  /r RMBoth  5   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  5   | alias for CMPSS */
int cmpnltss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F C2  /r RMBoth  5   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  5   | alias for CMPSS */
int cmpnltss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F C2  /r RMBoth  7   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  7   | alias for CMPPD */
int cmpordpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F C2  /r RMBoth  7   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  7   | alias for CMPPD */
int cmpordpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F C2  /r RMBoth  7   | alias for CMPPS */
 /*  0F C2  /r RMBoth  7   | alias for CMPPS */
int cmpordps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F C2  /r RMBoth  7   | alias for CMPPS */
 /*  0F C2  /r RMBoth  7   | alias for CMPPS */
int cmpordps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* F2 0F C2  /r RMBoth  7   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  7   | alias for CMPSD */
int cmpordsd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F2 0F C2  /r RMBoth  7   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  7   | alias for CMPSD */
int cmpordsd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* F3 0F C2  /r RMBoth  7   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  7   | alias for CMPSS */
int cmpordss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F C2  /r RMBoth  7   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  7   | alias for CMPSS */
int cmpordss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F C2  /r RMBoth  ib   | Compare packed double- precision floating-point values in xmm2/m128 and xmm1 using imm8 as comparison predicate. */
 /* 66 0F C2  /r RMBoth  ib   | Compare packed double- precision floating-point values in xmm2/m128 and xmm1 using imm8 as comparison predicate. */
int cmppd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F C2  /r RMBoth  ib   | Compare packed double- precision floating-point values in xmm2/m128 and xmm1 using imm8 as comparison predicate. */
 /* 66 0F C2  /r RMBoth  ib   | Compare packed double- precision floating-point values in xmm2/m128 and xmm1 using imm8 as comparison predicate. */
int cmppd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm),
           imm8_t imm);
/*  0F C2  /r RMBoth  ib   | Compare packed single- precision floating-point values in xmm2/mem and xmm1 using imm8 as comparison predicate. */
 /*  0F C2  /r RMBoth  ib   | Compare packed single- precision floating-point values in xmm2/mem and xmm1 using imm8 as comparison predicate. */
int cmpps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/*  0F C2  /r RMBoth  ib   | Compare packed single- precision floating-point values in xmm2/mem and xmm1 using imm8 as comparison predicate. */
 /*  0F C2  /r RMBoth  ib   | Compare packed single- precision floating-point values in xmm2/mem and xmm1 using imm8 as comparison predicate. */
int cmpps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm),
           imm8_t imm);
/*  A6        | For legacy mode, compare byte at address DS:(E)SI with byte at address ES:(E)DI; For 64-bit mode compare byte at address (R|E)SI to byte at address (R|E)DI. The status flags are set accordingly. */
 /*  A6        | For legacy mode, compare byte at address DS:(E)SI with byte at address ES:(E)DI; For 64-bit mode compare byte at address (R|E)SI to byte at address (R|E)DI. The status flags are set accordingly. */
int cmps (code_ptr p,
          const BytePtr_ESI (&   ptr),
          const BytePtr_ES_EDI (&   unused));
/*  A6        | For legacy mode, compare byte at address DS:(E)SI with byte at address ES:(E)DI; For 64-bit mode compare byte at address (R|E)SI to byte at address (R|E)DI. The status flags are set accordingly. */
 /*  A6        | For legacy mode, compare byte at address DS:(E)SI with byte at address ES:(E)DI; For 64-bit mode compare byte at address (R|E)SI to byte at address (R|E)DI. The status flags are set accordingly. */
int cmps (code_ptr p,
          const BytePtr_SI (&   ptr),
          const BytePtr_ES_DI (&   unused));
/*  A7      op16  | For legacy mode, compare word at address DS:(E)SI with word at address ES:(E)DI; For 64-bit mode compare word at address (R|E)SI with word at address (R|E)DI. The status flags are set accordingly. */
 /*  A7      op16  | For legacy mode, compare word at address DS:(E)SI with word at address ES:(E)DI; For 64-bit mode compare word at address (R|E)SI with word at address (R|E)DI. The status flags are set accordingly. */
int cmps (code_ptr p,
          const WordPtr_ESI (&   ptr),
          const WordPtr_ES_EDI (&   unused));
/*  A7      op16  | For legacy mode, compare word at address DS:(E)SI with word at address ES:(E)DI; For 64-bit mode compare word at address (R|E)SI with word at address (R|E)DI. The status flags are set accordingly. */
 /*  A7      op16  | For legacy mode, compare word at address DS:(E)SI with word at address ES:(E)DI; For 64-bit mode compare word at address (R|E)SI with word at address (R|E)DI. The status flags are set accordingly. */
int cmps (code_ptr p,
          const WordPtr_SI (&   ptr),
          const WordPtr_ES_DI (&   unused));
/*  A7      op32  | For legacy mode, compare dword at address DS:(E)SI at dword at address ES:(E)DI; For 64-bit mode compare dword at address (R|E)SI at dword at address (R|E)DI. The status flags are set accordingly. */
 /*  A7      op32  | For legacy mode, compare dword at address DS:(E)SI at dword at address ES:(E)DI; For 64-bit mode compare dword at address (R|E)SI at dword at address (R|E)DI. The status flags are set accordingly. */
int cmps (code_ptr p,
          const DwordPtr_ESI (&   ptr),
          const DwordPtr_ES_EDI (&   unused));
/*  A7      op32  | For legacy mode, compare dword at address DS:(E)SI at dword at address ES:(E)DI; For 64-bit mode compare dword at address (R|E)SI at dword at address (R|E)DI. The status flags are set accordingly. */
 /*  A7      op32  | For legacy mode, compare dword at address DS:(E)SI at dword at address ES:(E)DI; For 64-bit mode compare dword at address (R|E)SI at dword at address (R|E)DI. The status flags are set accordingly. */
int cmps (code_ptr p,
          const DwordPtr_SI (&   ptr),
          const DwordPtr_ES_DI (&   unused));
/*  A6        | For legacy mode, compare byte at address DS:(E)SI with byte at address ES:(E)DI; For 64-bit mode compare byte at address (R|E)SI with byte at address (R|E)DI. The status flags are set accordingly. */
 /*  A6        | For legacy mode, compare byte at address DS:(E)SI with byte at address ES:(E)DI; For 64-bit mode compare byte at address (R|E)SI with byte at address (R|E)DI. The status flags are set accordingly. */
int cmpsb (code_ptr p);
/*  A7      op32  | For legacy mode, compare dword at address DS:(E)SI with dword at address ES:(E)DI; For 64-bit mode compare dword at address (R|E)SI with dword at address (R|E)DI. The status flags are set accordingly. */
 /*  A7      op32  | For legacy mode, compare dword at address DS:(E)SI with dword at address ES:(E)DI; For 64-bit mode compare dword at address (R|E)SI with dword at address (R|E)DI. The status flags are set accordingly. */
int cmpsd (code_ptr p);
/* F2 0F C2  /r RMBoth  ib   | Compare low double- precision floating-point value in xmm2/m64 and xmm1 using imm8 as comparison predicate. */
 /* F2 0F C2  /r RMBoth  ib   | Compare low double- precision floating-point value in xmm2/m64 and xmm1 using imm8 as comparison predicate. */
int cmpsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* F2 0F C2  /r RMBoth  ib   | Compare low double- precision floating-point value in xmm2/m64 and xmm1 using imm8 as comparison predicate. */
 /* F2 0F C2  /r RMBoth  ib   | Compare low double- precision floating-point value in xmm2/m64 and xmm1 using imm8 as comparison predicate. */
int cmpsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm),
           imm8_t imm);
/* F3 0F C2  /r RMBoth  ib   | Compare low single- precision floating-point value in xmm2/m32 and xmm1 using imm8 as comparison predicate. */
 /* F3 0F C2  /r RMBoth  ib   | Compare low single- precision floating-point value in xmm2/m32 and xmm1 using imm8 as comparison predicate. */
int cmpss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* F3 0F C2  /r RMBoth  ib   | Compare low single- precision floating-point value in xmm2/m32 and xmm1 using imm8 as comparison predicate. */
 /* F3 0F C2  /r RMBoth  ib   | Compare low single- precision floating-point value in xmm2/m32 and xmm1 using imm8 as comparison predicate. */
int cmpss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm),
           imm8_t imm);
/*  A7      op16  | For legacy mode, compare word at address DS:(E)SI with word at address ES:(E)DI; For 64-bit mode compare word at address (R|E)SI with word at address (R|E)DI. The status flags are set accordingly. */
 /*  A7      op16  | For legacy mode, compare word at address DS:(E)SI with word at address ES:(E)DI; For 64-bit mode compare word at address (R|E)SI with word at address (R|E)DI. The status flags are set accordingly. */
int cmpsw (code_ptr p);
/* 66 0F C2  /r RMBoth  3   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  3   | alias for CMPPD */
int cmpunordpd (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/* 66 0F C2  /r RMBoth  3   | alias for CMPPD */
 /* 66 0F C2  /r RMBoth  3   | alias for CMPPD */
int cmpunordpd (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmWordPtr (&   modrm_rm));
/*  0F C2  /r RMBoth  3   | alias for CMPPS */
 /*  0F C2  /r RMBoth  3   | alias for CMPPS */
int cmpunordps (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/*  0F C2  /r RMBoth  3   | alias for CMPPS */
 /*  0F C2  /r RMBoth  3   | alias for CMPPS */
int cmpunordps (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmWordPtr (&   modrm_rm));
/* F2 0F C2  /r RMBoth  3   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  3   | alias for CMPSD */
int cmpunordsd (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/* F2 0F C2  /r RMBoth  3   | alias for CMPSD */
 /* F2 0F C2  /r RMBoth  3   | alias for CMPSD */
int cmpunordsd (code_ptr p,
                const XmmReg (&   modrm_reg),
                const QwordPtr (&   modrm_rm));
/* F3 0F C2  /r RMBoth  3   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  3   | alias for CMPSS */
int cmpunordss (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/* F3 0F C2  /r RMBoth  3   | alias for CMPSS */
 /* F3 0F C2  /r RMBoth  3   | alias for CMPSS */
int cmpunordss (code_ptr p,
                const XmmReg (&   modrm_reg),
                const DwordPtr (&   modrm_rm));
/*  0F B0  /r RMBoth     | Compare AL with r/m8. If equal, ZF is set and r8 is loaded into r/m8. Else, clear ZF and load r/m8 into AL. */
 /*  0F B0  /r RMBoth     | Compare AL with r/m8. If equal, ZF is set and r8 is loaded into r/m8. Else, clear ZF and load r/m8 into AL. */
int cmpxchg (code_ptr p,
             const ByteReg (&   modrm_rm),
             const ByteReg (&   modrm_reg));
/*  0F B0  /r RMBoth     | Compare AL with r/m8. If equal, ZF is set and r8 is loaded into r/m8. Else, clear ZF and load r/m8 into AL. */
 /*  0F B0  /r RMBoth     | Compare AL with r/m8. If equal, ZF is set and r8 is loaded into r/m8. Else, clear ZF and load r/m8 into AL. */
int cmpxchg (code_ptr p,
             const BytePtr (&   modrm_rm),
             const ByteReg (&   modrm_reg));
/*  0F B1  /r RMBoth   op16  | Compare AX with r/m16. If equal, ZF is set and r16 is loaded into r/m16. Else, clear ZF and load r/m16 into AX. */
 /*  0F B1  /r RMBoth   op16  | Compare AX with r/m16. If equal, ZF is set and r16 is loaded into r/m16. Else, clear ZF and load r/m16 into AX. */
int cmpxchg (code_ptr p,
             const WordReg (&   modrm_rm),
             const WordReg (&   modrm_reg));
/*  0F B1  /r RMBoth   op16  | Compare AX with r/m16. If equal, ZF is set and r16 is loaded into r/m16. Else, clear ZF and load r/m16 into AX. */
 /*  0F B1  /r RMBoth   op16  | Compare AX with r/m16. If equal, ZF is set and r16 is loaded into r/m16. Else, clear ZF and load r/m16 into AX. */
int cmpxchg (code_ptr p,
             const WordPtr (&   modrm_rm),
             const WordReg (&   modrm_reg));
/*  0F B1  /r RMBoth   op32  | Compare EAX with r/m32. If equal, ZF is set and r32 is loaded into r/m32. Else, clear ZF and load r/m32 into EAX. */
 /*  0F B1  /r RMBoth   op32  | Compare EAX with r/m32. If equal, ZF is set and r32 is loaded into r/m32. Else, clear ZF and load r/m32 into EAX. */
int cmpxchg (code_ptr p,
             const DwordReg (&   modrm_rm),
             const DwordReg (&   modrm_reg));
/*  0F B1  /r RMBoth   op32  | Compare EAX with r/m32. If equal, ZF is set and r32 is loaded into r/m32. Else, clear ZF and load r/m32 into EAX. */
 /*  0F B1  /r RMBoth   op32  | Compare EAX with r/m32. If equal, ZF is set and r32 is loaded into r/m32. Else, clear ZF and load r/m32 into EAX. */
int cmpxchg (code_ptr p,
             const DwordPtr (&   modrm_rm),
             const DwordReg (&   modrm_reg));
/*  0F C7  /1 RMMemOnly     | Compare EDX:EAX with m64. If equal, set ZF and load ECX:EBX into m64. Else, clear ZF and load m64 into EDX:EAX. */
 /*  0F C7  /1 RMMemOnly     | Compare EDX:EAX with m64. If equal, set ZF and load ECX:EBX into m64. Else, clear ZF and load m64 into EDX:EAX. */
int cmpxchg8b (code_ptr p,
               const QwordPtr (&   modrm_rm));
/* 66 0F 2F  /r RMBoth     | Compare low double- precision floating-point values in xmm1 and xmm2/mem64 and set the EFLAGS flags accordingly. */
 /* 66 0F 2F  /r RMBoth     | Compare low double- precision floating-point values in xmm1 and xmm2/mem64 and set the EFLAGS flags accordingly. */
int comisd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 2F  /r RMBoth     | Compare low double- precision floating-point values in xmm1 and xmm2/mem64 and set the EFLAGS flags accordingly. */
 /* 66 0F 2F  /r RMBoth     | Compare low double- precision floating-point values in xmm1 and xmm2/mem64 and set the EFLAGS flags accordingly. */
int comisd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/*  0F 2F  /r RMBoth     | Compare low single- precision floating-point values in xmm1 and xmm2/mem32 and set the EFLAGS flags accordingly. */
 /*  0F 2F  /r RMBoth     | Compare low single- precision floating-point values in xmm1 and xmm2/mem32 and set the EFLAGS flags accordingly. */
int comiss (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/*  0F 2F  /r RMBoth     | Compare low single- precision floating-point values in xmm1 and xmm2/mem32 and set the EFLAGS flags accordingly. */
 /*  0F 2F  /r RMBoth     | Compare low single- precision floating-point values in xmm1 and xmm2/mem32 and set the EFLAGS flags accordingly. */
int comiss (code_ptr p,
            const XmmReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  0F A2        | Returns processor identification and feature information to the EAX, EBX, ECX, and EDX registers, as determined by input entered in EAX (in some cases, ECX as well). */
 /*  0F A2        | Returns processor identification and feature information to the EAX, EBX, ECX, and EDX registers, as determined by input entered in EAX (in some cases, ECX as well). */
int cpuid (code_ptr p);
/* F2 0F 38 F0  /r RMBoth     | Accumulate CRC32 on r/m8. */
 /* F2 0F 38 F0  /r RMBoth     | Accumulate CRC32 on r/m8. */
int crc32 (code_ptr p,
           const DwordReg (&   modrm_reg),
           const ByteReg (&   modrm_rm));
/* F2 0F 38 F0  /r RMBoth     | Accumulate CRC32 on r/m8. */
 /* F2 0F 38 F0  /r RMBoth     | Accumulate CRC32 on r/m8. */
int crc32 (code_ptr p,
           const DwordReg (&   modrm_reg),
           const BytePtr (&   modrm_rm));
/* F2 0F 38 F1  /r RMBoth   op16  | Accumulate CRC32 on r/m16. */
 /* F2 0F 38 F1  /r RMBoth   op16  | Accumulate CRC32 on r/m16. */
int crc32 (code_ptr p,
           const DwordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/* F2 0F 38 F1  /r RMBoth   op16  | Accumulate CRC32 on r/m16. */
 /* F2 0F 38 F1  /r RMBoth   op16  | Accumulate CRC32 on r/m16. */
int crc32 (code_ptr p,
           const DwordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/* F2 0F 38 F1  /r RMBoth   op32  | Accumulate CRC32 on r/m32. */
 /* F2 0F 38 F1  /r RMBoth   op32  | Accumulate CRC32 on r/m32. */
int crc32 (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordReg (&   modrm_rm));
/* F2 0F 38 F1  /r RMBoth   op32  | Accumulate CRC32 on r/m32. */
 /* F2 0F 38 F1  /r RMBoth   op32  | Accumulate CRC32 on r/m32. */
int crc32 (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/* F3 0F E6  /r RMBoth     | Convert two packed signed doubleword integers from xmm2/m128 to two packed double-precision floating- point values in xmm1. */
 /* F3 0F E6  /r RMBoth     | Convert two packed signed doubleword integers from xmm2/m128 to two packed double-precision floating- point values in xmm1. */
int cvtdq2pd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F E6  /r RMBoth     | Convert two packed signed doubleword integers from xmm2/m128 to two packed double-precision floating- point values in xmm1. */
 /* F3 0F E6  /r RMBoth     | Convert two packed signed doubleword integers from xmm2/m128 to two packed double-precision floating- point values in xmm1. */
int cvtdq2pd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/*  0F 5B  /r RMBoth     | Convert four packed signed doubleword integers from xmm2/m128 to four packed single-precision floating- point values in xmm1. */
 /*  0F 5B  /r RMBoth     | Convert four packed signed doubleword integers from xmm2/m128 to four packed single-precision floating- point values in xmm1. */
int cvtdq2ps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F 5B  /r RMBoth     | Convert four packed signed doubleword integers from xmm2/m128 to four packed single-precision floating- point values in xmm1. */
 /*  0F 5B  /r RMBoth     | Convert four packed signed doubleword integers from xmm2/m128 to four packed single-precision floating- point values in xmm1. */
int cvtdq2ps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* F2 0F E6  /r RMBoth     | Convert two packed double- precision floating-point values from xmm2/m128 to two packed signed doubleword integers in xmm1. */
 /* F2 0F E6  /r RMBoth     | Convert two packed double- precision floating-point values from xmm2/m128 to two packed signed doubleword integers in xmm1. */
int cvtpd2dq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F2 0F E6  /r RMBoth     | Convert two packed double- precision floating-point values from xmm2/m128 to two packed signed doubleword integers in xmm1. */
 /* F2 0F E6  /r RMBoth     | Convert two packed double- precision floating-point values from xmm2/m128 to two packed signed doubleword integers in xmm1. */
int cvtpd2dq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* 66 0F 2D  /r RMBoth     | Convert two packed double- precision floating-point values from xmm/m128 to two packed signed doubleword integers in mm. */
 /* 66 0F 2D  /r RMBoth     | Convert two packed double- precision floating-point values from xmm/m128 to two packed signed doubleword integers in mm. */
int cvtpd2pi (code_ptr p,
              const MmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 2D  /r RMBoth     | Convert two packed double- precision floating-point values from xmm/m128 to two packed signed doubleword integers in mm. */
 /* 66 0F 2D  /r RMBoth     | Convert two packed double- precision floating-point values from xmm/m128 to two packed signed doubleword integers in mm. */
int cvtpd2pi (code_ptr p,
              const MmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* 66 0F 5A  /r RMBoth     | Convert two packed double- precision floating-point values in xmm2/m128 to two packed single-precision floating-point values in xmm1. */
 /* 66 0F 5A  /r RMBoth     | Convert two packed double- precision floating-point values in xmm2/m128 to two packed single-precision floating-point values in xmm1. */
int cvtpd2ps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 5A  /r RMBoth     | Convert two packed double- precision floating-point values in xmm2/m128 to two packed single-precision floating-point values in xmm1. */
 /* 66 0F 5A  /r RMBoth     | Convert two packed double- precision floating-point values in xmm2/m128 to two packed single-precision floating-point values in xmm1. */
int cvtpd2ps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* 66 0F 2A  /r RMBoth     | Convert two packed signed doubleword integers from mm/mem64 to two packed double-precision floating- point values in xmm. */
 /* 66 0F 2A  /r RMBoth     | Convert two packed signed doubleword integers from mm/mem64 to two packed double-precision floating- point values in xmm. */
int cvtpi2pd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const MmReg (&   modrm_rm));
/* 66 0F 2A  /r RMBoth     | Convert two packed signed doubleword integers from mm/mem64 to two packed double-precision floating- point values in xmm. */
 /* 66 0F 2A  /r RMBoth     | Convert two packed signed doubleword integers from mm/mem64 to two packed double-precision floating- point values in xmm. */
int cvtpi2pd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/*  0F 2A  /r RMBoth     | Convert two signed doubleword integers from mm/m64 to two single- precision floating-point values in xmm. */
 /*  0F 2A  /r RMBoth     | Convert two signed doubleword integers from mm/m64 to two single- precision floating-point values in xmm. */
int cvtpi2ps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const MmReg (&   modrm_rm));
/*  0F 2A  /r RMBoth     | Convert two signed doubleword integers from mm/m64 to two single- precision floating-point values in xmm. */
 /*  0F 2A  /r RMBoth     | Convert two signed doubleword integers from mm/m64 to two single- precision floating-point values in xmm. */
int cvtpi2ps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 5B  /r RMBoth     | Convert four packed single- precision floating-point values from xmm2/m128 to four packed signed doubleword integers in xmm1. */
 /* 66 0F 5B  /r RMBoth     | Convert four packed single- precision floating-point values from xmm2/m128 to four packed signed doubleword integers in xmm1. */
int cvtps2dq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 5B  /r RMBoth     | Convert four packed single- precision floating-point values from xmm2/m128 to four packed signed doubleword integers in xmm1. */
 /* 66 0F 5B  /r RMBoth     | Convert four packed single- precision floating-point values from xmm2/m128 to four packed signed doubleword integers in xmm1. */
int cvtps2dq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F 5A  /r RMBoth     | Convert two packed single- precision floating-point values in xmm2/m64 to two packed double-precision floating-point values in xmm1. */
 /*  0F 5A  /r RMBoth     | Convert two packed single- precision floating-point values in xmm2/m64 to two packed double-precision floating-point values in xmm1. */
int cvtps2pd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F 5A  /r RMBoth     | Convert two packed single- precision floating-point values in xmm2/m64 to two packed double-precision floating-point values in xmm1. */
 /*  0F 5A  /r RMBoth     | Convert two packed single- precision floating-point values in xmm2/m64 to two packed double-precision floating-point values in xmm1. */
int cvtps2pd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/*  0F 2D  /r RMBoth     | Convert two packed single- precision floating-point values from xmm/m64 to two packed signed doubleword integers in mm. */
 /*  0F 2D  /r RMBoth     | Convert two packed single- precision floating-point values from xmm/m64 to two packed signed doubleword integers in mm. */
int cvtps2pi (code_ptr p,
              const MmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F 2D  /r RMBoth     | Convert two packed single- precision floating-point values from xmm/m64 to two packed signed doubleword integers in mm. */
 /*  0F 2D  /r RMBoth     | Convert two packed single- precision floating-point values from xmm/m64 to two packed signed doubleword integers in mm. */
int cvtps2pi (code_ptr p,
              const MmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* F2 0F 2D  /r RMBoth     | Convert one double- precision floating-point value from xmm/m64 to one signed doubleword integer r32. */
 /* F2 0F 2D  /r RMBoth     | Convert one double- precision floating-point value from xmm/m64 to one signed doubleword integer r32. */
int cvtsd2si (code_ptr p,
              const DwordReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F2 0F 2D  /r RMBoth     | Convert one double- precision floating-point value from xmm/m64 to one signed doubleword integer r32. */
 /* F2 0F 2D  /r RMBoth     | Convert one double- precision floating-point value from xmm/m64 to one signed doubleword integer r32. */
int cvtsd2si (code_ptr p,
              const DwordReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* F2 0F 5A  /r RMBoth     | Convert one double- precision floating-point value in xmm2/m64 to one single-precision floating- point value in xmm1. */
 /* F2 0F 5A  /r RMBoth     | Convert one double- precision floating-point value in xmm2/m64 to one single-precision floating- point value in xmm1. */
int cvtsd2ss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F2 0F 5A  /r RMBoth     | Convert one double- precision floating-point value in xmm2/m64 to one single-precision floating- point value in xmm1. */
 /* F2 0F 5A  /r RMBoth     | Convert one double- precision floating-point value in xmm2/m64 to one single-precision floating- point value in xmm1. */
int cvtsd2ss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* F2 0F 2A  /r RMBoth     | Convert one signed doubleword integer from r/m32 to one double- precision floating-point value in xmm. */
 /* F2 0F 2A  /r RMBoth     | Convert one signed doubleword integer from r/m32 to one double- precision floating-point value in xmm. */
int cvtsi2sd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordReg (&   modrm_rm));
/* F2 0F 2A  /r RMBoth     | Convert one signed doubleword integer from r/m32 to one double- precision floating-point value in xmm. */
 /* F2 0F 2A  /r RMBoth     | Convert one signed doubleword integer from r/m32 to one double- precision floating-point value in xmm. */
int cvtsi2sd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* F3 0F 2A  /r RMBoth     | Convert one signed doubleword integer from r/m32 to one single- precision floating-point value in xmm. */
 /* F3 0F 2A  /r RMBoth     | Convert one signed doubleword integer from r/m32 to one single- precision floating-point value in xmm. */
int cvtsi2ss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordReg (&   modrm_rm));
/* F3 0F 2A  /r RMBoth     | Convert one signed doubleword integer from r/m32 to one single- precision floating-point value in xmm. */
 /* F3 0F 2A  /r RMBoth     | Convert one signed doubleword integer from r/m32 to one single- precision floating-point value in xmm. */
int cvtsi2ss (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* F3 0F 5A  /r RMBoth     | Convert one single-precision floating-point value in xmm2/m32 to one double- precision floating-point value in xmm1. */
 /* F3 0F 5A  /r RMBoth     | Convert one single-precision floating-point value in xmm2/m32 to one double- precision floating-point value in xmm1. */
int cvtss2sd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F 5A  /r RMBoth     | Convert one single-precision floating-point value in xmm2/m32 to one double- precision floating-point value in xmm1. */
 /* F3 0F 5A  /r RMBoth     | Convert one single-precision floating-point value in xmm2/m32 to one double- precision floating-point value in xmm1. */
int cvtss2sd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* F3 0F 2D  /r RMBoth     | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32. */
 /* F3 0F 2D  /r RMBoth     | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32. */
int cvtss2si (code_ptr p,
              const DwordReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F 2D  /r RMBoth     | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32. */
 /* F3 0F 2D  /r RMBoth     | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32. */
int cvtss2si (code_ptr p,
              const DwordReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F E6  /r RMBoth     | Convert two packed double- precision floating-point values from xmm2/m128 to two packed signed doubleword integers in xmm1 using truncation. */
 /* 66 0F E6  /r RMBoth     | Convert two packed double- precision floating-point values from xmm2/m128 to two packed signed doubleword integers in xmm1 using truncation. */
int cvttpd2dq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F E6  /r RMBoth     | Convert two packed double- precision floating-point values from xmm2/m128 to two packed signed doubleword integers in xmm1 using truncation. */
 /* 66 0F E6  /r RMBoth     | Convert two packed double- precision floating-point values from xmm2/m128 to two packed signed doubleword integers in xmm1 using truncation. */
int cvttpd2dq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/* 66 0F 2C  /r RMBoth     | Convert two packer double- precision floating-point values from xmm/m128 to two packed signed doubleword integers in mm using truncation. */
 /* 66 0F 2C  /r RMBoth     | Convert two packer double- precision floating-point values from xmm/m128 to two packed signed doubleword integers in mm using truncation. */
int cvttpd2pi (code_ptr p,
               const MmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F 2C  /r RMBoth     | Convert two packer double- precision floating-point values from xmm/m128 to two packed signed doubleword integers in mm using truncation. */
 /* 66 0F 2C  /r RMBoth     | Convert two packer double- precision floating-point values from xmm/m128 to two packed signed doubleword integers in mm using truncation. */
int cvttpd2pi (code_ptr p,
               const MmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/* F3 0F 5B  /r RMBoth     | Convert four single- precision floating-point values from xmm2/m128 to four signed doubleword integers in xmm1 using truncation. */
 /* F3 0F 5B  /r RMBoth     | Convert four single- precision floating-point values from xmm2/m128 to four signed doubleword integers in xmm1 using truncation. */
int cvttps2dq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* F3 0F 5B  /r RMBoth     | Convert four single- precision floating-point values from xmm2/m128 to four signed doubleword integers in xmm1 using truncation. */
 /* F3 0F 5B  /r RMBoth     | Convert four single- precision floating-point values from xmm2/m128 to four signed doubleword integers in xmm1 using truncation. */
int cvttps2dq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/*  0F 2C  /r RMBoth     | Convert two single- precision floating-point values from xmm/m64 to two signed doubleword signed integers in mm using truncation. */
 /*  0F 2C  /r RMBoth     | Convert two single- precision floating-point values from xmm/m64 to two signed doubleword signed integers in mm using truncation. */
int cvttps2pi (code_ptr p,
               const MmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/*  0F 2C  /r RMBoth     | Convert two single- precision floating-point values from xmm/m64 to two signed doubleword signed integers in mm using truncation. */
 /*  0F 2C  /r RMBoth     | Convert two single- precision floating-point values from xmm/m64 to two signed doubleword signed integers in mm using truncation. */
int cvttps2pi (code_ptr p,
               const MmReg (&   modrm_reg),
               const QwordPtr (&   modrm_rm));
/* F2 0F 2C  /r RMBoth     | Convert one double- precision floating-point value from xmm/m64 to one signed doubleword integer in r32 using truncation. */
 /* F2 0F 2C  /r RMBoth     | Convert one double- precision floating-point value from xmm/m64 to one signed doubleword integer in r32 using truncation. */
int cvttsd2si (code_ptr p,
               const DwordReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* F2 0F 2C  /r RMBoth     | Convert one double- precision floating-point value from xmm/m64 to one signed doubleword integer in r32 using truncation. */
 /* F2 0F 2C  /r RMBoth     | Convert one double- precision floating-point value from xmm/m64 to one signed doubleword integer in r32 using truncation. */
int cvttsd2si (code_ptr p,
               const DwordReg (&   modrm_reg),
               const QwordPtr (&   modrm_rm));
/* F3 0F 2C  /r RMBoth     | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32 using truncation. */
 /* F3 0F 2C  /r RMBoth     | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32 using truncation. */
int cvttss2si (code_ptr p,
               const DwordReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* F3 0F 2C  /r RMBoth     | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32 using truncation. */
 /* F3 0F 2C  /r RMBoth     | Convert one single-precision floating-point value from xmm/m32 to one signed doubleword integer in r32 using truncation. */
int cvttss2si (code_ptr p,
               const DwordReg (&   modrm_reg),
               const DwordPtr (&   modrm_rm));
/*  99      op16  | DX:AX ← sign-extend of AX. */
 /*  99      op16  | DX:AX ← sign-extend of AX. */
int cwd (code_ptr p);
/*  98      op32  | EAX ← sign-extend of AX. */
 /*  98      op32  | EAX ← sign-extend of AX. */
int cwde (code_ptr p);
/*  27        | Decimal adjust AL after addition. */
 /*  27        | Decimal adjust AL after addition. */
int daa (code_ptr p);
/*  2F        | Decimal adjust AL after subtraction. */
 /*  2F        | Decimal adjust AL after subtraction. */
int das (code_ptr p);
/*  48    +rd  op32  | Decrement r32 by 1. */
 /*  48    +rd  op32  | Decrement r32 by 1. */
int dec (code_ptr p,
         const DwordReg (&   radd));
/*  FF  /1 RMBoth   op32  | Decrement r/m32 by 1. */
 /*  FF  /1 RMBoth   op32  | Decrement r/m32 by 1. */
int dec (code_ptr p,
         const DwordPtr (&   modrm_rm));
/*  48    +rw  op16  | Decrement r16 by 1. */
 /*  48    +rw  op16  | Decrement r16 by 1. */
int dec (code_ptr p,
         const WordReg (&   radd));
/*  FF  /1 RMBoth   op16  | Decrement r/m16 by 1. */
 /*  FF  /1 RMBoth   op16  | Decrement r/m16 by 1. */
int dec (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  FE  /1 RMBoth     | Decrement r/m8 by 1. */
 /*  FE  /1 RMBoth     | Decrement r/m8 by 1. */
int dec (code_ptr p,
         const ByteReg (&   modrm_rm));
/*  FE  /1 RMBoth     | Decrement r/m8 by 1. */
 /*  FE  /1 RMBoth     | Decrement r/m8 by 1. */
int dec (code_ptr p,
         const BytePtr (&   modrm_rm));
/*  F6  /6 RMBoth     | Unsigned divide AX by r/m8, with result stored in AL ← Quotient, AH ← Remainder. */
 /*  F6  /6 RMBoth     | Unsigned divide AX by r/m8, with result stored in AL ← Quotient, AH ← Remainder. */
int div (code_ptr p,
         const ByteReg (&   modrm_rm));
/*  F6  /6 RMBoth     | Unsigned divide AX by r/m8, with result stored in AL ← Quotient, AH ← Remainder. */
 /*  F6  /6 RMBoth     | Unsigned divide AX by r/m8, with result stored in AL ← Quotient, AH ← Remainder. */
int div (code_ptr p,
         const BytePtr (&   modrm_rm));
/*  F7  /6 RMBoth   op16  | Unsigned divide DX:AX by r/m16, with result stored in AX ← Quotient, DX ← Remainder. */
 /*  F7  /6 RMBoth   op16  | Unsigned divide DX:AX by r/m16, with result stored in AX ← Quotient, DX ← Remainder. */
int div (code_ptr p,
         const WordReg (&   modrm_rm));
/*  F7  /6 RMBoth   op16  | Unsigned divide DX:AX by r/m16, with result stored in AX ← Quotient, DX ← Remainder. */
 /*  F7  /6 RMBoth   op16  | Unsigned divide DX:AX by r/m16, with result stored in AX ← Quotient, DX ← Remainder. */
int div (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  F7  /6 RMBoth   op32  | Unsigned divide EDX:EAX by r/m32, with result stored in EAX ← Quotient, EDX ← Remainder. */
 /*  F7  /6 RMBoth   op32  | Unsigned divide EDX:EAX by r/m32, with result stored in EAX ← Quotient, EDX ← Remainder. */
int div (code_ptr p,
         const DwordReg (&   modrm_rm));
/*  F7  /6 RMBoth   op32  | Unsigned divide EDX:EAX by r/m32, with result stored in EAX ← Quotient, EDX ← Remainder. */
 /*  F7  /6 RMBoth   op32  | Unsigned divide EDX:EAX by r/m32, with result stored in EAX ← Quotient, EDX ← Remainder. */
int div (code_ptr p,
         const DwordPtr (&   modrm_rm));
/* 66 0F 5E  /r RMBoth     | Divide packed double- precision floating-point values in xmm1 by packed double-precision floating- point values xmm2/m128. */
 /* 66 0F 5E  /r RMBoth     | Divide packed double- precision floating-point values in xmm1 by packed double-precision floating- point values xmm2/m128. */
int divpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 5E  /r RMBoth     | Divide packed double- precision floating-point values in xmm1 by packed double-precision floating- point values xmm2/m128. */
 /* 66 0F 5E  /r RMBoth     | Divide packed double- precision floating-point values in xmm1 by packed double-precision floating- point values xmm2/m128. */
int divpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 5E  /r RMBoth     | Divide packed single- precision floating-point values in xmm1 by packed single-precision floating- point values xmm2/m128. */
 /*  0F 5E  /r RMBoth     | Divide packed single- precision floating-point values in xmm1 by packed single-precision floating- point values xmm2/m128. */
int divps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 5E  /r RMBoth     | Divide packed single- precision floating-point values in xmm1 by packed single-precision floating- point values xmm2/m128. */
 /*  0F 5E  /r RMBoth     | Divide packed single- precision floating-point values in xmm1 by packed single-precision floating- point values xmm2/m128. */
int divps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* F2 0F 5E  /r RMBoth     | Divide low double-precision floating-point value n xmm1 by low double-precision floating-point value in xmm2/mem64. */
 /* F2 0F 5E  /r RMBoth     | Divide low double-precision floating-point value n xmm1 by low double-precision floating-point value in xmm2/mem64. */
int divsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F2 0F 5E  /r RMBoth     | Divide low double-precision floating-point value n xmm1 by low double-precision floating-point value in xmm2/mem64. */
 /* F2 0F 5E  /r RMBoth     | Divide low double-precision floating-point value n xmm1 by low double-precision floating-point value in xmm2/mem64. */
int divsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* F3 0F 5E  /r RMBoth     | Divide low single-precision floating-point value in xmm1 by low single- precision floating-point value in xmm2/m32. */
 /* F3 0F 5E  /r RMBoth     | Divide low single-precision floating-point value in xmm1 by low single- precision floating-point value in xmm2/m32. */
int divss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F3 0F 5E  /r RMBoth     | Divide low single-precision floating-point value in xmm1 by low single- precision floating-point value in xmm2/m32. */
 /* F3 0F 5E  /r RMBoth     | Divide low single-precision floating-point value in xmm1 by low single- precision floating-point value in xmm2/m32. */
int divss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/* 66 0F 3A 41  /r RMBoth  ib   | Selectively multiply packed DP floating-point values from xmm1 with packed DP floating-point values from xmm2, add and selectively store the packed DP floating-point values to xmm1. */
 /* 66 0F 3A 41  /r RMBoth  ib   | Selectively multiply packed DP floating-point values from xmm1 with packed DP floating-point values from xmm2, add and selectively store the packed DP floating-point values to xmm1. */
int dppd (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmReg (&   modrm_rm),
          imm8_t imm);
/* 66 0F 3A 41  /r RMBoth  ib   | Selectively multiply packed DP floating-point values from xmm1 with packed DP floating-point values from xmm2, add and selectively store the packed DP floating-point values to xmm1. */
 /* 66 0F 3A 41  /r RMBoth  ib   | Selectively multiply packed DP floating-point values from xmm1 with packed DP floating-point values from xmm2, add and selectively store the packed DP floating-point values to xmm1. */
int dppd (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmWordPtr (&   modrm_rm),
          imm8_t imm);
/* 66 0F 3A 40  /r RMBoth  ib   | Selectively multiply packed SP floating-point values from xmm1 with packed SP floating-point values from xmm2, add and selectively store the packed SP floating-point values or zero values to xmm1. */
 /* 66 0F 3A 40  /r RMBoth  ib   | Selectively multiply packed SP floating-point values from xmm1 with packed SP floating-point values from xmm2, add and selectively store the packed SP floating-point values or zero values to xmm1. */
int dpps (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmReg (&   modrm_rm),
          imm8_t imm);
/* 66 0F 3A 40  /r RMBoth  ib   | Selectively multiply packed SP floating-point values from xmm1 with packed SP floating-point values from xmm2, add and selectively store the packed SP floating-point values or zero values to xmm1. */
 /* 66 0F 3A 40  /r RMBoth  ib   | Selectively multiply packed SP floating-point values from xmm1 with packed SP floating-point values from xmm2, add and selectively store the packed SP floating-point values or zero values to xmm1. */
int dpps (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmWordPtr (&   modrm_rm),
          imm8_t imm);
/*  0F 77        | Set the x87 FPU tag word to empty. */
 /*  0F 77        | Set the x87 FPU tag word to empty. */
int emms (code_ptr p);
/*  C8     iw ib   | Create a nested stack frame for a procedure. */
 /*  C8     iw ib   | Create a nested stack frame for a procedure. */
int enter (code_ptr p,
           imm16_t imm1,
           imm8_t imm2);
/* 66 0F 3A 17  /r RMBoth  ib   | Extract a single-precision floating-point value from xmm2 at the source offset specified by imm8 and store the result to reg or m32. The upper 32 bits of r64 is zeroed if reg is r64. */
 /* 66 0F 3A 17  /r RMBoth  ib   | Extract a single-precision floating-point value from xmm2 at the source offset specified by imm8 and store the result to reg or m32. The upper 32 bits of r64 is zeroed if reg is r64. */
int extractps (code_ptr p,
               const DwordReg (&   modrm_rm),
               const XmmReg (&   modrm_reg),
               imm8_t imm);
/* 66 0F 3A 17  /r RMBoth  ib   | Extract a single-precision floating-point value from xmm2 at the source offset specified by imm8 and store the result to reg or m32. The upper 32 bits of r64 is zeroed if reg is r64. */
 /* 66 0F 3A 17  /r RMBoth  ib   | Extract a single-precision floating-point value from xmm2 at the source offset specified by imm8 and store the result to reg or m32. The upper 32 bits of r64 is zeroed if reg is r64. */
int extractps (code_ptr p,
               const DwordPtr (&   modrm_rm),
               const XmmReg (&   modrm_reg),
               imm8_t imm);
/*  D9 F0        | Replace ST(0) with (2ST(0) – 1). */
 /*  D9 F0        | Replace ST(0) with (2ST(0) – 1). */
int f2xm1 (code_ptr p);
/*  D9 E1        | Replace ST with its absolute value. */
 /*  D9 E1        | Replace ST with its absolute value. */
int fabs (code_ptr p);
/*  D8  /0 RMMemOnly     | Add m32fp to ST(0) and store result in ST(0). */
 /*  D8  /0 RMMemOnly     | Add m32fp to ST(0) and store result in ST(0). */
int fadd (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  DC  /0 RMMemOnly     | Add m64fp to ST(0) and store result in ST(0). */
 /*  DC  /0 RMMemOnly     | Add m64fp to ST(0) and store result in ST(0). */
int fadd (code_ptr p,
          const QwordPtr (&   modrm_rm));
/*  D8 C0    +i    | Add ST(0) to ST(i) and store result in ST(0). */
 /*  D8 C0    +i    | Add ST(0) to ST(i) and store result in ST(0). */
int fadd (code_ptr p,
          const RegST0 (&   unused),
          const StReg (&   radd));
/*  DC C0    +i    | Add ST(i) to ST(0) and store result in ST(i). */
 /*  DC C0    +i    | Add ST(i) to ST(0) and store result in ST(i). */
int fadd (code_ptr p,
          const StReg_m_ST0 (&   radd),
          const RegST0 (&   unused));
/*  DE C1        | Add ST(0) to ST(1), store result in ST(1), and pop the register stack. */
 /*  DE C1        | Add ST(0) to ST(1), store result in ST(1), and pop the register stack. */
int faddp (code_ptr p);
/*  DE C0    +i    | Add ST(0) to ST(i), store result in ST(i), and pop the register stack. */
 /*  DE C0    +i    | Add ST(0) to ST(i), store result in ST(i), and pop the register stack. */
int faddp (code_ptr p,
           const StReg (&   radd),
           const RegST0 (&   unused));
/*  DF  /4 RMMemOnly     | Convert BCD value to floating-point and push onto the FPU stack. */
 /*  DF  /4 RMMemOnly     | Convert BCD value to floating-point and push onto the FPU stack. */
int fbld (code_ptr p,
          const TbytePtr (&   modrm_rm));
/*  DF  /6 RMMemOnly     | Store ST(0) in m80bcd and pop ST(0). */
 /*  DF  /6 RMMemOnly     | Store ST(0) in m80bcd and pop ST(0). */
int fbstp (code_ptr p,
           const TbytePtr (&   modrm_rm));
/*  D9 E0        | Complements sign of ST(0). */
 /*  D9 E0        | Complements sign of ST(0). */
int fchs (code_ptr p);
/*  9B DB E2        | Clear floating-point exception flags after checking for pending unmasked floating- point exceptions. */
 /*  9B DB E2        | Clear floating-point exception flags after checking for pending unmasked floating- point exceptions. */
int fclex (code_ptr p);
/*  DA C0    +i    | Move if below (CF=1). */
 /*  DA C0    +i    | Move if below (CF=1). */
int fcmovb (code_ptr p,
            const RegST0 (&   unused),
            const StReg (&   radd));
/*  DA D0    +i    | Move if below or equal (CF=1 or ZF=1). */
 /*  DA D0    +i    | Move if below or equal (CF=1 or ZF=1). */
int fcmovbe (code_ptr p,
             const RegST0 (&   unused),
             const StReg (&   radd));
/*  DA C8    +i    | Move if equal (ZF=1). */
 /*  DA C8    +i    | Move if equal (ZF=1). */
int fcmove (code_ptr p,
            const RegST0 (&   unused),
            const StReg (&   radd));
/*  DB C0    +i    | Move if not below (CF=0). */
 /*  DB C0    +i    | Move if not below (CF=0). */
int fcmovnb (code_ptr p,
             const RegST0 (&   unused),
             const StReg (&   radd));
/*  DB D0    +i    | Move if not below or equal (CF=0 and ZF=0). */
 /*  DB D0    +i    | Move if not below or equal (CF=0 and ZF=0). */
int fcmovnbe (code_ptr p,
              const RegST0 (&   unused),
              const StReg (&   radd));
/*  DB C8    +i    | Move if not equal (ZF=0). */
 /*  DB C8    +i    | Move if not equal (ZF=0). */
int fcmovne (code_ptr p,
             const RegST0 (&   unused),
             const StReg (&   radd));
/*  DB D8    +i    | Move if not unordered (PF=0). */
 /*  DB D8    +i    | Move if not unordered (PF=0). */
int fcmovnu (code_ptr p,
             const RegST0 (&   unused),
             const StReg (&   radd));
/*  DA D8    +i    | Move if unordered (PF=1). */
 /*  DA D8    +i    | Move if unordered (PF=1). */
int fcmovu (code_ptr p,
            const RegST0 (&   unused),
            const StReg (&   radd));
/*  D8 D1        | Compare ST(0) with ST(1). */
 /*  D8 D1        | Compare ST(0) with ST(1). */
int fcom (code_ptr p);
/*  D8 D0    +i    | Compare ST(0) with ST(i). */
 /*  D8 D0    +i    | Compare ST(0) with ST(i). */
int fcom (code_ptr p,
          const StReg (&   radd));
/*  D8  /2 RMMemOnly     | Compare ST(0) with m32fp. */
 /*  D8  /2 RMMemOnly     | Compare ST(0) with m32fp. */
int fcom (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  DC  /2 RMMemOnly     | Compare ST(0) with m64fp. */
 /*  DC  /2 RMMemOnly     | Compare ST(0) with m64fp. */
int fcom (code_ptr p,
          const QwordPtr (&   modrm_rm));
/*  DB F0    +i    | Compare ST(0) with ST(i) and set status flags accordingly. */
 /*  DB F0    +i    | Compare ST(0) with ST(i) and set status flags accordingly. */
int fcomi (code_ptr p,
           const RegST0 (&   unused),
           const StReg (&   radd));
/*  DF F0    +i    | Compare ST(0) with ST(i), set status flags accordingly, and pop register stack. */
 /*  DF F0    +i    | Compare ST(0) with ST(i), set status flags accordingly, and pop register stack. */
int fcomip (code_ptr p,
            const RegST0 (&   unused),
            const StReg (&   radd));
/*  D8 D9        | Compare ST(0) with ST(1) and pop register stack. */
 /*  D8 D9        | Compare ST(0) with ST(1) and pop register stack. */
int fcomp (code_ptr p);
/*  D8 D8    +i    | Compare ST(0) with ST(i) and pop register stack. */
 /*  D8 D8    +i    | Compare ST(0) with ST(i) and pop register stack. */
int fcomp (code_ptr p,
           const StReg (&   radd));
/*  D8  /3 RMMemOnly     | Compare ST(0) with m32fp and pop register stack. */
 /*  D8  /3 RMMemOnly     | Compare ST(0) with m32fp and pop register stack. */
int fcomp (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  DC  /3 RMMemOnly     | Compare ST(0) with m64fp and pop register stack. */
 /*  DC  /3 RMMemOnly     | Compare ST(0) with m64fp and pop register stack. */
int fcomp (code_ptr p,
           const QwordPtr (&   modrm_rm));
/*  DE D9        | Compare ST(0) with ST(1) and pop register stack twice. */
 /*  DE D9        | Compare ST(0) with ST(1) and pop register stack twice. */
int fcompp (code_ptr p);
/*  D9 FF        | Replace ST(0) with its cosine. */
 /*  D9 FF        | Replace ST(0) with its cosine. */
int fcos (code_ptr p);
/*  D9 F6        | Decrement TOP field in FPU status word. */
 /*  D9 F6        | Decrement TOP field in FPU status word. */
int fdecstp (code_ptr p);
/*  D8  /6 RMMemOnly     | Divide ST(0) by m32fp and store result in ST(0). */
 /*  D8  /6 RMMemOnly     | Divide ST(0) by m32fp and store result in ST(0). */
int fdiv (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  DC  /6 RMMemOnly     | Divide ST(0) by m64fp and store result in ST(0). */
 /*  DC  /6 RMMemOnly     | Divide ST(0) by m64fp and store result in ST(0). */
int fdiv (code_ptr p,
          const QwordPtr (&   modrm_rm));
/*  D8 F0    +i    | Divide ST(0) by ST(i) and store result in ST(0). */
 /*  D8 F0    +i    | Divide ST(0) by ST(i) and store result in ST(0). */
int fdiv (code_ptr p,
          const RegST0 (&   unused),
          const StReg (&   radd));
/*  DC F8    +i    | Divide ST(i) by ST(0) and store result in ST(i). */
 /*  DC F8    +i    | Divide ST(i) by ST(0) and store result in ST(i). */
int fdiv (code_ptr p,
          const StReg_m_ST0 (&   radd),
          const RegST0 (&   unused));
/*  DE F9        | Divide ST(1) by ST(0), store result in ST(1), and pop the register stack. */
 /*  DE F9        | Divide ST(1) by ST(0), store result in ST(1), and pop the register stack. */
int fdivp (code_ptr p);
/*  DE F8    +i    | Divide ST(i) by ST(0), store result in ST(i), and pop the register stack. */
 /*  DE F8    +i    | Divide ST(i) by ST(0), store result in ST(i), and pop the register stack. */
int fdivp (code_ptr p,
           const StReg (&   radd),
           const RegST0 (&   unused));
/*  D8  /7 RMMemOnly     | Divide m32fp by ST(0) and store result in ST(0). */
 /*  D8  /7 RMMemOnly     | Divide m32fp by ST(0) and store result in ST(0). */
int fdivr (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  DC  /7 RMMemOnly     | Divide m64fp by ST(0) and store result in ST(0). */
 /*  DC  /7 RMMemOnly     | Divide m64fp by ST(0) and store result in ST(0). */
int fdivr (code_ptr p,
           const QwordPtr (&   modrm_rm));
/*  D8 F8    +i    | Divide ST(i) by ST(0) and store result in ST(0). */
 /*  D8 F8    +i    | Divide ST(i) by ST(0) and store result in ST(0). */
int fdivr (code_ptr p,
           const RegST0 (&   unused),
           const StReg (&   radd));
/*  DC F0    +i    | Divide ST(0) by ST(i) and store result in ST(i). */
 /*  DC F0    +i    | Divide ST(0) by ST(i) and store result in ST(i). */
int fdivr (code_ptr p,
           const StReg_m_ST0 (&   radd),
           const RegST0 (&   unused));
/*  DE F1        | Divide ST(0) by ST(1), store result in ST(1), and pop the register stack. */
 /*  DE F1        | Divide ST(0) by ST(1), store result in ST(1), and pop the register stack. */
int fdivrp (code_ptr p);
/*  DE F0    +i    | Divide ST(0) by ST(i), store result in ST(i), and pop the register stack. */
 /*  DE F0    +i    | Divide ST(0) by ST(i), store result in ST(i), and pop the register stack. */
int fdivrp (code_ptr p,
            const StReg (&   radd),
            const RegST0 (&   unused));
/*  DD C0    +i    | Sets tag for ST(i) to empty. */
 /*  DD C0    +i    | Sets tag for ST(i) to empty. */
int ffree (code_ptr p,
           const StReg (&   radd));
/*  DE  /0 RMMemOnly     | Add m16int to ST(0) and store result in ST(0). */
 /*  DE  /0 RMMemOnly     | Add m16int to ST(0) and store result in ST(0). */
int fiadd (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  DA  /0 RMMemOnly     | Add m32int to ST(0) and store result in ST(0). */
 /*  DA  /0 RMMemOnly     | Add m32int to ST(0) and store result in ST(0). */
int fiadd (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  DE  /2 RMMemOnly     | Compare ST(0) with m16int. */
 /*  DE  /2 RMMemOnly     | Compare ST(0) with m16int. */
int ficom (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  DA  /2 RMMemOnly     | Compare ST(0) with m32int. */
 /*  DA  /2 RMMemOnly     | Compare ST(0) with m32int. */
int ficom (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  DE  /3 RMMemOnly     | Compare ST(0) with m16int and pop stack register. */
 /*  DE  /3 RMMemOnly     | Compare ST(0) with m16int and pop stack register. */
int ficomp (code_ptr p,
            const WordPtr (&   modrm_rm));
/*  DA  /3 RMMemOnly     | Compare ST(0) with m32int and pop stack register. */
 /*  DA  /3 RMMemOnly     | Compare ST(0) with m32int and pop stack register. */
int ficomp (code_ptr p,
            const DwordPtr (&   modrm_rm));
/*  DE  /6 RMMemOnly     | Divide ST(0) by m64int and store result in ST(0). */
 /*  DE  /6 RMMemOnly     | Divide ST(0) by m64int and store result in ST(0). */
int fidiv (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  DA  /6 RMMemOnly     | Divide ST(0) by m32int and store result in ST(0). */
 /*  DA  /6 RMMemOnly     | Divide ST(0) by m32int and store result in ST(0). */
int fidiv (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  DE  /7 RMMemOnly     | Divide m16int by ST(0) and store result in ST(0). */
 /*  DE  /7 RMMemOnly     | Divide m16int by ST(0) and store result in ST(0). */
int fidivr (code_ptr p,
            const WordPtr (&   modrm_rm));
/*  DA  /7 RMMemOnly     | Divide m32int by ST(0) and store result in ST(0). */
 /*  DA  /7 RMMemOnly     | Divide m32int by ST(0) and store result in ST(0). */
int fidivr (code_ptr p,
            const DwordPtr (&   modrm_rm));
/*  DF  /0 RMMemOnly     | Push m16int onto the FPU register stack. */
 /*  DF  /0 RMMemOnly     | Push m16int onto the FPU register stack. */
int fild (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  DB  /0 RMMemOnly     | Push m32int onto the FPU register stack. */
 /*  DB  /0 RMMemOnly     | Push m32int onto the FPU register stack. */
int fild (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  DF  /5 RMMemOnly     | Push m64int onto the FPU register stack. */
 /*  DF  /5 RMMemOnly     | Push m64int onto the FPU register stack. */
int fild (code_ptr p,
          const QwordPtr (&   modrm_rm));
/*  DE  /1 RMMemOnly     | Multiply ST(0) by m16int and store result in ST(0). */
 /*  DE  /1 RMMemOnly     | Multiply ST(0) by m16int and store result in ST(0). */
int fimul (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  DA  /1 RMMemOnly     | Multiply ST(0) by m32int and store result in ST(0). */
 /*  DA  /1 RMMemOnly     | Multiply ST(0) by m32int and store result in ST(0). */
int fimul (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  D9 F7        | Increment the TOP field in the FPU status register. */
 /*  D9 F7        | Increment the TOP field in the FPU status register. */
int fincstp (code_ptr p);
/*  9B DB E3        | Initialize FPU after checking for pending unmasked floating-point exceptions. */
 /*  9B DB E3        | Initialize FPU after checking for pending unmasked floating-point exceptions. */
int finit (code_ptr p);
/*  DF  /2 RMMemOnly     | Store ST(0) in m16int. */
 /*  DF  /2 RMMemOnly     | Store ST(0) in m16int. */
int fist (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  DB  /2 RMMemOnly     | Store ST(0) in m32int. */
 /*  DB  /2 RMMemOnly     | Store ST(0) in m32int. */
int fist (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  DF  /3 RMMemOnly     | Store ST(0) in m16int and pop register stack. */
 /*  DF  /3 RMMemOnly     | Store ST(0) in m16int and pop register stack. */
int fistp (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  DB  /3 RMMemOnly     | Store ST(0) in m32int and pop register stack. */
 /*  DB  /3 RMMemOnly     | Store ST(0) in m32int and pop register stack. */
int fistp (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  DF  /7 RMMemOnly     | Store ST(0) in m64int and pop register stack. */
 /*  DF  /7 RMMemOnly     | Store ST(0) in m64int and pop register stack. */
int fistp (code_ptr p,
           const QwordPtr (&   modrm_rm));
/*  DF  /1 RMMemOnly     | Store ST(0) in m16int with truncation. */
 /*  DF  /1 RMMemOnly     | Store ST(0) in m16int with truncation. */
int fisttp (code_ptr p,
            const WordPtr (&   modrm_rm));
/*  DB  /1 RMMemOnly     | Store ST(0) in m32int with truncation. */
 /*  DB  /1 RMMemOnly     | Store ST(0) in m32int with truncation. */
int fisttp (code_ptr p,
            const DwordPtr (&   modrm_rm));
/*  DD  /1 RMMemOnly     | Store ST(0) in m64int with truncation. */
 /*  DD  /1 RMMemOnly     | Store ST(0) in m64int with truncation. */
int fisttp (code_ptr p,
            const QwordPtr (&   modrm_rm));
/*  DE  /4 RMMemOnly     | Subtract m16int from ST(0) and store result in ST(0). */
 /*  DE  /4 RMMemOnly     | Subtract m16int from ST(0) and store result in ST(0). */
int fisub (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  DA  /4 RMMemOnly     | Subtract m32int from ST(0) and store result in ST(0). */
 /*  DA  /4 RMMemOnly     | Subtract m32int from ST(0) and store result in ST(0). */
int fisub (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  DE  /5 RMMemOnly     | Subtract ST(0) from m16int and store result in ST(0). */
 /*  DE  /5 RMMemOnly     | Subtract ST(0) from m16int and store result in ST(0). */
int fisubr (code_ptr p,
            const WordPtr (&   modrm_rm));
/*  DA  /5 RMMemOnly     | Subtract ST(0) from m32int and store result in ST(0). */
 /*  DA  /5 RMMemOnly     | Subtract ST(0) from m32int and store result in ST(0). */
int fisubr (code_ptr p,
            const DwordPtr (&   modrm_rm));
/*  D9 C0    +i    | Push ST(i) onto the FPU register stack. */
 /*  D9 C0    +i    | Push ST(i) onto the FPU register stack. */
int fld (code_ptr p,
         const StReg (&   radd));
/*  D9  /0 RMMemOnly     | Push m32fp onto the FPU register stack. */
 /*  D9  /0 RMMemOnly     | Push m32fp onto the FPU register stack. */
int fld (code_ptr p,
         const DwordPtr (&   modrm_rm));
/*  DD  /0 RMMemOnly     | Push m64fp onto the FPU register stack. */
 /*  DD  /0 RMMemOnly     | Push m64fp onto the FPU register stack. */
int fld (code_ptr p,
         const QwordPtr (&   modrm_rm));
/*  DB  /5 RMMemOnly     | Push m80fp onto the FPU register stack. */
 /*  DB  /5 RMMemOnly     | Push m80fp onto the FPU register stack. */
int fld (code_ptr p,
         const TbytePtr (&   modrm_rm));
/*  D9 E8        | Push +1.0 onto the FPU register stack. */
 /*  D9 E8        | Push +1.0 onto the FPU register stack. */
int fld1 (code_ptr p);
/*  D9  /5 RMMemOnly     | Load FPU control word from m2byte. */
 /*  D9  /5 RMMemOnly     | Load FPU control word from m2byte. */
int fldcw (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  D9  /4 RMMemOnly     | Load FPU environment from m14byte or m28byte. */
 /*  D9  /4 RMMemOnly     | Load FPU environment from m14byte or m28byte. */
int fldenv (code_ptr p,
            const VoidPtr (&   modrm_rm));
/*  D9 EA        | Push log2e onto the FPU register stack. */
 /*  D9 EA        | Push log2e onto the FPU register stack. */
int fldl2e (code_ptr p);
/*  D9 E9        | Push log210 onto the FPU register stack. */
 /*  D9 E9        | Push log210 onto the FPU register stack. */
int fldl2t (code_ptr p);
/*  D9 EC        | Push log102 onto the FPU register stack. */
 /*  D9 EC        | Push log102 onto the FPU register stack. */
int fldlg2 (code_ptr p);
/*  D9 ED        | Push loge2 onto the FPU register stack. */
 /*  D9 ED        | Push loge2 onto the FPU register stack. */
int fldln2 (code_ptr p);
/*  D9 EB        | Push π onto the FPU register stack. */
 /*  D9 EB        | Push π onto the FPU register stack. */
int fldpi (code_ptr p);
/*  D9 EE        | Push +0.0 onto the FPU register stack. */
 /*  D9 EE        | Push +0.0 onto the FPU register stack. */
int fldz (code_ptr p);
/*  D8  /1 RMMemOnly     | Multiply ST(0) by m32fp and store result in ST(0). */
 /*  D8  /1 RMMemOnly     | Multiply ST(0) by m32fp and store result in ST(0). */
int fmul (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  DC  /1 RMMemOnly     | Multiply ST(0) by m64fp and store result in ST(0). */
 /*  DC  /1 RMMemOnly     | Multiply ST(0) by m64fp and store result in ST(0). */
int fmul (code_ptr p,
          const QwordPtr (&   modrm_rm));
/*  D8 C8    +i    | Multiply ST(0) by ST(i) and store result in ST(0). */
 /*  D8 C8    +i    | Multiply ST(0) by ST(i) and store result in ST(0). */
int fmul (code_ptr p,
          const RegST0 (&   unused),
          const StReg (&   radd));
/*  DC C8    +i    | Multiply ST(i) by ST(0) and store result in ST(i). */
 /*  DC C8    +i    | Multiply ST(i) by ST(0) and store result in ST(i). */
int fmul (code_ptr p,
          const StReg_m_ST0 (&   radd),
          const RegST0 (&   unused));
/*  DE C9        | Multiply ST(1) by ST(0), store result in ST(1), and pop the register stack. */
 /*  DE C9        | Multiply ST(1) by ST(0), store result in ST(1), and pop the register stack. */
int fmulp (code_ptr p);
/*  DE C8    +i    | Multiply ST(i) by ST(0), store result in ST(i), and pop the register stack. */
 /*  DE C8    +i    | Multiply ST(i) by ST(0), store result in ST(i), and pop the register stack. */
int fmulp (code_ptr p,
           const StReg (&   radd),
           const RegST0 (&   unused));
/*  DB E2        | Clear floating-point exception flags without checking for pending unmasked floating-point exceptions. */
 /*  DB E2        | Clear floating-point exception flags without checking for pending unmasked floating-point exceptions. */
int fnclex (code_ptr p);
/*  DB E3        | Initialize FPU without checking for pending unmasked floating-point exceptions. */
 /*  DB E3        | Initialize FPU without checking for pending unmasked floating-point exceptions. */
int fninit (code_ptr p);
/*  D9 D0        | No operation is performed. */
 /*  D9 D0        | No operation is performed. */
int fnop (code_ptr p);
/*  DD  /6 RMMemOnly     | Store FPU environment to m94byte or m108byte without checking for pending unmasked floating-point exceptions. Then re-initialize the FPU. */
 /*  DD  /6 RMMemOnly     | Store FPU environment to m94byte or m108byte without checking for pending unmasked floating-point exceptions. Then re-initialize the FPU. */
int fnsave (code_ptr p,
            const VoidPtr (&   modrm_rm));
/*  D9  /7 RMMemOnly     | Store FPU control word to m2byte without checking for pending unmasked floating-point exceptions. */
 /*  D9  /7 RMMemOnly     | Store FPU control word to m2byte without checking for pending unmasked floating-point exceptions. */
int fnstcw (code_ptr p,
            const WordPtr (&   modrm_rm));
/*  D9  /6 RMMemOnly     | Store FPU environment to m14byte or m28byte without checking for pending unmasked floating-point exceptions. Then mask all floating- point exceptions. */
 /*  D9  /6 RMMemOnly     | Store FPU environment to m14byte or m28byte without checking for pending unmasked floating-point exceptions. Then mask all floating- point exceptions. */
int fnstenv (code_ptr p,
             const VoidPtr (&   modrm_rm));
/*  DF E0        | Store FPU status word in AX register without checking for pending unmasked floating- point exceptions. */
 /*  DF E0        | Store FPU status word in AX register without checking for pending unmasked floating- point exceptions. */
int fnstsw (code_ptr p,
            const RegAX (&   unused));
/*  DD  /7 RMMemOnly     | Store FPU status word at m2byte without checking for pending unmasked floating- point exceptions. */
 /*  DD  /7 RMMemOnly     | Store FPU status word at m2byte without checking for pending unmasked floating- point exceptions. */
int fnstsw (code_ptr p,
            const WordPtr (&   modrm_rm));
/*  D9 F3        | Replace ST(1) with arctan(ST(1)/ST(0)) and pop the register stack. */
 /*  D9 F3        | Replace ST(1) with arctan(ST(1)/ST(0)) and pop the register stack. */
int fpatan (code_ptr p);
/*  D9 F8        | Replace ST(0) with the remainder obtained from dividing ST(0) by ST(1). */
 /*  D9 F8        | Replace ST(0) with the remainder obtained from dividing ST(0) by ST(1). */
int fprem (code_ptr p);
/*  D9 F5        | Replace ST(0) with the IEEE remainder obtained from dividing ST(0) by ST(1). */
 /*  D9 F5        | Replace ST(0) with the IEEE remainder obtained from dividing ST(0) by ST(1). */
int fprem1 (code_ptr p);
/*  D9 F2        | Replace ST(0) with its tangent and push 1 onto the FPU stack. */
 /*  D9 F2        | Replace ST(0) with its tangent and push 1 onto the FPU stack. */
int fptan (code_ptr p);
/*  D9 FC        | Round ST(0) to an integer. */
 /*  D9 FC        | Round ST(0) to an integer. */
int frndint (code_ptr p);
/*  DD  /4 RMMemOnly     | Load FPU state from m94byte or m108byte. */
 /*  DD  /4 RMMemOnly     | Load FPU state from m94byte or m108byte. */
int frstor (code_ptr p,
            const VoidPtr (&   modrm_rm));
/*  9B DD  /6 RMMemOnly     | Store FPU state to m94byte or m108byte after checking for pending unmasked floating- point exceptions. Then re- initialize the FPU. */
 /*  9B DD  /6 RMMemOnly     | Store FPU state to m94byte or m108byte after checking for pending unmasked floating- point exceptions. Then re- initialize the FPU. */
int fsave (code_ptr p,
           const VoidPtr (&   modrm_rm));
/*  D9 FD        | Scale ST(0) by ST(1). */
 /*  D9 FD        | Scale ST(0) by ST(1). */
int fscale (code_ptr p);
/*  D9 FE        | Replace ST(0) with its sine. */
 /*  D9 FE        | Replace ST(0) with its sine. */
int fsin (code_ptr p);
/*  D9 FB        | Compute the sine and cosine of ST(0); replace ST(0) with the sine, and push the cosine onto the register stack. */
 /*  D9 FB        | Compute the sine and cosine of ST(0); replace ST(0) with the sine, and push the cosine onto the register stack. */
int fsincos (code_ptr p);
/*  D9 FA        | Computes square root of ST(0) and stores the result in ST(0). */
 /*  D9 FA        | Computes square root of ST(0) and stores the result in ST(0). */
int fsqrt (code_ptr p);
/*  DD D0    +i    | Copy ST(0) to ST(i). */
 /*  DD D0    +i    | Copy ST(0) to ST(i). */
int fst (code_ptr p,
         const StReg (&   radd));
/*  D9  /2 RMMemOnly     | Copy ST(0) to m32fp. */
 /*  D9  /2 RMMemOnly     | Copy ST(0) to m32fp. */
int fst (code_ptr p,
         const DwordPtr (&   modrm_rm));
/*  DD  /2 RMMemOnly     | Copy ST(0) to m64fp. */
 /*  DD  /2 RMMemOnly     | Copy ST(0) to m64fp. */
int fst (code_ptr p,
         const QwordPtr (&   modrm_rm));
/*  9B D9  /7 RMMemOnly     | Store FPU control word to m2byte after checking for pending unmasked floating-point exceptions. */
 /*  9B D9  /7 RMMemOnly     | Store FPU control word to m2byte after checking for pending unmasked floating-point exceptions. */
int fstcw (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  9B D9  /6 RMMemOnly     | Store FPU environment to m14byte or m28byte after checking for pending unmasked floating-point exceptions. Then mask all floating- point exceptions. */
 /*  9B D9  /6 RMMemOnly     | Store FPU environment to m14byte or m28byte after checking for pending unmasked floating-point exceptions. Then mask all floating- point exceptions. */
int fstenv (code_ptr p,
            const VoidPtr (&   modrm_rm));
/*  DD D8    +i    | Copy ST(0) to ST(i) and pop register stack. */
 /*  DD D8    +i    | Copy ST(0) to ST(i) and pop register stack. */
int fstp (code_ptr p,
          const StReg (&   radd));
/*  D9  /3 RMMemOnly     | Copy ST(0) to m32fp and pop register stack. */
 /*  D9  /3 RMMemOnly     | Copy ST(0) to m32fp and pop register stack. */
int fstp (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  DD  /3 RMMemOnly     | Copy ST(0) to m64fp and pop register stack. */
 /*  DD  /3 RMMemOnly     | Copy ST(0) to m64fp and pop register stack. */
int fstp (code_ptr p,
          const QwordPtr (&   modrm_rm));
/*  DB  /7 RMMemOnly     | Copy ST(0) to m80fp and pop register stack. */
 /*  DB  /7 RMMemOnly     | Copy ST(0) to m80fp and pop register stack. */
int fstp (code_ptr p,
          const TbytePtr (&   modrm_rm));
/*  9B DF E0        | Store FPU status word in AX register after checking for pending unmasked floating- point exceptions. */
 /*  9B DF E0        | Store FPU status word in AX register after checking for pending unmasked floating- point exceptions. */
int fstsw (code_ptr p,
           const RegAX (&   unused));
/*  9B DD  /7 RMMemOnly     | Store FPU status word at m2byte after checking for pending unmasked floating- point exceptions. */
 /*  9B DD  /7 RMMemOnly     | Store FPU status word at m2byte after checking for pending unmasked floating- point exceptions. */
int fstsw (code_ptr p,
           const WordPtr (&   modrm_rm));
/*  D8  /4 RMMemOnly     | Subtract m32fp from ST(0) and store result in ST(0). */
 /*  D8  /4 RMMemOnly     | Subtract m32fp from ST(0) and store result in ST(0). */
int fsub (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  DC  /4 RMMemOnly     | Subtract m64fp from ST(0) and store result in ST(0). */
 /*  DC  /4 RMMemOnly     | Subtract m64fp from ST(0) and store result in ST(0). */
int fsub (code_ptr p,
          const QwordPtr (&   modrm_rm));
/*  D8 E0    +i    | Subtract ST(i) from ST(0) and store result in ST(0). */
 /*  D8 E0    +i    | Subtract ST(i) from ST(0) and store result in ST(0). */
int fsub (code_ptr p,
          const RegST0 (&   unused),
          const StReg (&   radd));
/*  DC E8    +i    | Subtract ST(0) from ST(i) and store result in ST(i). */
 /*  DC E8    +i    | Subtract ST(0) from ST(i) and store result in ST(i). */
int fsub (code_ptr p,
          const StReg_m_ST0 (&   radd),
          const RegST0 (&   unused));
/*  DE E9        | Subtract ST(0) from ST(1), store result in ST(1), and pop register stack. */
 /*  DE E9        | Subtract ST(0) from ST(1), store result in ST(1), and pop register stack. */
int fsubp (code_ptr p);
/*  DE E8    +i    | Subtract ST(0) from ST(i), store result in ST(i), and pop register stack. */
 /*  DE E8    +i    | Subtract ST(0) from ST(i), store result in ST(i), and pop register stack. */
int fsubp (code_ptr p,
           const StReg (&   radd),
           const RegST0 (&   unused));
/*  D8  /5 RMMemOnly     | Subtract ST(0) from m32fp and store result in ST(0). */
 /*  D8  /5 RMMemOnly     | Subtract ST(0) from m32fp and store result in ST(0). */
int fsubr (code_ptr p,
           const DwordPtr (&   modrm_rm));
/*  DC  /5 RMMemOnly     | Subtract ST(0) from m64fp and store result in ST(0). */
 /*  DC  /5 RMMemOnly     | Subtract ST(0) from m64fp and store result in ST(0). */
int fsubr (code_ptr p,
           const QwordPtr (&   modrm_rm));
/*  D8 E8    +i    | Subtract ST(0) from ST(i) and store result in ST(0). */
 /*  D8 E8    +i    | Subtract ST(0) from ST(i) and store result in ST(0). */
int fsubr (code_ptr p,
           const RegST0 (&   unused),
           const StReg (&   radd));
/*  DC E0    +i    | Subtract ST(i) from ST(0) and store result in ST(i). */
 /*  DC E0    +i    | Subtract ST(i) from ST(0) and store result in ST(i). */
int fsubr (code_ptr p,
           const StReg_m_ST0 (&   radd),
           const RegST0 (&   unused));
/*  DE E1        | Subtract ST(1) from ST(0), store result in ST(1), and pop register stack. */
 /*  DE E1        | Subtract ST(1) from ST(0), store result in ST(1), and pop register stack. */
int fsubrp (code_ptr p);
/*  DE E0    +i    | Subtract ST(i) from ST(0), store result in ST(i), and pop register stack. */
 /*  DE E0    +i    | Subtract ST(i) from ST(0), store result in ST(i), and pop register stack. */
int fsubrp (code_ptr p,
            const StReg (&   radd),
            const RegST0 (&   unused));
/*  D9 E4        | Compare ST(0) with 0.0. */
 /*  D9 E4        | Compare ST(0) with 0.0. */
int ftst (code_ptr p);
/*  DD E1        | Compare ST(0) with ST(1). */
 /*  DD E1        | Compare ST(0) with ST(1). */
int fucom (code_ptr p);
/*  DD E0    +i    | Compare ST(0) with ST(i). */
 /*  DD E0    +i    | Compare ST(0) with ST(i). */
int fucom (code_ptr p,
           const StReg (&   radd));
/*  DB E8    +i    | Compare ST(0) with ST(i), check for ordered values, and set status flags accordingly. */
 /*  DB E8    +i    | Compare ST(0) with ST(i), check for ordered values, and set status flags accordingly. */
int fucomi (code_ptr p,
            const RegST0 (&   unused),
            const StReg (&   radd));
/*  DF E8    +i    | Compare ST(0) with ST(i), check for ordered values, set status flags accordingly, and pop register stack. */
 /*  DF E8    +i    | Compare ST(0) with ST(i), check for ordered values, set status flags accordingly, and pop register stack. */
int fucomip (code_ptr p,
             const RegST0 (&   unused),
             const StReg (&   radd));
/*  DD E9        | Compare ST(0) with ST(1) and pop register stack. */
 /*  DD E9        | Compare ST(0) with ST(1) and pop register stack. */
int fucomp (code_ptr p);
/*  DD E8    +i    | Compare ST(0) with ST(i) and pop register stack. */
 /*  DD E8    +i    | Compare ST(0) with ST(i) and pop register stack. */
int fucomp (code_ptr p,
            const StReg (&   radd));
/*  DA E9        | Compare ST(0) with ST(1) and pop register stack twice. */
 /*  DA E9        | Compare ST(0) with ST(1) and pop register stack twice. */
int fucompp (code_ptr p);
/*  9B        | Check pending unmasked floating-point exceptions. */
 /*  9B        | Check pending unmasked floating-point exceptions. */
int fwait (code_ptr p);
/*  D9 E5        | Classify value or number in ST(0). */
 /*  D9 E5        | Classify value or number in ST(0). */
int fxam (code_ptr p);
/*  D9 C9        | Exchange the contents of ST(0) and ST(1). */
 /*  D9 C9        | Exchange the contents of ST(0) and ST(1). */
int fxch (code_ptr p);
/*  D9 C8    +i    | Exchange the contents of ST(0) and ST(i). */
 /*  D9 C8    +i    | Exchange the contents of ST(0) and ST(i). */
int fxch (code_ptr p,
          const StReg (&   radd));
/*  0F AE  /1 RMMemOnly     | Restore the x87 FPU, MMX, XMM, and MXCSR register state from m512byte. */
 /*  0F AE  /1 RMMemOnly     | Restore the x87 FPU, MMX, XMM, and MXCSR register state from m512byte. */
int fxrstor (code_ptr p,
             const VoidPtr (&   modrm_rm));
/*  0F AE  /0 RMMemOnly     | Save the x87 FPU, MMX, XMM, and MXCSR register state to m512byte. */
 /*  0F AE  /0 RMMemOnly     | Save the x87 FPU, MMX, XMM, and MXCSR register state to m512byte. */
int fxsave (code_ptr p,
            const VoidPtr (&   modrm_rm));
/*  D9 F4        | Separate value in ST(0) into exponent and significand, store exponent in ST(0), and push the significand onto the register stack. */
 /*  D9 F4        | Separate value in ST(0) into exponent and significand, store exponent in ST(0), and push the significand onto the register stack. */
int fxtract (code_ptr p);
/*  D9 F1        | Replace ST(1) with (ST(1) ∗ log2ST(0)) and pop the register stack. */
 /*  D9 F1        | Replace ST(1) with (ST(1) ∗ log2ST(0)) and pop the register stack. */
int fyl2x (code_ptr p);
/*  D9 F9        | Replace ST(1) with ST(1) ∗ log2(ST(0) + 1.0) and pop the register stack. */
 /*  D9 F9        | Replace ST(1) with ST(1) ∗ log2(ST(0) + 1.0) and pop the register stack. */
int fyl2xp1 (code_ptr p);
/*  0F 37        | See Intel Instruction Set Reference, Chapter 6 for details. */
 /*  0F 37        | See Intel Instruction Set Reference, Chapter 6 for details. */
int getsec (code_ptr p);
/* 66 0F 7C  /r RMBoth     | Horizontal add packed double-precision floating- point values from xmm2/m128 to xmm1. */
 /* 66 0F 7C  /r RMBoth     | Horizontal add packed double-precision floating- point values from xmm2/m128 to xmm1. */
int haddpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 7C  /r RMBoth     | Horizontal add packed double-precision floating- point values from xmm2/m128 to xmm1. */
 /* 66 0F 7C  /r RMBoth     | Horizontal add packed double-precision floating- point values from xmm2/m128 to xmm1. */
int haddpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* F2 0F 7C  /r RMBoth     | Horizontal add packed single-precision floating- point values from xmm2/m128 to xmm1. */
 /* F2 0F 7C  /r RMBoth     | Horizontal add packed single-precision floating- point values from xmm2/m128 to xmm1. */
int haddps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* F2 0F 7C  /r RMBoth     | Horizontal add packed single-precision floating- point values from xmm2/m128 to xmm1. */
 /* F2 0F 7C  /r RMBoth     | Horizontal add packed single-precision floating- point values from xmm2/m128 to xmm1. */
int haddps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  F4        | Halt */
 /*  F4        | Halt */
int hlt (code_ptr p);
/* 66 0F 7D  /r RMBoth     | Horizontal subtract packed double-precision floating- point values from xmm2/m128 to xmm1. */
 /* 66 0F 7D  /r RMBoth     | Horizontal subtract packed double-precision floating- point values from xmm2/m128 to xmm1. */
int hsubpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 7D  /r RMBoth     | Horizontal subtract packed double-precision floating- point values from xmm2/m128 to xmm1. */
 /* 66 0F 7D  /r RMBoth     | Horizontal subtract packed double-precision floating- point values from xmm2/m128 to xmm1. */
int hsubpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* F2 0F 7D  /r RMBoth     | Horizontal subtract packed single-precision floating- point values from xmm2/m128 to xmm1. */
 /* F2 0F 7D  /r RMBoth     | Horizontal subtract packed single-precision floating- point values from xmm2/m128 to xmm1. */
int hsubps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* F2 0F 7D  /r RMBoth     | Horizontal subtract packed single-precision floating- point values from xmm2/m128 to xmm1. */
 /* F2 0F 7D  /r RMBoth     | Horizontal subtract packed single-precision floating- point values from xmm2/m128 to xmm1. */
int hsubps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  F6  /7 RMBoth     | Signed divide AX by r/m8, with result stored in: AL ← Quotient, AH ← Remainder. */
 /*  F6  /7 RMBoth     | Signed divide AX by r/m8, with result stored in: AL ← Quotient, AH ← Remainder. */
int idiv (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  F6  /7 RMBoth     | Signed divide AX by r/m8, with result stored in: AL ← Quotient, AH ← Remainder. */
 /*  F6  /7 RMBoth     | Signed divide AX by r/m8, with result stored in: AL ← Quotient, AH ← Remainder. */
int idiv (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  F7  /7 RMBoth   op16  | Signed divide DX:AX by r/m16, with result stored in AX ← Quotient, DX ← Remainder. */
 /*  F7  /7 RMBoth   op16  | Signed divide DX:AX by r/m16, with result stored in AX ← Quotient, DX ← Remainder. */
int idiv (code_ptr p,
          const WordReg (&   modrm_rm));
/*  F7  /7 RMBoth   op16  | Signed divide DX:AX by r/m16, with result stored in AX ← Quotient, DX ← Remainder. */
 /*  F7  /7 RMBoth   op16  | Signed divide DX:AX by r/m16, with result stored in AX ← Quotient, DX ← Remainder. */
int idiv (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  F7  /7 RMBoth   op32  | Signed divide EDX:EAX by r/m32, with result stored in EAX ← Quotient, EDX ← Remainder. */
 /*  F7  /7 RMBoth   op32  | Signed divide EDX:EAX by r/m32, with result stored in EAX ← Quotient, EDX ← Remainder. */
int idiv (code_ptr p,
          const DwordReg (&   modrm_rm));
/*  F7  /7 RMBoth   op32  | Signed divide EDX:EAX by r/m32, with result stored in EAX ← Quotient, EDX ← Remainder. */
 /*  F7  /7 RMBoth   op32  | Signed divide EDX:EAX by r/m32, with result stored in EAX ← Quotient, EDX ← Remainder. */
int idiv (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  F6  /5 RMBoth     | AX← AL ∗ r/m byte. */
 /*  F6  /5 RMBoth     | AX← AL ∗ r/m byte. */
int imul (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  F6  /5 RMBoth     | AX← AL ∗ r/m byte. */
 /*  F6  /5 RMBoth     | AX← AL ∗ r/m byte. */
int imul (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  F7  /5 RMBoth   op16  | DX:AX ← AX ∗ r/m word. */
 /*  F7  /5 RMBoth   op16  | DX:AX ← AX ∗ r/m word. */
int imul (code_ptr p,
          const WordReg (&   modrm_rm));
/*  F7  /5 RMBoth   op16  | DX:AX ← AX ∗ r/m word. */
 /*  F7  /5 RMBoth   op16  | DX:AX ← AX ∗ r/m word. */
int imul (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  F7  /5 RMBoth   op32  | EDX:EAX ← EAX ∗ r/m32. */
 /*  F7  /5 RMBoth   op32  | EDX:EAX ← EAX ∗ r/m32. */
int imul (code_ptr p,
          const DwordReg (&   modrm_rm));
/*  F7  /5 RMBoth   op32  | EDX:EAX ← EAX ∗ r/m32. */
 /*  F7  /5 RMBoth   op32  | EDX:EAX ← EAX ∗ r/m32. */
int imul (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  0F AF  /r RMBoth   op16  | word register ← word register ∗ r/m16. */
 /*  0F AF  /r RMBoth   op16  | word register ← word register ∗ r/m16. */
int imul (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordReg (&   modrm_rm));
/*  0F AF  /r RMBoth   op16  | word register ← word register ∗ r/m16. */
 /*  0F AF  /r RMBoth   op16  | word register ← word register ∗ r/m16. */
int imul (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordPtr (&   modrm_rm));
/*  0F AF  /r RMBoth   op32  | doubleword register ← doubleword register ∗ r/m32. */
 /*  0F AF  /r RMBoth   op32  | doubleword register ← doubleword register ∗ r/m32. */
int imul (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordReg (&   modrm_rm));
/*  0F AF  /r RMBoth   op32  | doubleword register ← doubleword register ∗ r/m32. */
 /*  0F AF  /r RMBoth   op32  | doubleword register ← doubleword register ∗ r/m32. */
int imul (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordPtr (&   modrm_rm));
/*  6B  /r RMBoth  ib op16  | word register ← r/m16 ∗ sign-extended immediate byte. */
 /*  6B  /r RMBoth  ib op16  | word register ← r/m16 ∗ sign-extended immediate byte. */
int imul (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordReg (&   modrm_rm),
          imm8_t imm);
/*  6B  /r RMBoth  ib op16  | word register ← r/m16 ∗ sign-extended immediate byte. */
 /*  6B  /r RMBoth  ib op16  | word register ← r/m16 ∗ sign-extended immediate byte. */
int imul (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordPtr (&   modrm_rm),
          imm8_t imm);
/*  69  /r RMBoth  iw op16  | word register ← r/m16 ∗ immediate word. */
 /*  69  /r RMBoth  iw op16  | word register ← r/m16 ∗ immediate word. */
int imul (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordReg (&   modrm_rm),
          imm16_t imm);
/*  69  /r RMBoth  iw op16  | word register ← r/m16 ∗ immediate word. */
 /*  69  /r RMBoth  iw op16  | word register ← r/m16 ∗ immediate word. */
int imul (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordPtr (&   modrm_rm),
          imm16_t imm);
/*  6B  /r RMBoth  ib op32  | doubleword register ← r/m32 ∗ sign-extended immediate byte. */
 /*  6B  /r RMBoth  ib op32  | doubleword register ← r/m32 ∗ sign-extended immediate byte. */
int imul (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordReg (&   modrm_rm),
          imm8_t imm);
/*  6B  /r RMBoth  ib op32  | doubleword register ← r/m32 ∗ sign-extended immediate byte. */
 /*  6B  /r RMBoth  ib op32  | doubleword register ← r/m32 ∗ sign-extended immediate byte. */
int imul (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordPtr (&   modrm_rm),
          imm8_t imm);
/*  69  /r RMBoth  id op32  | doubleword register ← r/m32 ∗ immediate doubleword. */
 /*  69  /r RMBoth  id op32  | doubleword register ← r/m32 ∗ immediate doubleword. */
int imul (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordReg (&   modrm_rm),
          imm32_t imm);
/*  69  /r RMBoth  id op32  | doubleword register ← r/m32 ∗ immediate doubleword. */
 /*  69  /r RMBoth  id op32  | doubleword register ← r/m32 ∗ immediate doubleword. */
int imul (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordPtr (&   modrm_rm),
          imm32_t imm);
/*  E4     ib   | Input byte from imm8 I/O port address into AL. */
 /*  E4     ib   | Input byte from imm8 I/O port address into AL. */
int in (code_ptr p,
        const RegAL (&   unused),
        imm8_t imm);
/*  EC        | Input byte from I/O port in DX into AL. */
 /*  EC        | Input byte from I/O port in DX into AL. */
int in (code_ptr p,
        const RegAL (&   unused1),
        const RegDX (&   unused2));
/*  E5     ib op16  | Input word from imm8 I/O port address into AX. */
 /*  E5     ib op16  | Input word from imm8 I/O port address into AX. */
int in (code_ptr p,
        const RegAX (&   unused),
        imm8_t imm);
/*  ED      op16  | Input word from I/O port in DX into AX. */
 /*  ED      op16  | Input word from I/O port in DX into AX. */
int in (code_ptr p,
        const RegAX (&   unused1),
        const RegDX (&   unused2));
/*  E5     ib op32  | Input dword from imm8 I/O port address into EAX. */
 /*  E5     ib op32  | Input dword from imm8 I/O port address into EAX. */
int in (code_ptr p,
        const RegEAX (&   unused),
        imm8_t imm);
/*  ED      op32  | Input doubleword from I/O port in DX into EAX. */
 /*  ED      op32  | Input doubleword from I/O port in DX into EAX. */
int in (code_ptr p,
        const RegEAX (&   unused1),
        const RegDX (&   unused2));
/*  40    +rd  op32  | Increment doubleword register by 1. */
 /*  40    +rd  op32  | Increment doubleword register by 1. */
int inc (code_ptr p,
         const DwordReg (&   radd));
/*  FF  /0 RMBoth   op32  | Increment r/m doubleword by 1. */
 /*  FF  /0 RMBoth   op32  | Increment r/m doubleword by 1. */
int inc (code_ptr p,
         const DwordPtr (&   modrm_rm));
/*  40    +rw  op16  | Increment word register by 1. */
 /*  40    +rw  op16  | Increment word register by 1. */
int inc (code_ptr p,
         const WordReg (&   radd));
/*  FF  /0 RMBoth   op16  | Increment r/m word by 1. */
 /*  FF  /0 RMBoth   op16  | Increment r/m word by 1. */
int inc (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  FE  /0 RMBoth     | Increment r/m byte by 1. */
 /*  FE  /0 RMBoth     | Increment r/m byte by 1. */
int inc (code_ptr p,
         const ByteReg (&   modrm_rm));
/*  FE  /0 RMBoth     | Increment r/m byte by 1. */
 /*  FE  /0 RMBoth     | Increment r/m byte by 1. */
int inc (code_ptr p,
         const BytePtr (&   modrm_rm));
/*  6C        | Input byte from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.* */
 /*  6C        | Input byte from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.* */
int ins (code_ptr p,
         const BytePtr_ES_EDI (&   unused1),
         const RegDX (&   unused2));
/*  6C        | Input byte from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.* */
 /*  6C        | Input byte from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.* */
int ins (code_ptr p,
         const BytePtr_ES_DI (&   unused1),
         const RegDX (&   unused2));
/*  6D      op16  | Input word from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
 /*  6D      op16  | Input word from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
int ins (code_ptr p,
         const WordPtr_ES_EDI (&   unused1),
         const RegDX (&   unused2));
/*  6D      op16  | Input word from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
 /*  6D      op16  | Input word from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
int ins (code_ptr p,
         const WordPtr_ES_DI (&   unused1),
         const RegDX (&   unused2));
/*  6D      op32  | Input doubleword from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
 /*  6D      op32  | Input doubleword from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
int ins (code_ptr p,
         const DwordPtr_ES_EDI (&   unused1),
         const RegDX (&   unused2));
/*  6D      op32  | Input doubleword from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
 /*  6D      op32  | Input doubleword from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
int ins (code_ptr p,
         const DwordPtr_ES_DI (&   unused1),
         const RegDX (&   unused2));
/*  6C        | Input byte from I/O port specified in DX into memory location specified with ES:(E)DI or RDI.1 */
 /*  6C        | Input byte from I/O port specified in DX into memory location specified with ES:(E)DI or RDI.1 */
int insb (code_ptr p);
/*  6D      op32  | Input doubleword from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
 /*  6D      op32  | Input doubleword from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
int insd (code_ptr p);
/* 66 0F 3A 21  /r RMBoth  ib   | Insert a single precision floating-point value selected by imm8 from xmm2/m32 into xmm1 at the specified destination element specified by imm8 and zero out destination elements in xmm1 as indicated in imm8. */
 /* 66 0F 3A 21  /r RMBoth  ib   | Insert a single precision floating-point value selected by imm8 from xmm2/m32 into xmm1 at the specified destination element specified by imm8 and zero out destination elements in xmm1 as indicated in imm8. */
int insertps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm),
              imm8_t imm);
/* 66 0F 3A 21  /r RMBoth  ib   | Insert a single precision floating-point value selected by imm8 from xmm2/m32 into xmm1 at the specified destination element specified by imm8 and zero out destination elements in xmm1 as indicated in imm8. */
 /* 66 0F 3A 21  /r RMBoth  ib   | Insert a single precision floating-point value selected by imm8 from xmm2/m32 into xmm1 at the specified destination element specified by imm8 and zero out destination elements in xmm1 as indicated in imm8. */
int insertps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm),
              imm8_t imm);
/*  6D      op16  | Input word from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
 /*  6D      op16  | Input word from I/O port specified in DX into memory location specified in ES:(E)DI or RDI.1 */
int insw (code_ptr p);
/*  CD     ib   | Interrupt vector number specified by immediate byte. */
 /*  CD     ib   | Interrupt vector number specified by immediate byte. */
int int_ (code_ptr p,
          imm8_t imm);
/*  CC        | Interrupt 3—trap to debugger. */
 /*  CC        | Interrupt 3—trap to debugger. */
int int3 (code_ptr p);
/*  CE        | Interrupt 4—if overflow flag is 1. */
 /*  CE        | Interrupt 4—if overflow flag is 1. */
int into (code_ptr p);
/*  0F 08        | Flush internal caches; initiate flushing of external caches. */
 /*  0F 08        | Flush internal caches; initiate flushing of external caches. */
int invd (code_ptr p);
/* 66 0F 38 80  /r RMMemOnly     | Invalidates EPT-derived entries in the TLBs and paging-structure caches (outside 64-bit mode) */
 /* 66 0F 38 80  /r RMMemOnly     | Invalidates EPT-derived entries in the TLBs and paging-structure caches (outside 64-bit mode) */
int invept (code_ptr p,
            const DwordReg (&   modrm_reg),
            const OwordPtr (&   modrm_rm));
/*  0F 01  /7 RMMemOnly     | Invalidate TLB Entry for page that contains m. */
 /*  0F 01  /7 RMMemOnly     | Invalidate TLB Entry for page that contains m. */
int invlpg (code_ptr p,
            const VoidPtr (&   modrm_rm));
/* 66 0F 38 81  /r RMMemOnly     | Invalidates entries in the TLBs and paging-structure caches based on VPID (outside 64-bit mode) */
 /* 66 0F 38 81  /r RMMemOnly     | Invalidates entries in the TLBs and paging-structure caches based on VPID (outside 64-bit mode) */
int invvpid (code_ptr p,
             const DwordReg (&   modrm_reg),
             const OwordPtr (&   modrm_rm));
/*  CF      op16  | Interrupt return (16-bit operand size). */
 /*  CF      op16  | Interrupt return (16-bit operand size). */
int iret (code_ptr p);
/*  CF      op32  | Interrupt return (32-bit operand size). */
 /*  CF      op32  | Interrupt return (32-bit operand size). */
int iretd (code_ptr p);
/*  77     cb   | Jump short if above (CF=0 and ZF=0). */
 /*  77     cb   | Jump short if above (CF=0 and ZF=0). */
int ja (code_ptr p,rel8_t imm);
/*  0F 87     cw op16  | Jump near if above (CF=0 and ZF=0). Not supported in 64-bit mode. */
 /*  0F 87     cw op16  | Jump near if above (CF=0 and ZF=0). Not supported in 64-bit mode. */
int ja (code_ptr p,rel16_t imm);
/*  0F 87     cd op32  | Jump near if above (CF=0 and ZF=0). */
 /*  0F 87     cd op32  | Jump near if above (CF=0 and ZF=0). */
int ja (code_ptr p,rel32_t imm);
/*  73     cb   | Jump short if above or equal (CF=0). */
 /*  73     cb   | Jump short if above or equal (CF=0). */
int jae (code_ptr p,rel8_t imm);
/*  0F 83     cw op16  | Jump near if above or equal (CF=0). Not supported in 64- bit mode. */
 /*  0F 83     cw op16  | Jump near if above or equal (CF=0). Not supported in 64- bit mode. */
int jae (code_ptr p,
         rel16_t imm);
/*  0F 83     cd op32  | Jump near if above or equal (CF=0). */
 /*  0F 83     cd op32  | Jump near if above or equal (CF=0). */
int jae (code_ptr p,
         rel32_t imm);
/*  72     cb   | Jump short if below (CF=1). */
 /*  72     cb   | Jump short if below (CF=1). */
int jb (code_ptr p,rel8_t imm);
/*  0F 82     cw op16  | Jump near if below (CF=1). Not supported in 64-bit mode. */
 /*  0F 82     cw op16  | Jump near if below (CF=1). Not supported in 64-bit mode. */
int jb (code_ptr p,rel16_t imm);
/*  0F 82     cd op32  | Jump near if below (CF=1). */
 /*  0F 82     cd op32  | Jump near if below (CF=1). */
int jb (code_ptr p,rel32_t imm);
/*  76     cb   | Jump short if below or equal (CF=1 or ZF=1). */
 /*  76     cb   | Jump short if below or equal (CF=1 or ZF=1). */
int jbe (code_ptr p,rel8_t imm);
/*  0F 86     cw op16  | Jump near if below or equal (CF=1 or ZF=1). Not supported in 64-bit mode. */
 /*  0F 86     cw op16  | Jump near if below or equal (CF=1 or ZF=1). Not supported in 64-bit mode. */
int jbe (code_ptr p,
         rel16_t imm);
/*  0F 86     cd op32  | Jump near if below or equal (CF=1 or ZF=1). */
 /*  0F 86     cd op32  | Jump near if below or equal (CF=1 or ZF=1). */
int jbe (code_ptr p,
         rel32_t imm);
/*  72     cb   | Jump short if carry (CF=1). */
 /*  72     cb   | Jump short if carry (CF=1). */
int jc (code_ptr p,rel8_t imm);
/*  0F 82     cw op16  | Jump near if carry (CF=1). Not supported in 64-bit mode. */
 /*  0F 82     cw op16  | Jump near if carry (CF=1). Not supported in 64-bit mode. */
int jc (code_ptr p,rel16_t imm);
/*  0F 82     cd op32  | Jump near if carry (CF=1). */
 /*  0F 82     cd op32  | Jump near if carry (CF=1). */
int jc (code_ptr p,rel32_t imm);
/*  E3     cb  addr16 | Jump short if CX register is 0. */
 /*  E3     cb  addr16 | Jump short if CX register is 0. */
int jcxz (code_ptr p,
          rel8_t imm);
/*  74     cb   | Jump short if equal (ZF=1). */
 /*  74     cb   | Jump short if equal (ZF=1). */
int je (code_ptr p,rel8_t imm);
/*  0F 84     cw op16  | Jump near if equal (ZF=1). Not supported in 64-bit mode. */
 /*  0F 84     cw op16  | Jump near if equal (ZF=1). Not supported in 64-bit mode. */
int je (code_ptr p,rel16_t imm);
/*  0F 84     cd op32  | Jump near if equal (ZF=1). */
 /*  0F 84     cd op32  | Jump near if equal (ZF=1). */
int je (code_ptr p,rel32_t imm);
/*  E3     cb  addr32 | Jump short if ECX register is 0. */
 /*  E3     cb  addr32 | Jump short if ECX register is 0. */
int jecxz (code_ptr p,
           rel8_t imm);
/*  7F     cb   | Jump short if greater (ZF=0 and SF=OF). */
 /*  7F     cb   | Jump short if greater (ZF=0 and SF=OF). */
int jg (code_ptr p,rel8_t imm);
/*  0F 8F     cw op16  | Jump near if greater (ZF=0 and SF=OF). Not supported in 64-bit mode. */
 /*  0F 8F     cw op16  | Jump near if greater (ZF=0 and SF=OF). Not supported in 64-bit mode. */
int jg (code_ptr p,rel16_t imm);
/*  0F 8F     cd op32  | Jump near if greater (ZF=0 and SF=OF). */
 /*  0F 8F     cd op32  | Jump near if greater (ZF=0 and SF=OF). */
int jg (code_ptr p,rel32_t imm);
/*  7D     cb   | Jump short if greater or equal (SF=OF). */
 /*  7D     cb   | Jump short if greater or equal (SF=OF). */
int jge (code_ptr p,rel8_t imm);
/*  0F 8D     cw op16  | Jump near if greater or equal (SF=OF). Not supported in 64-bit mode. */
 /*  0F 8D     cw op16  | Jump near if greater or equal (SF=OF). Not supported in 64-bit mode. */
int jge (code_ptr p,
         rel16_t imm);
/*  0F 8D     cd op32  | Jump near if greater or equal (SF=OF). */
 /*  0F 8D     cd op32  | Jump near if greater or equal (SF=OF). */
int jge (code_ptr p,
         rel32_t imm);
/*  7C     cb   | Jump short if less (SF≠ OF). */
 /*  7C     cb   | Jump short if less (SF≠ OF). */
int jl (code_ptr p,rel8_t imm);
/*  0F 8C     cw op16  | Jump near if less (SF≠ OF). Not supported in 64-bit mode. */
 /*  0F 8C     cw op16  | Jump near if less (SF≠ OF). Not supported in 64-bit mode. */
int jl (code_ptr p,rel16_t imm);
/*  0F 8C     cd op32  | Jump near if less (SF≠ OF). */
 /*  0F 8C     cd op32  | Jump near if less (SF≠ OF). */
int jl (code_ptr p,rel32_t imm);
/*  7E     cb   | Jump short if less or equal (ZF=1 or SF≠ OF). */
 /*  7E     cb   | Jump short if less or equal (ZF=1 or SF≠ OF). */
int jle (code_ptr p,rel8_t imm);
/*  0F 8E     cw op16  | Jump near if less or equal (ZF=1 or SF≠ OF). Not supported in 64-bit mode. */
 /*  0F 8E     cw op16  | Jump near if less or equal (ZF=1 or SF≠ OF). Not supported in 64-bit mode. */
int jle (code_ptr p,
         rel16_t imm);
/*  0F 8E     cd op32  | Jump near if less or equal (ZF=1 or SF≠ OF). */
 /*  0F 8E     cd op32  | Jump near if less or equal (ZF=1 or SF≠ OF). */
int jle (code_ptr p,
         rel32_t imm);
/*  EB     cb   | Jump short, RIP = RIP + 8-bit displacement sign extended to 64-bits */
 /*  EB     cb   | Jump short, RIP = RIP + 8-bit displacement sign extended to 64-bits */
int jmp (code_ptr p,rel8_t imm);
/*  E9     cw op16  | Jump near, relative, displacement relative to next instruction. Not supported in 64-bit mode. */
 /*  E9     cw op16  | Jump near, relative, displacement relative to next instruction. Not supported in 64-bit mode. */
int jmp (code_ptr p,
         rel16_t imm);
/*  E9     cd op32  | Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits */
 /*  E9     cd op32  | Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits */
int jmp (code_ptr p,
         rel32_t imm);
/*  EA     cd op16  | Jump far, absolute, address given in operand */
 /*  EA     cd op16  | Jump far, absolute, address given in operand */
int jmp (code_ptr p,
         const FarPtr16 (&   imm));
/*  EA     cp op32  | Jump far, absolute, address given in operand */
 /*  EA     cp op32  | Jump far, absolute, address given in operand */
int jmp (code_ptr p,
         const FarPtr32 (&   imm));
/*  FF  /4 RMBoth   op16  | Jump near, absolute indirect, address = zero-extended r/m16. Not supported in 64- bit mode. */
 /*  FF  /4 RMBoth   op16  | Jump near, absolute indirect, address = zero-extended r/m16. Not supported in 64- bit mode. */
int jmp (code_ptr p,
         const WordReg (&   modrm_rm));
/*  FF  /4 RMBoth   op16  | Jump near, absolute indirect, address = zero-extended r/m16. Not supported in 64- bit mode. */
 /*  FF  /4 RMBoth   op16  | Jump near, absolute indirect, address = zero-extended r/m16. Not supported in 64- bit mode. */
int jmp (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  FF  /4 RMBoth   op32  | Jump near, absolute indirect, address given in r/m32. Not supported in 64-bit mode. */
 /*  FF  /4 RMBoth   op32  | Jump near, absolute indirect, address given in r/m32. Not supported in 64-bit mode. */
int jmp (code_ptr p,
         const DwordReg (&   modrm_rm));
/*  FF  /4 RMBoth   op32  | Jump near, absolute indirect, address given in r/m32. Not supported in 64-bit mode. */
 /*  FF  /4 RMBoth   op32  | Jump near, absolute indirect, address given in r/m32. Not supported in 64-bit mode. */
int jmp (code_ptr p,
         const DwordPtr (&   modrm_rm));
/*  FF  /5 RMMemOnly   op16  | Jump far, absolute indirect, address given in m16:16 */
 /*  FF  /5 RMMemOnly   op16  | Jump far, absolute indirect, address given in m16:16 */
int jmp (code_ptr p,
         const Far16Ptr (&   modrm_rm));
/*  FF  /5 RMMemOnly   op32  | Jump far, absolute indirect, address given in m16:32. */
 /*  FF  /5 RMMemOnly   op32  | Jump far, absolute indirect, address given in m16:32. */
int jmp (code_ptr p,
         const Far32Ptr (&   modrm_rm));
/*  76     cb   | Jump short if not above (CF=1 or ZF=1). */
 /*  76     cb   | Jump short if not above (CF=1 or ZF=1). */
int jna (code_ptr p,rel8_t imm);
/*  0F 86     cw op16  | Jump near if not above (CF=1 or ZF=1). Not supported in 64-bit mode. */
 /*  0F 86     cw op16  | Jump near if not above (CF=1 or ZF=1). Not supported in 64-bit mode. */
int jna (code_ptr p,
         rel16_t imm);
/*  0F 86     cd op32  | Jump near if not above (CF=1 or ZF=1). */
 /*  0F 86     cd op32  | Jump near if not above (CF=1 or ZF=1). */
int jna (code_ptr p,
         rel32_t imm);
/*  72     cb   | Jump short if not above or equal (CF=1). */
 /*  72     cb   | Jump short if not above or equal (CF=1). */
int jnae (code_ptr p,
          rel8_t imm);
/*  0F 82     cw op16  | Jump near if not above or equal (CF=1). Not supported in 64-bit mode. */
 /*  0F 82     cw op16  | Jump near if not above or equal (CF=1). Not supported in 64-bit mode. */
int jnae (code_ptr p,
          rel16_t imm);
/*  0F 82     cd op32  | Jump near if not above or equal (CF=1). */
 /*  0F 82     cd op32  | Jump near if not above or equal (CF=1). */
int jnae (code_ptr p,
          rel32_t imm);
/*  73     cb   | Jump short if not below (CF=0). */
 /*  73     cb   | Jump short if not below (CF=0). */
int jnb (code_ptr p,rel8_t imm);
/*  0F 83     cw op16  | Jump near if not below (CF=0). Not supported in 64- bit mode. */
 /*  0F 83     cw op16  | Jump near if not below (CF=0). Not supported in 64- bit mode. */
int jnb (code_ptr p,
         rel16_t imm);
/*  0F 83     cd op32  | Jump near if not below (CF=0). */
 /*  0F 83     cd op32  | Jump near if not below (CF=0). */
int jnb (code_ptr p,
         rel32_t imm);
/*  77     cb   | Jump short if not below or equal (CF=0 and ZF=0). */
 /*  77     cb   | Jump short if not below or equal (CF=0 and ZF=0). */
int jnbe (code_ptr p,
          rel8_t imm);
/*  0F 87     cw op16  | Jump near if not below or equal (CF=0 and ZF=0). Not supported in 64-bit mode. */
 /*  0F 87     cw op16  | Jump near if not below or equal (CF=0 and ZF=0). Not supported in 64-bit mode. */
int jnbe (code_ptr p,
          rel16_t imm);
/*  0F 87     cd op32  | Jump near if not below or equal (CF=0 and ZF=0). */
 /*  0F 87     cd op32  | Jump near if not below or equal (CF=0 and ZF=0). */
int jnbe (code_ptr p,
          rel32_t imm);
/*  73     cb   | Jump short if not carry (CF=0). */
 /*  73     cb   | Jump short if not carry (CF=0). */
int jnc (code_ptr p,rel8_t imm);
/*  0F 83     cw op16  | Jump near if not carry (CF=0). Not supported in 64- bit mode. */
 /*  0F 83     cw op16  | Jump near if not carry (CF=0). Not supported in 64- bit mode. */
int jnc (code_ptr p,
         rel16_t imm);
/*  0F 83     cd op32  | Jump near if not carry (CF=0). */
 /*  0F 83     cd op32  | Jump near if not carry (CF=0). */
int jnc (code_ptr p,
         rel32_t imm);
/*  75     cb   | Jump short if not equal (ZF=0). */
 /*  75     cb   | Jump short if not equal (ZF=0). */
int jne (code_ptr p,rel8_t imm);
/*  0F 85     cw op16  | Jump near if not equal (ZF=0). Not supported in 64-bit mode. */
 /*  0F 85     cw op16  | Jump near if not equal (ZF=0). Not supported in 64-bit mode. */
int jne (code_ptr p,
         rel16_t imm);
/*  0F 85     cd op32  | Jump near if not equal (ZF=0). */
 /*  0F 85     cd op32  | Jump near if not equal (ZF=0). */
int jne (code_ptr p,
         rel32_t imm);
/*  7E     cb   | Jump short if not greater (ZF=1 or SF≠ OF). */
 /*  7E     cb   | Jump short if not greater (ZF=1 or SF≠ OF). */
int jng (code_ptr p,rel8_t imm);
/*  0F 8E     cw op16  | Jump near if not greater (ZF=1 or SF≠ OF). Not supported in 64-bit mode. */
 /*  0F 8E     cw op16  | Jump near if not greater (ZF=1 or SF≠ OF). Not supported in 64-bit mode. */
int jng (code_ptr p,
         rel16_t imm);
/*  0F 8E     cd op32  | Jump near if not greater (ZF=1 or SF≠ OF). */
 /*  0F 8E     cd op32  | Jump near if not greater (ZF=1 or SF≠ OF). */
int jng (code_ptr p,
         rel32_t imm);
/*  7C     cb   | Jump short if not greater or equal (SF≠ OF). */
 /*  7C     cb   | Jump short if not greater or equal (SF≠ OF). */
int jnge (code_ptr p,
          rel8_t imm);
/*  0F 8C     cw op16  | Jump near if not greater or equal (SF≠ OF). Not supported in 64-bit mode. */
 /*  0F 8C     cw op16  | Jump near if not greater or equal (SF≠ OF). Not supported in 64-bit mode. */
int jnge (code_ptr p,
          rel16_t imm);
/*  0F 8C     cd op32  | Jump near if not greater or equal (SF≠ OF). */
 /*  0F 8C     cd op32  | Jump near if not greater or equal (SF≠ OF). */
int jnge (code_ptr p,
          rel32_t imm);
/*  7D     cb   | Jump short if not less (SF=OF). */
 /*  7D     cb   | Jump short if not less (SF=OF). */
int jnl (code_ptr p,rel8_t imm);
/*  0F 8D     cw op16  | Jump near if not less (SF=OF). Not supported in 64-bit mode. */
 /*  0F 8D     cw op16  | Jump near if not less (SF=OF). Not supported in 64-bit mode. */
int jnl (code_ptr p,
         rel16_t imm);
/*  0F 8D     cd op32  | Jump near if not less (SF=OF). */
 /*  0F 8D     cd op32  | Jump near if not less (SF=OF). */
int jnl (code_ptr p,
         rel32_t imm);
/*  7F     cb   | Jump short if not less or equal (ZF=0 and SF=OF). */
 /*  7F     cb   | Jump short if not less or equal (ZF=0 and SF=OF). */
int jnle (code_ptr p,
          rel8_t imm);
/*  0F 8F     cw op16  | Jump near if not less or equal (ZF=0 and SF=OF). Not supported in 64-bit mode. */
 /*  0F 8F     cw op16  | Jump near if not less or equal (ZF=0 and SF=OF). Not supported in 64-bit mode. */
int jnle (code_ptr p,
          rel16_t imm);
/*  0F 8F     cd op32  | Jump near if not less or equal (ZF=0 and SF=OF). */
 /*  0F 8F     cd op32  | Jump near if not less or equal (ZF=0 and SF=OF). */
int jnle (code_ptr p,
          rel32_t imm);
/*  71     cb   | Jump short if not overflow (OF=0). */
 /*  71     cb   | Jump short if not overflow (OF=0). */
int jno (code_ptr p,rel8_t imm);
/*  0F 81     cw op16  | Jump near if not overflow (OF=0). Not supported in 64-bit mode. */
 /*  0F 81     cw op16  | Jump near if not overflow (OF=0). Not supported in 64-bit mode. */
int jno (code_ptr p,
         rel16_t imm);
/*  0F 81     cd op32  | Jump near if not overflow (OF=0). */
 /*  0F 81     cd op32  | Jump near if not overflow (OF=0). */
int jno (code_ptr p,
         rel32_t imm);
/*  7B     cb   | Jump short if not parity (PF=0). */
 /*  7B     cb   | Jump short if not parity (PF=0). */
int jnp (code_ptr p,rel8_t imm);
/*  0F 8B     cw op16  | Jump near if not parity (PF=0). Not supported in 64- bit mode. */
 /*  0F 8B     cw op16  | Jump near if not parity (PF=0). Not supported in 64- bit mode. */
int jnp (code_ptr p,
         rel16_t imm);
/*  0F 8B     cd op32  | Jump near if not parity (PF=0). */
 /*  0F 8B     cd op32  | Jump near if not parity (PF=0). */
int jnp (code_ptr p,
         rel32_t imm);
/*  79     cb   | Jump short if not sign (SF=0). */
 /*  79     cb   | Jump short if not sign (SF=0). */
int jns (code_ptr p,rel8_t imm);
/*  0F 89     cw op16  | Jump near if not sign (SF=0). Not supported in 64-bit mode. */
 /*  0F 89     cw op16  | Jump near if not sign (SF=0). Not supported in 64-bit mode. */
int jns (code_ptr p,
         rel16_t imm);
/*  0F 89     cd op32  | Jump near if not sign (SF=0). */
 /*  0F 89     cd op32  | Jump near if not sign (SF=0). */
int jns (code_ptr p,
         rel32_t imm);
/*  75     cb   | Jump short if not zero (ZF=0). */
 /*  75     cb   | Jump short if not zero (ZF=0). */
int jnz (code_ptr p,rel8_t imm);
/*  0F 85     cw op16  | Jump near if not zero (ZF=0). Not supported in 64-bit mode. */
 /*  0F 85     cw op16  | Jump near if not zero (ZF=0). Not supported in 64-bit mode. */
int jnz (code_ptr p,
         rel16_t imm);
/*  0F 85     cd op32  | Jump near if not zero (ZF=0). */
 /*  0F 85     cd op32  | Jump near if not zero (ZF=0). */
int jnz (code_ptr p,
         rel32_t imm);
/*  70     cb   | Jump short if overflow (OF=1). */
 /*  70     cb   | Jump short if overflow (OF=1). */
int jo (code_ptr p,rel8_t imm);
/*  0F 80     cw op16  | Jump near if overflow (OF=1). Not supported in 64-bit mode. */
 /*  0F 80     cw op16  | Jump near if overflow (OF=1). Not supported in 64-bit mode. */
int jo (code_ptr p,rel16_t imm);
/*  0F 80     cd op32  | Jump near if overflow (OF=1). */
 /*  0F 80     cd op32  | Jump near if overflow (OF=1). */
int jo (code_ptr p,rel32_t imm);
/*  7A     cb   | Jump short if parity (PF=1). */
 /*  7A     cb   | Jump short if parity (PF=1). */
int jp (code_ptr p,rel8_t imm);
/*  0F 8A     cw op16  | Jump near if parity (PF=1). Not supported in 64-bit mode. */
 /*  0F 8A     cw op16  | Jump near if parity (PF=1). Not supported in 64-bit mode. */
int jp (code_ptr p,rel16_t imm);
/*  0F 8A     cd op32  | Jump near if parity (PF=1). */
 /*  0F 8A     cd op32  | Jump near if parity (PF=1). */
int jp (code_ptr p,rel32_t imm);
/*  7A     cb   | Jump short if parity even (PF=1). */
 /*  7A     cb   | Jump short if parity even (PF=1). */
int jpe (code_ptr p,rel8_t imm);
/*  0F 8A     cw op16  | Jump near if parity even (PF=1). Not supported in 64- bit mode. */
 /*  0F 8A     cw op16  | Jump near if parity even (PF=1). Not supported in 64- bit mode. */
int jpe (code_ptr p,
         rel16_t imm);
/*  0F 8A     cd op32  | Jump near if parity even (PF=1). */
 /*  0F 8A     cd op32  | Jump near if parity even (PF=1). */
int jpe (code_ptr p,
         rel32_t imm);
/*  7B     cb   | Jump short if parity odd (PF=0). */
 /*  7B     cb   | Jump short if parity odd (PF=0). */
int jpo (code_ptr p,rel8_t imm);
/*  0F 8B     cw op16  | Jump near if parity odd (PF=0). Not supported in 64- bit mode. */
 /*  0F 8B     cw op16  | Jump near if parity odd (PF=0). Not supported in 64- bit mode. */
int jpo (code_ptr p,
         rel16_t imm);
/*  0F 8B     cd op32  | Jump near if parity odd (PF=0). */
 /*  0F 8B     cd op32  | Jump near if parity odd (PF=0). */
int jpo (code_ptr p,
         rel32_t imm);
/*  78     cb   | Jump short if sign (SF=1). */
 /*  78     cb   | Jump short if sign (SF=1). */
int js (code_ptr p,rel8_t imm);
/*  0F 88     cw op16  | Jump near if sign (SF=1). Not supported in 64-bit mode. */
 /*  0F 88     cw op16  | Jump near if sign (SF=1). Not supported in 64-bit mode. */
int js (code_ptr p,rel16_t imm);
/*  0F 88     cd op32  | Jump near if sign (SF=1). */
 /*  0F 88     cd op32  | Jump near if sign (SF=1). */
int js (code_ptr p,rel32_t imm);
/*  74     cb   | Jump short if zero (ZF ← 1). */
 /*  74     cb   | Jump short if zero (ZF ← 1). */
int jz (code_ptr p,rel8_t imm);
/*  0F 84     cw op16  | Jump near if 0 (ZF=1). Not supported in 64-bit mode. */
 /*  0F 84     cw op16  | Jump near if 0 (ZF=1). Not supported in 64-bit mode. */
int jz (code_ptr p,rel16_t imm);
/*  0F 84     cd op32  | Jump near if 0 (ZF=1). */
 /*  0F 84     cd op32  | Jump near if 0 (ZF=1). */
int jz (code_ptr p,rel32_t imm);
/*  9F        | Load: AH ← EFLAGS(SF:ZF:0:AF:0:PF:1:CF). */
 /*  9F        | Load: AH ← EFLAGS(SF:ZF:0:AF:0:PF:1:CF). */
int lahf (code_ptr p);
/*  0F 02  /r RMBoth   op16  | r16 ← r16/m16 masked by FF00H. */
 /*  0F 02  /r RMBoth   op16  | r16 ← r16/m16 masked by FF00H. */
int lar (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  0F 02  /r RMBoth   op16  | r16 ← r16/m16 masked by FF00H. */
 /*  0F 02  /r RMBoth   op16  | r16 ← r16/m16 masked by FF00H. */
int lar (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  0F 02  /r RMBoth   op32  | r32 ← r32/m16 masked by 00FxFF00H */
 /*  0F 02  /r RMBoth   op32  | r32 ← r32/m16 masked by 00FxFF00H */
int lar (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  0F 02  /r RMBoth   op32  | r32 ← r32/m16 masked by 00FxFF00H */
 /*  0F 02  /r RMBoth   op32  | r32 ← r32/m16 masked by 00FxFF00H */
int lar (code_ptr p,
         const DwordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/* F2 0F F0  /r RMMemOnly     | Load unaligned data from mem and return double quadword in xmm1. */
 /* F2 0F F0  /r RMMemOnly     | Load unaligned data from mem and return double quadword in xmm1. */
int lddqu (code_ptr p,
           const XmmReg (&   modrm_reg),
           const VoidPtr (&   modrm_rm));
/*  0F AE  /2 RMMemOnly     | Load MXCSR register from m32. */
 /*  0F AE  /2 RMMemOnly     | Load MXCSR register from m32. */
int ldmxcsr (code_ptr p,
             const DwordPtr (&   modrm_rm));
/*  C5  /r RMMemOnly   op16  | Load DS:r16 with far pointer from memory. */
 /*  C5  /r RMMemOnly   op16  | Load DS:r16 with far pointer from memory. */
int lds (code_ptr p,
         const WordReg (&   modrm_reg),
         const Far16Ptr (&   modrm_rm));
/*  C5  /r RMMemOnly   op32  | Load DS:r32 with far pointer from memory. */
 /*  C5  /r RMMemOnly   op32  | Load DS:r32 with far pointer from memory. */
int lds (code_ptr p,
         const DwordReg (&   modrm_reg),
         const Far32Ptr (&   modrm_rm));
/*  8D  /r RMMemOnly   op16  | Store effective address for m in register r16. */
 /*  8D  /r RMMemOnly   op16  | Store effective address for m in register r16. */
int lea (code_ptr p,
         const WordReg (&   modrm_reg),
         const VoidPtr (&   modrm_rm));
/*  8D  /r RMMemOnly   op32  | Store effective address for m in register r32. */
 /*  8D  /r RMMemOnly   op32  | Store effective address for m in register r32. */
int lea (code_ptr p,
         const DwordReg (&   modrm_reg),
         const VoidPtr (&   modrm_rm));
/*  C9      op32  | Set ESP to EBP, then pop EBP. */
 /*  C9      op32  | Set ESP to EBP, then pop EBP. */
int leave (code_ptr p);
/*  C9      op16  | Set SP to BP, then pop BP. */
 /*  C9      op16  | Set SP to BP, then pop BP. */
int leavew (code_ptr p);
/*  C4  /r RMMemOnly   op16  | Load ES:r16 with far pointer from memory. */
 /*  C4  /r RMMemOnly   op16  | Load ES:r16 with far pointer from memory. */
int les (code_ptr p,
         const WordReg (&   modrm_reg),
         const Far16Ptr (&   modrm_rm));
/*  C4  /r RMMemOnly   op32  | Load ES:r32 with far pointer from memory. */
 /*  C4  /r RMMemOnly   op32  | Load ES:r32 with far pointer from memory. */
int les (code_ptr p,
         const DwordReg (&   modrm_reg),
         const Far32Ptr (&   modrm_rm));
/*  0F AE  /5 RMRegOnly     | Serializes load operations. */
 /*  0F AE  /5 RMRegOnly     | Serializes load operations. */
int lfence (code_ptr p);
/*  0F B4  /r RMMemOnly   op16  | Load FS:r16 with far pointer from memory. */
 /*  0F B4  /r RMMemOnly   op16  | Load FS:r16 with far pointer from memory. */
int lfs (code_ptr p,
         const WordReg (&   modrm_reg),
         const Far16Ptr (&   modrm_rm));
/*  0F B4  /r RMMemOnly   op32  | Load FS:r32 with far pointer from memory. */
 /*  0F B4  /r RMMemOnly   op32  | Load FS:r32 with far pointer from memory. */
int lfs (code_ptr p,
         const DwordReg (&   modrm_reg),
         const Far32Ptr (&   modrm_rm));
/*  0F 01  /2 RMMemOnly     | Load m into GDTR. */
 /*  0F 01  /2 RMMemOnly     | Load m into GDTR. */
int lgdt (code_ptr p,
          const FwordPtr (&   modrm_rm));
/*  0F B5  /r RMMemOnly   op16  | Load GS:r16 with far pointer from memory. */
 /*  0F B5  /r RMMemOnly   op16  | Load GS:r16 with far pointer from memory. */
int lgs (code_ptr p,
         const WordReg (&   modrm_reg),
         const Far16Ptr (&   modrm_rm));
/*  0F B5  /r RMMemOnly   op32  | Load GS:r32 with far pointer from memory. */
 /*  0F B5  /r RMMemOnly   op32  | Load GS:r32 with far pointer from memory. */
int lgs (code_ptr p,
         const DwordReg (&   modrm_reg),
         const Far32Ptr (&   modrm_rm));
/*  0F 01  /3 RMMemOnly     | Load m into IDTR. */
 /*  0F 01  /3 RMMemOnly     | Load m into IDTR. */
int lidt (code_ptr p,
          const FwordPtr (&   modrm_rm));
/*  0F 00  /2 RMBoth     | Load segment selector r/m16 into LDTR. */
 /*  0F 00  /2 RMBoth     | Load segment selector r/m16 into LDTR. */
int lldt (code_ptr p,
          const WordReg (&   modrm_rm));
/*  0F 00  /2 RMBoth     | Load segment selector r/m16 into LDTR. */
 /*  0F 00  /2 RMBoth     | Load segment selector r/m16 into LDTR. */
int lldt (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  0F 01  /6 RMBoth     | Loads r/m16 in machine status word of CR0. */
 /*  0F 01  /6 RMBoth     | Loads r/m16 in machine status word of CR0. */
int lmsw (code_ptr p,
          const WordReg (&   modrm_rm));
/*  0F 01  /6 RMBoth     | Loads r/m16 in machine status word of CR0. */
 /*  0F 01  /6 RMBoth     | Loads r/m16 in machine status word of CR0. */
int lmsw (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
 /*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
int lods (code_ptr p,
          const BytePtr_ESI (&   ptr));
/*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
 /*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
int lods (code_ptr p,
          const RegAL (&   unused),
          const BytePtr_ESI (&   ptr));
/*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
 /*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
int lods (code_ptr p,
          const BytePtr_SI (&   ptr));
/*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
 /*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
int lods (code_ptr p,
          const RegAL (&   unused),
          const BytePtr_SI (&   ptr));
/*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
 /*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
int lods (code_ptr p,
          const WordPtr_ESI (&   ptr));
/*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
 /*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
int lods (code_ptr p,
          const RegAX (&   unused),
          const WordPtr_ESI (&   ptr));
/*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
 /*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
int lods (code_ptr p,
          const WordPtr_SI (&   ptr));
/*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
 /*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
int lods (code_ptr p,
          const RegAX (&   unused),
          const WordPtr_SI (&   ptr));
/*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
 /*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
int lods (code_ptr p,
          const DwordPtr_ESI (&   ptr));
/*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
 /*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
int lods (code_ptr p,
          const RegEAX (&   unused),
          const DwordPtr_ESI (&   ptr));
/*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
 /*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
int lods (code_ptr p,
          const DwordPtr_SI (&   ptr));
/*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
 /*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
int lods (code_ptr p,
          const RegEAX (&   unused),
          const DwordPtr_SI (&   ptr));
/*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
 /*  AC        | For legacy mode, Load byte at address DS:(E)SI into AL. For 64-bit mode load byte at address (R)SI into AL. */
int lodsb (code_ptr p);
/*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
 /*  AD      op32  | For legacy mode, Load dword at address DS:(E)SI into EAX. For 64-bit mode load dword at address (R)SI into EAX. */
int lodsd (code_ptr p);
/*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
 /*  AD      op16  | For legacy mode, Load word at address DS:(E)SI into AX. For 64-bit mode load word at address (R)SI into AX. */
int lodsw (code_ptr p);
/*  E2     cb   | Decrement count; jump short if count ≠ 0. */
 /*  E2     cb   | Decrement count; jump short if count ≠ 0. */
int loop (code_ptr p,
          rel8_t imm);
/*  E1     cb   | Decrement count; jump short if count ≠ 0 and ZF = 1. */
 /*  E1     cb   | Decrement count; jump short if count ≠ 0 and ZF = 1. */
int loope (code_ptr p,
           rel8_t imm);
/*  E0     cb   | Decrement count; jump short if count ≠ 0 and ZF = 0. */
 /*  E0     cb   | Decrement count; jump short if count ≠ 0 and ZF = 0. */
int loopne (code_ptr p,
            rel8_t imm);
/*  0F 03  /r RMBoth   op16  | Load: r16 ← segment limit, selector r16/m16. */
 /*  0F 03  /r RMBoth   op16  | Load: r16 ← segment limit, selector r16/m16. */
int lsl (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  0F 03  /r RMBoth   op16  | Load: r16 ← segment limit, selector r16/m16. */
 /*  0F 03  /r RMBoth   op16  | Load: r16 ← segment limit, selector r16/m16. */
int lsl (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  0F 03  /r RMBoth   op32  | Load: r32 ← segment limit, selector r32/m16. */
 /*  0F 03  /r RMBoth   op32  | Load: r32 ← segment limit, selector r32/m16. */
int lsl (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  0F 03  /r RMBoth   op32  | Load: r32 ← segment limit, selector r32/m16. */
 /*  0F 03  /r RMBoth   op32  | Load: r32 ← segment limit, selector r32/m16. */
int lsl (code_ptr p,
         const DwordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  0F B2  /r RMMemOnly   op16  | Load SS:r16 with far pointer from memory. */
 /*  0F B2  /r RMMemOnly   op16  | Load SS:r16 with far pointer from memory. */
int lss (code_ptr p,
         const WordReg (&   modrm_reg),
         const Far16Ptr (&   modrm_rm));
/*  0F B2  /r RMMemOnly   op32  | Load SS:r32 with far pointer from memory. */
 /*  0F B2  /r RMMemOnly   op32  | Load SS:r32 with far pointer from memory. */
int lss (code_ptr p,
         const DwordReg (&   modrm_reg),
         const Far32Ptr (&   modrm_rm));
/*  0F 00  /3 RMBoth     | Load r/m16 into task register. */
 /*  0F 00  /3 RMBoth     | Load r/m16 into task register. */
int ltr (code_ptr p,
         const WordReg (&   modrm_rm));
/*  0F 00  /3 RMBoth     | Load r/m16 into task register. */
 /*  0F 00  /3 RMBoth     | Load r/m16 into task register. */
int ltr (code_ptr p,
         const WordPtr (&   modrm_rm));
/* 66 0F F7  /r RMRegOnly     | Selectively write bytes from xmm1 to memory location using the byte mask in xmm2. The default memory location is specified by DS:EDI. */
 /* 66 0F F7  /r RMRegOnly     | Selectively write bytes from xmm1 to memory location using the byte mask in xmm2. The default memory location is specified by DS:EDI. */
int maskmovdqu (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/*  0F F7  /r RMRegOnly     | Selectively write bytes from mm1 to memory location using the byte mask in mm2. The default memory location is specified by DS:EDI. */
 /*  0F F7  /r RMRegOnly     | Selectively write bytes from mm1 to memory location using the byte mask in mm2. The default memory location is specified by DS:EDI. */
int maskmovq (code_ptr p,
              const MmReg (&   modrm_reg),
              const MmReg (&   modrm_rm));
/* 66 0F 5F  /r RMBoth     | Return the maximum double-precision floating- point values between xmm2/m128 and xmm1. */
 /* 66 0F 5F  /r RMBoth     | Return the maximum double-precision floating- point values between xmm2/m128 and xmm1. */
int maxpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 5F  /r RMBoth     | Return the maximum double-precision floating- point values between xmm2/m128 and xmm1. */
 /* 66 0F 5F  /r RMBoth     | Return the maximum double-precision floating- point values between xmm2/m128 and xmm1. */
int maxpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 5F  /r RMBoth     | Return the maximum single- precision floating-point values between xmm2/m128 and xmm1. */
 /*  0F 5F  /r RMBoth     | Return the maximum single- precision floating-point values between xmm2/m128 and xmm1. */
int maxps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 5F  /r RMBoth     | Return the maximum single- precision floating-point values between xmm2/m128 and xmm1. */
 /*  0F 5F  /r RMBoth     | Return the maximum single- precision floating-point values between xmm2/m128 and xmm1. */
int maxps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* F2 0F 5F  /r RMBoth     | Return the maximum scalar double-precision floating- point value between xmm2/mem64 and xmm1. */
 /* F2 0F 5F  /r RMBoth     | Return the maximum scalar double-precision floating- point value between xmm2/mem64 and xmm1. */
int maxsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F2 0F 5F  /r RMBoth     | Return the maximum scalar double-precision floating- point value between xmm2/mem64 and xmm1. */
 /* F2 0F 5F  /r RMBoth     | Return the maximum scalar double-precision floating- point value between xmm2/mem64 and xmm1. */
int maxsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* F3 0F 5F  /r RMBoth     | Return the maximum scalar single-precision floating- point value between xmm2/mem32 and xmm1. */
 /* F3 0F 5F  /r RMBoth     | Return the maximum scalar single-precision floating- point value between xmm2/mem32 and xmm1. */
int maxss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F3 0F 5F  /r RMBoth     | Return the maximum scalar single-precision floating- point value between xmm2/mem32 and xmm1. */
 /* F3 0F 5F  /r RMBoth     | Return the maximum scalar single-precision floating- point value between xmm2/mem32 and xmm1. */
int maxss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F AE  /6 RMRegOnly     | Serializes load and store operations. */
 /*  0F AE  /6 RMRegOnly     | Serializes load and store operations. */
int mfence (code_ptr p);
/* 66 0F 5D  /r RMBoth     | Return the minimum double- precision floating-point values between xmm2/m128 and xmm1. */
 /* 66 0F 5D  /r RMBoth     | Return the minimum double- precision floating-point values between xmm2/m128 and xmm1. */
int minpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 5D  /r RMBoth     | Return the minimum double- precision floating-point values between xmm2/m128 and xmm1. */
 /* 66 0F 5D  /r RMBoth     | Return the minimum double- precision floating-point values between xmm2/m128 and xmm1. */
int minpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 5D  /r RMBoth     | Return the minimum single- precision floating-point values between xmm2/m128 and xmm1. */
 /*  0F 5D  /r RMBoth     | Return the minimum single- precision floating-point values between xmm2/m128 and xmm1. */
int minps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 5D  /r RMBoth     | Return the minimum single- precision floating-point values between xmm2/m128 and xmm1. */
 /*  0F 5D  /r RMBoth     | Return the minimum single- precision floating-point values between xmm2/m128 and xmm1. */
int minps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* F2 0F 5D  /r RMBoth     | Return the minimum scalar double-precision floating- point value between xmm2/mem64 and xmm1. */
 /* F2 0F 5D  /r RMBoth     | Return the minimum scalar double-precision floating- point value between xmm2/mem64 and xmm1. */
int minsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F2 0F 5D  /r RMBoth     | Return the minimum scalar double-precision floating- point value between xmm2/mem64 and xmm1. */
 /* F2 0F 5D  /r RMBoth     | Return the minimum scalar double-precision floating- point value between xmm2/mem64 and xmm1. */
int minsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* F3 0F 5D  /r RMBoth     | Return the minimum scalar single-precision floating- point value between xmm2/mem32 and xmm1. */
 /* F3 0F 5D  /r RMBoth     | Return the minimum scalar single-precision floating- point value between xmm2/mem32 and xmm1. */
int minss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F3 0F 5D  /r RMBoth     | Return the minimum scalar single-precision floating- point value between xmm2/mem32 and xmm1. */
 /* F3 0F 5D  /r RMBoth     | Return the minimum scalar single-precision floating- point value between xmm2/mem32 and xmm1. */
int minss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 01 C8        | Sets up a linear address range to be monitored by hardware and activates the monitor. The address range should be a write-back memory caching type. The address is DS:EAX (DS:RAX in 64-bit mode). */
 /*  0F 01 C8        | Sets up a linear address range to be monitored by hardware and activates the monitor. The address range should be a write-back memory caching type. The address is DS:EAX (DS:RAX in 64-bit mode). */
int monitor (code_ptr p);
/*  8B  /r RMBoth   op32  | Move r/m32 to r32. */
 /*  8B  /r RMBoth   op32  | Move r/m32 to r32. */
int mov (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  8B  /r RMBoth   op32  | Move r/m32 to r32. */
 /*  8B  /r RMBoth   op32  | Move r/m32 to r32. */
int mov (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  89  /r RMBoth   op32  | Move r32 to r/m32. */
 /*  89  /r RMBoth   op32  | Move r32 to r/m32. */
int mov (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  B8    +rd id op32  | Move imm32 to r32. */
 /*  B8    +rd id op32  | Move imm32 to r32. */
int mov (code_ptr p,
         const DwordReg (&   radd),
         imm32_t imm);
/*  C7  /0 RMBoth  id op32  | Move imm32 to r/m32. */
 /*  C7  /0 RMBoth  id op32  | Move imm32 to r/m32. */
int mov (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm32_t imm);
/*  8B  /r RMBoth   op16  | Move r/m16 to r16. */
 /*  8B  /r RMBoth   op16  | Move r/m16 to r16. */
int mov (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  8B  /r RMBoth   op16  | Move r/m16 to r16. */
 /*  8B  /r RMBoth   op16  | Move r/m16 to r16. */
int mov (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  89  /r RMBoth   op16  | Move r16 to r/m16. */
 /*  89  /r RMBoth   op16  | Move r16 to r/m16. */
int mov (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  B8    +rw iw op16  | Move imm16 to r16. */
 /*  B8    +rw iw op16  | Move imm16 to r16. */
int mov (code_ptr p,
         const WordReg (&   radd),
         imm16_t imm);
/*  C7  /0 RMBoth  iw op16  | Move imm16 to r/m16. */
 /*  C7  /0 RMBoth  iw op16  | Move imm16 to r/m16. */
int mov (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm16_t imm);
/*  8A  /r RMBoth     | Move r/m8 to r8. */
 /*  8A  /r RMBoth     | Move r/m8 to r8. */
int mov (code_ptr p,
         const ByteReg (&   modrm_reg),
         const ByteReg (&   modrm_rm));
/*  8A  /r RMBoth     | Move r/m8 to r8. */
 /*  8A  /r RMBoth     | Move r/m8 to r8. */
int mov (code_ptr p,
         const ByteReg (&   modrm_reg),
         const BytePtr (&   modrm_rm));
/*  88  /r RMBoth     | Move r8 to r/m8. */
 /*  88  /r RMBoth     | Move r8 to r/m8. */
int mov (code_ptr p,
         const BytePtr (&   modrm_rm),
         const ByteReg (&   modrm_reg));
/*  B0    +rb ib   | Move imm8 to r8. */
 /*  B0    +rb ib   | Move imm8 to r8. */
int mov (code_ptr p,
         const ByteReg (&   radd),
         imm8_t imm);
/*  C6  /0 RMBoth  ib   | Move imm8 to r/m8. */
 /*  C6  /0 RMBoth  ib   | Move imm8 to r/m8. */
int mov (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  0F 20  /r RMRegOnly     | Move control register to r32 */
 /*  0F 20  /r RMRegOnly     | Move control register to r32 */
int mov (code_ptr p,
         const DwordReg (&   modrm_rm),
         const CrReg (&   modrm_reg));
/*  0F 21  /r RMRegOnly     | Move debug register to r32 */
 /*  0F 21  /r RMRegOnly     | Move debug register to r32 */
int mov (code_ptr p,
         const DwordReg (&   modrm_rm),
         const DrReg (&   modrm_reg));
/*  0F 22  /r RMRegOnly     | Move r32 to control register */
 /*  0F 22  /r RMRegOnly     | Move r32 to control register */
int mov (code_ptr p,
         const CrReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  0F 23  /r RMRegOnly     | Move r32 to debug register */
 /*  0F 23  /r RMRegOnly     | Move r32 to debug register */
int mov (code_ptr p,
         const DrReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  8E  /r RMBoth     | Move r/m16 to segment register. */
 /*  8E  /r RMBoth     | Move r/m16 to segment register. */
int mov (code_ptr p,
         const SegReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  8E  /r RMBoth     | Move r/m16 to segment register. */
 /*  8E  /r RMBoth     | Move r/m16 to segment register. */
int mov (code_ptr p,
         const SegReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  A0        | Move byte at (seg:offset) to AL. */
 /*  A0        | Move byte at (seg:offset) to AL. */
int mov (code_ptr p,
         const RegAL (&   unused),
         const ByteOffset16 (&   offset));
/*  A0        | Move byte at (seg:offset) to AL. */
 /*  A0        | Move byte at (seg:offset) to AL. */
int mov (code_ptr p,
         const RegAL (&   unused),
         const ByteOffset32 (&   offset));
/*  A1      op16  | Move word at (seg:offset) to AX. */
 /*  A1      op16  | Move word at (seg:offset) to AX. */
int mov (code_ptr p,
         const RegAX (&   unused),
         const WordOffset16 (&   offset));
/*  A1      op16  | Move word at (seg:offset) to AX. */
 /*  A1      op16  | Move word at (seg:offset) to AX. */
int mov (code_ptr p,
         const RegAX (&   unused),
         const WordOffset32 (&   offset));
/*  A1      op32  | Move doubleword at (seg:offset) to EAX. */
 /*  A1      op32  | Move doubleword at (seg:offset) to EAX. */
int mov (code_ptr p,
         const RegEAX (&   unused),
         const DwordOffset16 (&   offset));
/*  A1      op32  | Move doubleword at (seg:offset) to EAX. */
 /*  A1      op32  | Move doubleword at (seg:offset) to EAX. */
int mov (code_ptr p,
         const RegEAX (&   unused),
         const DwordOffset32 (&   offset));
/*  8C  /r RMBoth     | Move segment register to r/m16. */
 /*  8C  /r RMBoth     | Move segment register to r/m16. */
int mov (code_ptr p,
         const WordReg (&   modrm_rm),
         const SegReg (&   modrm_reg));
/*  8C  /r RMBoth     | Move segment register to r/m16. */
 /*  8C  /r RMBoth     | Move segment register to r/m16. */
int mov (code_ptr p,
         const WordPtr (&   modrm_rm),
         const SegReg (&   modrm_reg));
/*  A2        | Move AL to (seg:offset). */
 /*  A2        | Move AL to (seg:offset). */
int mov (code_ptr p,
         const ByteOffset16 (&   offset),
         const RegAL (&   unused));
/*  A2        | Move AL to (seg:offset). */
 /*  A2        | Move AL to (seg:offset). */
int mov (code_ptr p,
         const ByteOffset32 (&   offset),
         const RegAL (&   unused));
/*  A3      op16  | Move AX to (seg:offset). */
 /*  A3      op16  | Move AX to (seg:offset). */
int mov (code_ptr p,
         const WordOffset16 (&   offset),
         const RegAX (&   unused));
/*  A3      op16  | Move AX to (seg:offset). */
 /*  A3      op16  | Move AX to (seg:offset). */
int mov (code_ptr p,
         const WordOffset32 (&   offset),
         const RegAX (&   unused));
/*  A3      op32  | Move EAX to (seg:offset). */
 /*  A3      op32  | Move EAX to (seg:offset). */
int mov (code_ptr p,
         const DwordOffset16 (&   offset),
         const RegEAX (&   unused));
/*  A3      op32  | Move EAX to (seg:offset). */
 /*  A3      op32  | Move EAX to (seg:offset). */
int mov (code_ptr p,
         const DwordOffset32 (&   offset),
         const RegEAX (&   unused));
/* 66 0F 28  /r RMBoth     | Move packed double- precision floating-point values from xmm2/m128 to xmm1. */
 /* 66 0F 28  /r RMBoth     | Move packed double- precision floating-point values from xmm2/m128 to xmm1. */
int movapd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 28  /r RMBoth     | Move packed double- precision floating-point values from xmm2/m128 to xmm1. */
 /* 66 0F 28  /r RMBoth     | Move packed double- precision floating-point values from xmm2/m128 to xmm1. */
int movapd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 29  /r RMBoth     | Move packed double- precision floating-point values from xmm1 to xmm2/m128. */
 /* 66 0F 29  /r RMBoth     | Move packed double- precision floating-point values from xmm1 to xmm2/m128. */
int movapd (code_ptr p,
            const XmmWordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/*  0F 28  /r RMBoth     | Move packed single- precision floating-point values from xmm2/m128 to xmm1. */
 /*  0F 28  /r RMBoth     | Move packed single- precision floating-point values from xmm2/m128 to xmm1. */
int movaps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/*  0F 28  /r RMBoth     | Move packed single- precision floating-point values from xmm2/m128 to xmm1. */
 /*  0F 28  /r RMBoth     | Move packed single- precision floating-point values from xmm2/m128 to xmm1. */
int movaps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 29  /r RMBoth     | Move packed single- precision floating-point values from xmm1 to xmm2/m128. */
 /*  0F 29  /r RMBoth     | Move packed single- precision floating-point values from xmm1 to xmm2/m128. */
int movaps (code_ptr p,
            const XmmWordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/*  0F 38 F0  /r RMMemOnly   op16  | Reverse byte order in m16 and move to r16 */
 /*  0F 38 F0  /r RMMemOnly   op16  | Reverse byte order in m16 and move to r16 */
int movbe (code_ptr p,
           const WordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/*  0F 38 F0  /r RMMemOnly   op32  | Reverse byte order in m32 and move to r32 */
 /*  0F 38 F0  /r RMMemOnly   op32  | Reverse byte order in m32 and move to r32 */
int movbe (code_ptr p,
           const DwordReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 38 F1  /r RMMemOnly   op16  | Reverse byte order in r16 and move to m16 */
 /*  0F 38 F1  /r RMMemOnly   op16  | Reverse byte order in r16 and move to m16 */
int movbe (code_ptr p,
           const WordPtr (&   modrm_rm),
           const WordReg (&   modrm_reg));
/*  0F 38 F1  /r RMMemOnly   op32  | Reverse byte order in r32 and move to m32 */
 /*  0F 38 F1  /r RMMemOnly   op32  | Reverse byte order in r32 and move to m32 */
int movbe (code_ptr p,
           const DwordPtr (&   modrm_rm),
           const DwordReg (&   modrm_reg));
/*  0F 6E  /r RMBoth     | Move doubleword from r/m32 to mm. */
 /*  0F 6E  /r RMBoth     | Move doubleword from r/m32 to mm. */
int movd (code_ptr p,
          const MmReg (&   modrm_reg),
          const DwordReg (&   modrm_rm));
/*  0F 6E  /r RMBoth     | Move doubleword from r/m32 to mm. */
 /*  0F 6E  /r RMBoth     | Move doubleword from r/m32 to mm. */
int movd (code_ptr p,
          const MmReg (&   modrm_reg),
          const DwordPtr (&   modrm_rm));
/* 66 0F 6E  /r RMBoth     | Move doubleword from r/m32 to xmm. */
 /* 66 0F 6E  /r RMBoth     | Move doubleword from r/m32 to xmm. */
int movd (code_ptr p,
          const XmmReg (&   modrm_reg),
          const DwordReg (&   modrm_rm));
/* 66 0F 6E  /r RMBoth     | Move doubleword from r/m32 to xmm. */
 /* 66 0F 6E  /r RMBoth     | Move doubleword from r/m32 to xmm. */
int movd (code_ptr p,
          const XmmReg (&   modrm_reg),
          const DwordPtr (&   modrm_rm));
/*  0F 7E  /r RMBoth     | Move doubleword from mm to r/m32. */
 /*  0F 7E  /r RMBoth     | Move doubleword from mm to r/m32. */
int movd (code_ptr p,
          const DwordReg (&   modrm_rm),
          const MmReg (&   modrm_reg));
/*  0F 7E  /r RMBoth     | Move doubleword from mm to r/m32. */
 /*  0F 7E  /r RMBoth     | Move doubleword from mm to r/m32. */
int movd (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const MmReg (&   modrm_reg));
/* 66 0F 7E  /r RMBoth     | Move doubleword from xmm register to r/m32. */
 /* 66 0F 7E  /r RMBoth     | Move doubleword from xmm register to r/m32. */
int movd (code_ptr p,
          const DwordReg (&   modrm_rm),
          const XmmReg (&   modrm_reg));
/* 66 0F 7E  /r RMBoth     | Move doubleword from xmm register to r/m32. */
 /* 66 0F 7E  /r RMBoth     | Move doubleword from xmm register to r/m32. */
int movd (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const XmmReg (&   modrm_reg));
/* F2 0F 12  /r RMBoth     | Move one double-precision floating-point value from the lower 64-bit operand in xmm2/m64 to xmm1 and duplicate. */
 /* F2 0F 12  /r RMBoth     | Move one double-precision floating-point value from the lower 64-bit operand in xmm2/m64 to xmm1 and duplicate. */
int movddup (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* F2 0F 12  /r RMBoth     | Move one double-precision floating-point value from the lower 64-bit operand in xmm2/m64 to xmm1 and duplicate. */
 /* F2 0F 12  /r RMBoth     | Move one double-precision floating-point value from the lower 64-bit operand in xmm2/m64 to xmm1 and duplicate. */
int movddup (code_ptr p,
             const XmmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* F2 0F D6  /r RMRegOnly     | Move low quadword from xmm to mmx register. */
 /* F2 0F D6  /r RMRegOnly     | Move low quadword from xmm to mmx register. */
int movdq2q (code_ptr p,
             const MmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 6F  /r RMBoth     | Move aligned double quadword from xmm2/m128 to xmm1. */
 /* 66 0F 6F  /r RMBoth     | Move aligned double quadword from xmm2/m128 to xmm1. */
int movdqa (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 6F  /r RMBoth     | Move aligned double quadword from xmm2/m128 to xmm1. */
 /* 66 0F 6F  /r RMBoth     | Move aligned double quadword from xmm2/m128 to xmm1. */
int movdqa (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 7F  /r RMBoth     | Move aligned double quadword from xmm1 to xmm2/m128. */
 /* 66 0F 7F  /r RMBoth     | Move aligned double quadword from xmm1 to xmm2/m128. */
int movdqa (code_ptr p,
            const XmmWordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/* F3 0F 6F  /r RMBoth     | Move unaligned double quadword from xmm2/m128 to xmm1. */
 /* F3 0F 6F  /r RMBoth     | Move unaligned double quadword from xmm2/m128 to xmm1. */
int movdqu (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* F3 0F 6F  /r RMBoth     | Move unaligned double quadword from xmm2/m128 to xmm1. */
 /* F3 0F 6F  /r RMBoth     | Move unaligned double quadword from xmm2/m128 to xmm1. */
int movdqu (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* F3 0F 7F  /r RMBoth     | Move unaligned double quadword from xmm1 to xmm2/m128. */
 /* F3 0F 7F  /r RMBoth     | Move unaligned double quadword from xmm1 to xmm2/m128. */
int movdqu (code_ptr p,
            const XmmWordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/*  0F 12  /r RMRegOnly     | Move two packed single- precision floating-point values from high quadword of xmm2 to low quadword of xmm1. */
 /*  0F 12  /r RMRegOnly     | Move two packed single- precision floating-point values from high quadword of xmm2 to low quadword of xmm1. */
int movhlps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 16  /r RMMemOnly     | Move double-precision floating-point value from m64 to high quadword of xmm. */
 /* 66 0F 16  /r RMMemOnly     | Move double-precision floating-point value from m64 to high quadword of xmm. */
int movhpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 17  /r RMMemOnly     | Move double-precision floating-point value from high quadword of xmm to m64. */
 /* 66 0F 17  /r RMMemOnly     | Move double-precision floating-point value from high quadword of xmm to m64. */
int movhpd (code_ptr p,
            const QwordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/*  0F 16  /r RMMemOnly     | Move two packed single- precision floating-point values from m64 to high quadword of xmm. */
 /*  0F 16  /r RMMemOnly     | Move two packed single- precision floating-point values from m64 to high quadword of xmm. */
int movhps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/*  0F 17  /r RMMemOnly     | Move two packed single- precision floating-point values from high quadword of xmm to m64. */
 /*  0F 17  /r RMMemOnly     | Move two packed single- precision floating-point values from high quadword of xmm to m64. */
int movhps (code_ptr p,
            const QwordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/*  0F 16  /r RMRegOnly     | Move two packed single- precision floating-point values from low quadword of xmm2 to high quadword of xmm1. */
 /*  0F 16  /r RMRegOnly     | Move two packed single- precision floating-point values from low quadword of xmm2 to high quadword of xmm1. */
int movlhps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 12  /r RMMemOnly     | Move double-precision floating-point value from m64 to low quadword of xmm register. */
 /* 66 0F 12  /r RMMemOnly     | Move double-precision floating-point value from m64 to low quadword of xmm register. */
int movlpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 13  /r RMMemOnly     | Move double-precision floating-point nvalue from low quadword of xmm register to m64. */
 /* 66 0F 13  /r RMMemOnly     | Move double-precision floating-point nvalue from low quadword of xmm register to m64. */
int movlpd (code_ptr p,
            const QwordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/*  0F 12  /r RMMemOnly     | Move two packed single- precision floating-point values from m64 to low quadword of xmm. */
 /*  0F 12  /r RMMemOnly     | Move two packed single- precision floating-point values from m64 to low quadword of xmm. */
int movlps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/*  0F 13  /r RMMemOnly     | Move two packed single- precision floating-point values from low quadword of xmm to m64. */
 /*  0F 13  /r RMMemOnly     | Move two packed single- precision floating-point values from low quadword of xmm to m64. */
int movlps (code_ptr p,
            const QwordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/* 66 0F 50  /r RMRegOnly     | Extract 2-bit sign mask from xmm and store in reg. The upper bits of r32 or r64 are filled with zeros. */
 /* 66 0F 50  /r RMRegOnly     | Extract 2-bit sign mask from xmm and store in reg. The upper bits of r32 or r64 are filled with zeros. */
int movmskpd (code_ptr p,
              const DwordReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F 50  /r RMRegOnly     | Extract 4-bit sign mask from xmm and store in reg. The upper bits of r32 or r64 are filled with zeros. */
 /*  0F 50  /r RMRegOnly     | Extract 4-bit sign mask from xmm and store in reg. The upper bits of r32 or r64 are filled with zeros. */
int movmskps (code_ptr p,
              const DwordReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F E7  /r RMMemOnly     | Move double quadword from xmm to m128 using non-temporal hint. */
 /* 66 0F E7  /r RMMemOnly     | Move double quadword from xmm to m128 using non-temporal hint. */
int movntdq (code_ptr p,
             const XmmWordPtr (&   modrm_rm),
             const XmmReg (&   modrm_reg));
/* 66 0F 38 2A  /r RMMemOnly     | Move double quadword from m128 to xmm using non-temporal hint if WC memory type. */
 /* 66 0F 38 2A  /r RMMemOnly     | Move double quadword from m128 to xmm using non-temporal hint if WC memory type. */
int movntdqa (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F C3  /r RMMemOnly     | Move doubleword from r32 to m32 using non-temporal hint. */
 /*  0F C3  /r RMMemOnly     | Move doubleword from r32 to m32 using non-temporal hint. */
int movnti (code_ptr p,
            const DwordPtr (&   modrm_rm),
            const DwordReg (&   modrm_reg));
/* 66 0F 2B  /r RMMemOnly     | Move packed double- precision floating-point values from xmm to m128 using non-temporal hint. */
 /* 66 0F 2B  /r RMMemOnly     | Move packed double- precision floating-point values from xmm to m128 using non-temporal hint. */
int movntpd (code_ptr p,
             const XmmWordPtr (&   modrm_rm),
             const XmmReg (&   modrm_reg));
/*  0F 2B  /r RMMemOnly     | Move packed single- precision floating-point values from xmm to m128 using non-temporal hint. */
 /*  0F 2B  /r RMMemOnly     | Move packed single- precision floating-point values from xmm to m128 using non-temporal hint. */
int movntps (code_ptr p,
             const XmmWordPtr (&   modrm_rm),
             const XmmReg (&   modrm_reg));
/*  0F E7  /r RMMemOnly     | Move quadword from mm to m64 using non-temporal hint. */
 /*  0F E7  /r RMMemOnly     | Move quadword from mm to m64 using non-temporal hint. */
int movntq (code_ptr p,
            const QwordPtr (&   modrm_rm),
            const MmReg (&   modrm_reg));
/* F3 0F 7E  /r RMBoth     | Move quadword from xmm2/mem64 to xmm1. */
 /* F3 0F 7E  /r RMBoth     | Move quadword from xmm2/mem64 to xmm1. */
int movq (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmReg (&   modrm_rm));
/* F3 0F 7E  /r RMBoth     | Move quadword from xmm2/mem64 to xmm1. */
 /* F3 0F 7E  /r RMBoth     | Move quadword from xmm2/mem64 to xmm1. */
int movq (code_ptr p,
          const XmmReg (&   modrm_reg),
          const QwordPtr (&   modrm_rm));
/* 66 0F D6  /r RMBoth     | Move quadword from xmm1 to xmm2/mem64. */
 /* 66 0F D6  /r RMBoth     | Move quadword from xmm1 to xmm2/mem64. */
int movq (code_ptr p,
          const QwordPtr (&   modrm_rm),
          const XmmReg (&   modrm_reg));
/*  0F 6F  /r RMBoth     | Move quadword from mm/m64 to mm. */
 /*  0F 6F  /r RMBoth     | Move quadword from mm/m64 to mm. */
int movq (code_ptr p,
          const MmReg (&   modrm_reg),
          const MmReg (&   modrm_rm));
/*  0F 6F  /r RMBoth     | Move quadword from mm/m64 to mm. */
 /*  0F 6F  /r RMBoth     | Move quadword from mm/m64 to mm. */
int movq (code_ptr p,
          const MmReg (&   modrm_reg),
          const QwordPtr (&   modrm_rm));
/*  0F 7F  /r RMBoth     | Move quadword from mm to mm/m64. */
 /*  0F 7F  /r RMBoth     | Move quadword from mm to mm/m64. */
int movq (code_ptr p,
          const QwordPtr (&   modrm_rm),
          const MmReg (&   modrm_reg));
/* F3 0F D6  /r RMRegOnly     | Move quadword from mmx to low quadword of xmm. */
 /* F3 0F D6  /r RMRegOnly     | Move quadword from mmx to low quadword of xmm. */
int movq2dq (code_ptr p,
             const XmmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  A4        | For legacy mode, Move byte from address DS:(E)SI to ES:(E)DI. For 64-bit mode move byte from address (R|E)SI to (R|E)DI. */
 /*  A4        | For legacy mode, Move byte from address DS:(E)SI to ES:(E)DI. For 64-bit mode move byte from address (R|E)SI to (R|E)DI. */
int movs (code_ptr p,
          const BytePtr_ES_EDI (&   unused),
          const BytePtr_ESI (&   ptr));
/*  A4        | For legacy mode, Move byte from address DS:(E)SI to ES:(E)DI. For 64-bit mode move byte from address (R|E)SI to (R|E)DI. */
 /*  A4        | For legacy mode, Move byte from address DS:(E)SI to ES:(E)DI. For 64-bit mode move byte from address (R|E)SI to (R|E)DI. */
int movs (code_ptr p,
          const BytePtr_ES_DI (&   unused),
          const BytePtr_SI (&   ptr));
/*  A5      op16  | For legacy mode, move word from address DS:(E)SI to ES:(E)DI. For 64-bit mode move word at address (R|E)SI to (R|E)DI. */
 /*  A5      op16  | For legacy mode, move word from address DS:(E)SI to ES:(E)DI. For 64-bit mode move word at address (R|E)SI to (R|E)DI. */
int movs (code_ptr p,
          const WordPtr_ES_EDI (&   unused),
          const WordPtr_ESI (&   ptr));
/*  A5      op16  | For legacy mode, move word from address DS:(E)SI to ES:(E)DI. For 64-bit mode move word at address (R|E)SI to (R|E)DI. */
 /*  A5      op16  | For legacy mode, move word from address DS:(E)SI to ES:(E)DI. For 64-bit mode move word at address (R|E)SI to (R|E)DI. */
int movs (code_ptr p,
          const WordPtr_ES_DI (&   unused),
          const WordPtr_SI (&   ptr));
/*  A5      op32  | For legacy mode, move dword from address DS:(E)SI to ES:(E)DI. For 64-bit mode move dword from address (R|E)SI to (R|E)DI. */
 /*  A5      op32  | For legacy mode, move dword from address DS:(E)SI to ES:(E)DI. For 64-bit mode move dword from address (R|E)SI to (R|E)DI. */
int movs (code_ptr p,
          const DwordPtr_ES_EDI (&   unused),
          const DwordPtr_ESI (&   ptr));
/*  A5      op32  | For legacy mode, move dword from address DS:(E)SI to ES:(E)DI. For 64-bit mode move dword from address (R|E)SI to (R|E)DI. */
 /*  A5      op32  | For legacy mode, move dword from address DS:(E)SI to ES:(E)DI. For 64-bit mode move dword from address (R|E)SI to (R|E)DI. */
int movs (code_ptr p,
          const DwordPtr_ES_DI (&   unused),
          const DwordPtr_SI (&   ptr));
/*  A4        | For legacy mode, Move byte from address DS:(E)SI to ES:(E)DI. For 64-bit mode move byte from address (R|E)SI to (R|E)DI. */
 /*  A4        | For legacy mode, Move byte from address DS:(E)SI to ES:(E)DI. For 64-bit mode move byte from address (R|E)SI to (R|E)DI. */
int movsb (code_ptr p);
/*  A5      op32  | For legacy mode, move dword from address DS:(E)SI to ES:(E)DI. For 64-bit mode move dword from address (R|E)SI to (R|E)DI. */
 /*  A5      op32  | For legacy mode, move dword from address DS:(E)SI to ES:(E)DI. For 64-bit mode move dword from address (R|E)SI to (R|E)DI. */
int movsd (code_ptr p);
/* F2 0F 10  /r RMBoth     | Move scalar double- precision floating-point value from xmm2/m64 to xmm1 register. */
 /* F2 0F 10  /r RMBoth     | Move scalar double- precision floating-point value from xmm2/m64 to xmm1 register. */
int movsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F2 0F 10  /r RMBoth     | Move scalar double- precision floating-point value from xmm2/m64 to xmm1 register. */
 /* F2 0F 10  /r RMBoth     | Move scalar double- precision floating-point value from xmm2/m64 to xmm1 register. */
int movsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* F2 0F 11  /r RMBoth     | Move scalar double- precision floating-point value from xmm1 register to xmm2/m64. */
 /* F2 0F 11  /r RMBoth     | Move scalar double- precision floating-point value from xmm1 register to xmm2/m64. */
int movsd (code_ptr p,
           const QwordPtr (&   modrm_rm),
           const XmmReg (&   modrm_reg));
/* F3 0F 16  /r RMBoth     | Move two single-precision floating-point values from the higher 32-bit operand of each qword in xmm2/m128 to xmm1 and duplicate each 32-bit operand to the lower 32-bits of each qword. */
 /* F3 0F 16  /r RMBoth     | Move two single-precision floating-point values from the higher 32-bit operand of each qword in xmm2/m128 to xmm1 and duplicate each 32-bit operand to the lower 32-bits of each qword. */
int movshdup (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F 16  /r RMBoth     | Move two single-precision floating-point values from the higher 32-bit operand of each qword in xmm2/m128 to xmm1 and duplicate each 32-bit operand to the lower 32-bits of each qword. */
 /* F3 0F 16  /r RMBoth     | Move two single-precision floating-point values from the higher 32-bit operand of each qword in xmm2/m128 to xmm1 and duplicate each 32-bit operand to the lower 32-bits of each qword. */
int movshdup (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* F3 0F 12  /r RMBoth     | Move two single-precision floating-point values from the lower 32-bit operand of each qword in xmm2/m128 to xmm1 and duplicate each 32-bit operand to the higher 32-bits of each qword. */
 /* F3 0F 12  /r RMBoth     | Move two single-precision floating-point values from the lower 32-bit operand of each qword in xmm2/m128 to xmm1 and duplicate each 32-bit operand to the higher 32-bits of each qword. */
int movsldup (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* F3 0F 12  /r RMBoth     | Move two single-precision floating-point values from the lower 32-bit operand of each qword in xmm2/m128 to xmm1 and duplicate each 32-bit operand to the higher 32-bits of each qword. */
 /* F3 0F 12  /r RMBoth     | Move two single-precision floating-point values from the lower 32-bit operand of each qword in xmm2/m128 to xmm1 and duplicate each 32-bit operand to the higher 32-bits of each qword. */
int movsldup (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* F3 0F 10  /r RMBoth     | Move scalar single-precision floating-point value from xmm2/m32 to xmm1 register. */
 /* F3 0F 10  /r RMBoth     | Move scalar single-precision floating-point value from xmm2/m32 to xmm1 register. */
int movss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F3 0F 10  /r RMBoth     | Move scalar single-precision floating-point value from xmm2/m32 to xmm1 register. */
 /* F3 0F 10  /r RMBoth     | Move scalar single-precision floating-point value from xmm2/m32 to xmm1 register. */
int movss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/* F3 0F 11  /r RMBoth     | Move scalar single-precision floating-point value from xmm1 register to xmm2/m32. */
 /* F3 0F 11  /r RMBoth     | Move scalar single-precision floating-point value from xmm1 register to xmm2/m32. */
int movss (code_ptr p,
           const DwordPtr (&   modrm_rm),
           const XmmReg (&   modrm_reg));
/*  A5      op16  | For legacy mode, move word from address DS:(E)SI to ES:(E)DI. For 64-bit mode move word at address (R|E)SI to (R|E)DI. */
 /*  A5      op16  | For legacy mode, move word from address DS:(E)SI to ES:(E)DI. For 64-bit mode move word at address (R|E)SI to (R|E)DI. */
int movsw (code_ptr p);
/*  0F BE  /r RMBoth   op16  | Move byte to word with sign-extension. */
 /*  0F BE  /r RMBoth   op16  | Move byte to word with sign-extension. */
int movsx (code_ptr p,
           const WordReg (&   modrm_reg),
           const ByteReg (&   modrm_rm));
/*  0F BE  /r RMBoth   op16  | Move byte to word with sign-extension. */
 /*  0F BE  /r RMBoth   op16  | Move byte to word with sign-extension. */
int movsx (code_ptr p,
           const WordReg (&   modrm_reg),
           const BytePtr (&   modrm_rm));
/*  0F BE  /r RMBoth   op32  | Move byte to doubleword with sign-extension. */
 /*  0F BE  /r RMBoth   op32  | Move byte to doubleword with sign-extension. */
int movsx (code_ptr p,
           const DwordReg (&   modrm_reg),
           const ByteReg (&   modrm_rm));
/*  0F BE  /r RMBoth   op32  | Move byte to doubleword with sign-extension. */
 /*  0F BE  /r RMBoth   op32  | Move byte to doubleword with sign-extension. */
int movsx (code_ptr p,
           const DwordReg (&   modrm_reg),
           const BytePtr (&   modrm_rm));
/*  0F BF  /r RMBoth     | Move word to doubleword, with sign-extension. */
 /*  0F BF  /r RMBoth     | Move word to doubleword, with sign-extension. */
int movsx (code_ptr p,
           const DwordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F BF  /r RMBoth     | Move word to doubleword, with sign-extension. */
 /*  0F BF  /r RMBoth     | Move word to doubleword, with sign-extension. */
int movsx (code_ptr p,
           const DwordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/* 66 0F 10  /r RMBoth     | Move packed double- precision floating-point values from xmm2/m128 to xmm1. */
 /* 66 0F 10  /r RMBoth     | Move packed double- precision floating-point values from xmm2/m128 to xmm1. */
int movupd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 10  /r RMBoth     | Move packed double- precision floating-point values from xmm2/m128 to xmm1. */
 /* 66 0F 10  /r RMBoth     | Move packed double- precision floating-point values from xmm2/m128 to xmm1. */
int movupd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 11  /r RMBoth     | Move packed double- precision floating-point values from xmm1 to xmm2/m128. */
 /* 66 0F 11  /r RMBoth     | Move packed double- precision floating-point values from xmm1 to xmm2/m128. */
int movupd (code_ptr p,
            const XmmWordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/*  0F 10  /r RMBoth     | Move packed single- precision floating-point values from xmm2/m128 to xmm1. */
 /*  0F 10  /r RMBoth     | Move packed single- precision floating-point values from xmm2/m128 to xmm1. */
int movups (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/*  0F 10  /r RMBoth     | Move packed single- precision floating-point values from xmm2/m128 to xmm1. */
 /*  0F 10  /r RMBoth     | Move packed single- precision floating-point values from xmm2/m128 to xmm1. */
int movups (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 11  /r RMBoth     | Move packed single- precision floating-point values from xmm1 to xmm2/m128. */
 /*  0F 11  /r RMBoth     | Move packed single- precision floating-point values from xmm1 to xmm2/m128. */
int movups (code_ptr p,
            const XmmWordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg));
/*  0F B6  /r RMBoth   op16  | Move byte to word with zero-extension. */
 /*  0F B6  /r RMBoth   op16  | Move byte to word with zero-extension. */
int movzx (code_ptr p,
           const WordReg (&   modrm_reg),
           const ByteReg (&   modrm_rm));
/*  0F B6  /r RMBoth   op16  | Move byte to word with zero-extension. */
 /*  0F B6  /r RMBoth   op16  | Move byte to word with zero-extension. */
int movzx (code_ptr p,
           const WordReg (&   modrm_reg),
           const BytePtr (&   modrm_rm));
/*  0F B6  /r RMBoth   op32  | Move byte to doubleword, zero-extension. */
 /*  0F B6  /r RMBoth   op32  | Move byte to doubleword, zero-extension. */
int movzx (code_ptr p,
           const DwordReg (&   modrm_reg),
           const ByteReg (&   modrm_rm));
/*  0F B6  /r RMBoth   op32  | Move byte to doubleword, zero-extension. */
 /*  0F B6  /r RMBoth   op32  | Move byte to doubleword, zero-extension. */
int movzx (code_ptr p,
           const DwordReg (&   modrm_reg),
           const BytePtr (&   modrm_rm));
/*  0F B7  /r RMBoth     | Move word to doubleword, zero-extension. */
 /*  0F B7  /r RMBoth     | Move word to doubleword, zero-extension. */
int movzx (code_ptr p,
           const DwordReg (&   modrm_reg),
           const WordReg (&   modrm_rm));
/*  0F B7  /r RMBoth     | Move word to doubleword, zero-extension. */
 /*  0F B7  /r RMBoth     | Move word to doubleword, zero-extension. */
int movzx (code_ptr p,
           const DwordReg (&   modrm_reg),
           const WordPtr (&   modrm_rm));
/* 66 0F 3A 42  /r RMBoth  ib   | Sums absolute 8-bit integer difference of adjacent groups of 4 byte integers in xmm1 and xmm2/m128 and writes the results in xmm1. Starting offsets within xmm1 and xmm2/m128 are determined by imm8. */
 /* 66 0F 3A 42  /r RMBoth  ib   | Sums absolute 8-bit integer difference of adjacent groups of 4 byte integers in xmm1 and xmm2/m128 and writes the results in xmm1. Starting offsets within xmm1 and xmm2/m128 are determined by imm8. */
int mpsadbw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 42  /r RMBoth  ib   | Sums absolute 8-bit integer difference of adjacent groups of 4 byte integers in xmm1 and xmm2/m128 and writes the results in xmm1. Starting offsets within xmm1 and xmm2/m128 are determined by imm8. */
 /* 66 0F 3A 42  /r RMBoth  ib   | Sums absolute 8-bit integer difference of adjacent groups of 4 byte integers in xmm1 and xmm2/m128 and writes the results in xmm1. Starting offsets within xmm1 and xmm2/m128 are determined by imm8. */
int mpsadbw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/*  F6  /4 RMBoth     | Unsigned multiply (AX ← AL ∗ r/m8). */
 /*  F6  /4 RMBoth     | Unsigned multiply (AX ← AL ∗ r/m8). */
int mul (code_ptr p,
         const ByteReg (&   modrm_rm));
/*  F6  /4 RMBoth     | Unsigned multiply (AX ← AL ∗ r/m8). */
 /*  F6  /4 RMBoth     | Unsigned multiply (AX ← AL ∗ r/m8). */
int mul (code_ptr p,
         const BytePtr (&   modrm_rm));
/*  F7  /4 RMBoth   op16  | Unsigned multiply (DX:AX ← AX ∗ r/m16). */
 /*  F7  /4 RMBoth   op16  | Unsigned multiply (DX:AX ← AX ∗ r/m16). */
int mul (code_ptr p,
         const WordReg (&   modrm_rm));
/*  F7  /4 RMBoth   op16  | Unsigned multiply (DX:AX ← AX ∗ r/m16). */
 /*  F7  /4 RMBoth   op16  | Unsigned multiply (DX:AX ← AX ∗ r/m16). */
int mul (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  F7  /4 RMBoth   op32  | Unsigned multiply (EDX:EAX ← EAX ∗ r/m32). */
 /*  F7  /4 RMBoth   op32  | Unsigned multiply (EDX:EAX ← EAX ∗ r/m32). */
int mul (code_ptr p,
         const DwordReg (&   modrm_rm));
/*  F7  /4 RMBoth   op32  | Unsigned multiply (EDX:EAX ← EAX ∗ r/m32). */
 /*  F7  /4 RMBoth   op32  | Unsigned multiply (EDX:EAX ← EAX ∗ r/m32). */
int mul (code_ptr p,
         const DwordPtr (&   modrm_rm));
/* 66 0F 59  /r RMBoth     | Multiply packed double- precision floating-point values in xmm2/m128 by xmm1. */
 /* 66 0F 59  /r RMBoth     | Multiply packed double- precision floating-point values in xmm2/m128 by xmm1. */
int mulpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 59  /r RMBoth     | Multiply packed double- precision floating-point values in xmm2/m128 by xmm1. */
 /* 66 0F 59  /r RMBoth     | Multiply packed double- precision floating-point values in xmm2/m128 by xmm1. */
int mulpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 59  /r RMBoth     | Multiply packed single- precision floating-point values in xmm2/mem by xmm1. */
 /*  0F 59  /r RMBoth     | Multiply packed single- precision floating-point values in xmm2/mem by xmm1. */
int mulps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 59  /r RMBoth     | Multiply packed single- precision floating-point values in xmm2/mem by xmm1. */
 /*  0F 59  /r RMBoth     | Multiply packed single- precision floating-point values in xmm2/mem by xmm1. */
int mulps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* F2 0F 59  /r RMBoth     | Multiply the low double- precision floating-point value in xmm2/mem64 by low double-precision floating-point value in xmm1. */
 /* F2 0F 59  /r RMBoth     | Multiply the low double- precision floating-point value in xmm2/mem64 by low double-precision floating-point value in xmm1. */
int mulsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F2 0F 59  /r RMBoth     | Multiply the low double- precision floating-point value in xmm2/mem64 by low double-precision floating-point value in xmm1. */
 /* F2 0F 59  /r RMBoth     | Multiply the low double- precision floating-point value in xmm2/mem64 by low double-precision floating-point value in xmm1. */
int mulsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* F3 0F 59  /r RMBoth     | Multiply the low single- precision floating-point value in xmm2/mem by the low single-precision floating-point value in xmm1. */
 /* F3 0F 59  /r RMBoth     | Multiply the low single- precision floating-point value in xmm2/mem by the low single-precision floating-point value in xmm1. */
int mulss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F3 0F 59  /r RMBoth     | Multiply the low single- precision floating-point value in xmm2/mem by the low single-precision floating-point value in xmm1. */
 /* F3 0F 59  /r RMBoth     | Multiply the low single- precision floating-point value in xmm2/mem by the low single-precision floating-point value in xmm1. */
int mulss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 01 C9        | A hint that allow the processor to stop instruction execution and enter an implementation- dependent optimized state until occurrence of a class of events. */
 /*  0F 01 C9        | A hint that allow the processor to stop instruction execution and enter an implementation- dependent optimized state until occurrence of a class of events. */
int mwait (code_ptr p);
/*  F6  /3 RMBoth     | Two's complement negate r/m8. */
 /*  F6  /3 RMBoth     | Two's complement negate r/m8. */
int neg (code_ptr p,
         const ByteReg (&   modrm_rm));
/*  F6  /3 RMBoth     | Two's complement negate r/m8. */
 /*  F6  /3 RMBoth     | Two's complement negate r/m8. */
int neg (code_ptr p,
         const BytePtr (&   modrm_rm));
/*  F7  /3 RMBoth   op16  | Two's complement negate r/m16. */
 /*  F7  /3 RMBoth   op16  | Two's complement negate r/m16. */
int neg (code_ptr p,
         const WordReg (&   modrm_rm));
/*  F7  /3 RMBoth   op16  | Two's complement negate r/m16. */
 /*  F7  /3 RMBoth   op16  | Two's complement negate r/m16. */
int neg (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  F7  /3 RMBoth   op32  | Two's complement negate r/m32. */
 /*  F7  /3 RMBoth   op32  | Two's complement negate r/m32. */
int neg (code_ptr p,
         const DwordReg (&   modrm_rm));
/*  F7  /3 RMBoth   op32  | Two's complement negate r/m32. */
 /*  F7  /3 RMBoth   op32  | Two's complement negate r/m32. */
int neg (code_ptr p,
         const DwordPtr (&   modrm_rm));
/*  90        | One byte no-operation instruction. */
 /*  90        | One byte no-operation instruction. */
int nop (code_ptr p);
/*  0F 1F  /0 RMBoth   op16  | Multi-byte no-operation instruction. */
 /*  0F 1F  /0 RMBoth   op16  | Multi-byte no-operation instruction. */
int nop (code_ptr p,
         const WordReg (&   modrm_rm));
/*  0F 1F  /0 RMBoth   op16  | Multi-byte no-operation instruction. */
 /*  0F 1F  /0 RMBoth   op16  | Multi-byte no-operation instruction. */
int nop (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  0F 1F  /0 RMBoth   op32  | Multi-byte no-operation instruction. */
 /*  0F 1F  /0 RMBoth   op32  | Multi-byte no-operation instruction. */
int nop (code_ptr p,
         const DwordReg (&   modrm_rm));
/*  0F 1F  /0 RMBoth   op32  | Multi-byte no-operation instruction. */
 /*  0F 1F  /0 RMBoth   op32  | Multi-byte no-operation instruction. */
int nop (code_ptr p,
         const DwordPtr (&   modrm_rm));
/*  F6  /2 RMBoth     | Reverse each bit of r/m8. */
 /*  F6  /2 RMBoth     | Reverse each bit of r/m8. */
int not_ (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  F6  /2 RMBoth     | Reverse each bit of r/m8. */
 /*  F6  /2 RMBoth     | Reverse each bit of r/m8. */
int not_ (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  F7  /2 RMBoth   op16  | Reverse each bit of r/m16. */
 /*  F7  /2 RMBoth   op16  | Reverse each bit of r/m16. */
int not_ (code_ptr p,
          const WordReg (&   modrm_rm));
/*  F7  /2 RMBoth   op16  | Reverse each bit of r/m16. */
 /*  F7  /2 RMBoth   op16  | Reverse each bit of r/m16. */
int not_ (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  F7  /2 RMBoth   op32  | Reverse each bit of r/m32. */
 /*  F7  /2 RMBoth   op32  | Reverse each bit of r/m32. */
int not_ (code_ptr p,
          const DwordReg (&   modrm_rm));
/*  F7  /2 RMBoth   op32  | Reverse each bit of r/m32. */
 /*  F7  /2 RMBoth   op32  | Reverse each bit of r/m32. */
int not_ (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  0D     id op32  | EAX OR imm32. */
 /*  0D     id op32  | EAX OR imm32. */
int or_ (code_ptr p,
         const RegEAX (&   unused),
         imm32_t imm);
/*  83  /1 RMBoth  ib op32  | r/m32 OR imm8 (sign- extended). */
 /*  83  /1 RMBoth  ib op32  | r/m32 OR imm8 (sign- extended). */
int or_ (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm8_t imm);
/*  81  /1 RMBoth  id op32  | r/m32 OR imm32. */
 /*  81  /1 RMBoth  id op32  | r/m32 OR imm32. */
int or_ (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm32_t imm);
/*  81  /1 RMBoth  id op32  | r/m32 OR imm32. */
 /*  81  /1 RMBoth  id op32  | r/m32 OR imm32. */
int or_ (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm32_t imm);
/*  83  /1 RMBoth  ib op32  | r/m32 OR imm8 (sign- extended). */
 /*  83  /1 RMBoth  ib op32  | r/m32 OR imm8 (sign- extended). */
int or_ (code_ptr p,
         const RegEAX (&   modrm_rm),
         imm8_t imm);
/*  83  /1 RMBoth  ib op32  | r/m32 OR imm8 (sign- extended). */
 /*  83  /1 RMBoth  ib op32  | r/m32 OR imm8 (sign- extended). */
int or_ (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  0D     iw op16  | AX OR imm16. */
 /*  0D     iw op16  | AX OR imm16. */
int or_ (code_ptr p,
         const RegAX (&   unused),
         imm16_t imm);
/*  83  /1 RMBoth  ib op16  | r/m16 OR imm8 (sign- extended). */
 /*  83  /1 RMBoth  ib op16  | r/m16 OR imm8 (sign- extended). */
int or_ (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm8_t imm);
/*  81  /1 RMBoth  iw op16  | r/m16 OR imm16. */
 /*  81  /1 RMBoth  iw op16  | r/m16 OR imm16. */
int or_ (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm16_t imm);
/*  81  /1 RMBoth  iw op16  | r/m16 OR imm16. */
 /*  81  /1 RMBoth  iw op16  | r/m16 OR imm16. */
int or_ (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm16_t imm);
/*  83  /1 RMBoth  ib op16  | r/m16 OR imm8 (sign- extended). */
 /*  83  /1 RMBoth  ib op16  | r/m16 OR imm8 (sign- extended). */
int or_ (code_ptr p,
         const RegAX (&   modrm_rm),
         imm8_t imm);
/*  83  /1 RMBoth  ib op16  | r/m16 OR imm8 (sign- extended). */
 /*  83  /1 RMBoth  ib op16  | r/m16 OR imm8 (sign- extended). */
int or_ (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  0B  /r RMBoth   op32  | r32 OR r/m32. */
 /*  0B  /r RMBoth   op32  | r32 OR r/m32. */
int or_ (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  0B  /r RMBoth   op32  | r32 OR r/m32. */
 /*  0B  /r RMBoth   op32  | r32 OR r/m32. */
int or_ (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  09  /r RMBoth   op32  | r/m32 OR r32. */
 /*  09  /r RMBoth   op32  | r/m32 OR r32. */
int or_ (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  0B  /r RMBoth   op16  | r16 OR r/m16. */
 /*  0B  /r RMBoth   op16  | r16 OR r/m16. */
int or_ (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  0B  /r RMBoth   op16  | r16 OR r/m16. */
 /*  0B  /r RMBoth   op16  | r16 OR r/m16. */
int or_ (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  09  /r RMBoth   op16  | r/m16 OR r16. */
 /*  09  /r RMBoth   op16  | r/m16 OR r16. */
int or_ (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  0A  /r RMBoth     | r8 OR r/m8. */
 /*  0A  /r RMBoth     | r8 OR r/m8. */
int or_ (code_ptr p,
         const ByteReg (&   modrm_reg),
         const ByteReg (&   modrm_rm));
/*  0A  /r RMBoth     | r8 OR r/m8. */
 /*  0A  /r RMBoth     | r8 OR r/m8. */
int or_ (code_ptr p,
         const ByteReg (&   modrm_reg),
         const BytePtr (&   modrm_rm));
/*  08  /r RMBoth     | r/m8 OR r8. */
 /*  08  /r RMBoth     | r/m8 OR r8. */
int or_ (code_ptr p,
         const BytePtr (&   modrm_rm),
         const ByteReg (&   modrm_reg));
/*  0C     ib   | AL OR imm8. */
 /*  0C     ib   | AL OR imm8. */
int or_ (code_ptr p,
         const RegAL (&   unused),
         imm8_t imm);
/*  80  /1 RMBoth  ib   | r/m8 OR imm8. */
 /*  80  /1 RMBoth  ib   | r/m8 OR imm8. */
int or_ (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  80  /1 RMBoth  ib   | r/m8 OR imm8. */
 /*  80  /1 RMBoth  ib   | r/m8 OR imm8. */
int or_ (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/* 66 0F 56  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
 /* 66 0F 56  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
int orpd (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmReg (&   modrm_rm));
/* 66 0F 56  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
 /* 66 0F 56  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
int orpd (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmWordPtr (&   modrm_rm));
/*  0F 56  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
 /*  0F 56  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
int orps (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmReg (&   modrm_rm));
/*  0F 56  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
 /*  0F 56  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
int orps (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmWordPtr (&   modrm_rm));
/*  E6     ib   | Output byte in AL to I/O port address imm8. */
 /*  E6     ib   | Output byte in AL to I/O port address imm8. */
int out (code_ptr p,
         imm8_t imm,
         const RegAL (&   unused));
/*  E7     ib op16  | Output word in AX to I/O port address imm8. */
 /*  E7     ib op16  | Output word in AX to I/O port address imm8. */
int out (code_ptr p,
         imm8_t imm,
         const RegAX (&   unused));
/*  E7     ib op32  | Output doubleword in EAX to I/O port address imm8. */
 /*  E7     ib op32  | Output doubleword in EAX to I/O port address imm8. */
int out (code_ptr p,
         imm8_t imm,
         const RegEAX (&   unused));
/*  EE        | Output byte in AL to I/O port address in DX. */
 /*  EE        | Output byte in AL to I/O port address in DX. */
int out (code_ptr p,
         const RegDX (&   unused1),
         const RegAL (&   unused2));
/*  EF      op16  | Output word in AX to I/O port address in DX. */
 /*  EF      op16  | Output word in AX to I/O port address in DX. */
int out (code_ptr p,
         const RegDX (&   unused1),
         const RegAX (&   unused2));
/*  EF      op32  | Output doubleword in EAX to I/O port address in DX. */
 /*  EF      op32  | Output doubleword in EAX to I/O port address in DX. */
int out (code_ptr p,
         const RegDX (&   unused1),
         const RegEAX (&   unused2));
/*  6E        | Output byte from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6E        | Output byte from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outs (code_ptr p,
          const RegDX (&   unused),
          const BytePtr_ESI (&   ptr));
/*  6E        | Output byte from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6E        | Output byte from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outs (code_ptr p,
          const RegDX (&   unused),
          const BytePtr_SI (&   ptr));
/*  6F      op16  | Output word from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6F      op16  | Output word from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outs (code_ptr p,
          const RegDX (&   unused),
          const WordPtr_ESI (&   ptr));
/*  6F      op16  | Output word from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6F      op16  | Output word from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outs (code_ptr p,
          const RegDX (&   unused),
          const WordPtr_SI (&   ptr));
/*  6F      op32  | Output doubleword from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6F      op32  | Output doubleword from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outs (code_ptr p,
          const RegDX (&   unused),
          const DwordPtr_ESI (&   ptr));
/*  6F      op32  | Output doubleword from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6F      op32  | Output doubleword from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outs (code_ptr p,
          const RegDX (&   unused),
          const DwordPtr_SI (&   ptr));
/*  6E        | Output byte from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6E        | Output byte from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outsb (code_ptr p);
/*  6F      op32  | Output doubleword from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6F      op32  | Output doubleword from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outsd (code_ptr p);
/*  6F      op16  | Output word from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
 /*  6F      op16  | Output word from memory location specified in DS:(E)SI or RSI to I/O port specified in DX**. */
int outsw (code_ptr p);
/*  0F 38 1C  /r RMBoth     | Compute the absolute value of bytes in mm2/m64 and store UNSIGNED result in mm1. */
 /*  0F 38 1C  /r RMBoth     | Compute the absolute value of bytes in mm2/m64 and store UNSIGNED result in mm1. */
int pabsb (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F 38 1C  /r RMBoth     | Compute the absolute value of bytes in mm2/m64 and store UNSIGNED result in mm1. */
 /*  0F 38 1C  /r RMBoth     | Compute the absolute value of bytes in mm2/m64 and store UNSIGNED result in mm1. */
int pabsb (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 38 1C  /r RMBoth     | Compute the absolute value of bytes in xmm2/m128 and store UNSIGNED result in xmm1. */
 /* 66 0F 38 1C  /r RMBoth     | Compute the absolute value of bytes in xmm2/m128 and store UNSIGNED result in xmm1. */
int pabsb (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 38 1C  /r RMBoth     | Compute the absolute value of bytes in xmm2/m128 and store UNSIGNED result in xmm1. */
 /* 66 0F 38 1C  /r RMBoth     | Compute the absolute value of bytes in xmm2/m128 and store UNSIGNED result in xmm1. */
int pabsb (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 38 1E  /r RMBoth     | Compute the absolute value of 32-bit integers in mm2/m64 and store UNSIGNED result in mm1. */
 /*  0F 38 1E  /r RMBoth     | Compute the absolute value of 32-bit integers in mm2/m64 and store UNSIGNED result in mm1. */
int pabsd (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F 38 1E  /r RMBoth     | Compute the absolute value of 32-bit integers in mm2/m64 and store UNSIGNED result in mm1. */
 /*  0F 38 1E  /r RMBoth     | Compute the absolute value of 32-bit integers in mm2/m64 and store UNSIGNED result in mm1. */
int pabsd (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 38 1E  /r RMBoth     | Compute the absolute value of 32-bit integers in xmm2/m128 and store UNSIGNED result in xmm1. */
 /* 66 0F 38 1E  /r RMBoth     | Compute the absolute value of 32-bit integers in xmm2/m128 and store UNSIGNED result in xmm1. */
int pabsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 38 1E  /r RMBoth     | Compute the absolute value of 32-bit integers in xmm2/m128 and store UNSIGNED result in xmm1. */
 /* 66 0F 38 1E  /r RMBoth     | Compute the absolute value of 32-bit integers in xmm2/m128 and store UNSIGNED result in xmm1. */
int pabsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 38 1D  /r RMBoth     | Compute the absolute value of 16-bit integers in mm2/m64 and store UNSIGNED result in mm1. */
 /*  0F 38 1D  /r RMBoth     | Compute the absolute value of 16-bit integers in mm2/m64 and store UNSIGNED result in mm1. */
int pabsw (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F 38 1D  /r RMBoth     | Compute the absolute value of 16-bit integers in mm2/m64 and store UNSIGNED result in mm1. */
 /*  0F 38 1D  /r RMBoth     | Compute the absolute value of 16-bit integers in mm2/m64 and store UNSIGNED result in mm1. */
int pabsw (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 38 1D  /r RMBoth     | Compute the absolute value of 16-bit integers in xmm2/m128 and store UNSIGNED result in xmm1. */
 /* 66 0F 38 1D  /r RMBoth     | Compute the absolute value of 16-bit integers in xmm2/m128 and store UNSIGNED result in xmm1. */
int pabsw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 38 1D  /r RMBoth     | Compute the absolute value of 16-bit integers in xmm2/m128 and store UNSIGNED result in xmm1. */
 /* 66 0F 38 1D  /r RMBoth     | Compute the absolute value of 16-bit integers in xmm2/m128 and store UNSIGNED result in xmm1. */
int pabsw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 6B  /r RMBoth     | Converts 2 packed signed doubleword integers from mm1 and from mm2/m64 into 4 packed signed word integers in mm1 using signed saturation. */
 /*  0F 6B  /r RMBoth     | Converts 2 packed signed doubleword integers from mm1 and from mm2/m64 into 4 packed signed word integers in mm1 using signed saturation. */
int packssdw (code_ptr p,
              const MmReg (&   modrm_reg),
              const MmReg (&   modrm_rm));
/*  0F 6B  /r RMBoth     | Converts 2 packed signed doubleword integers from mm1 and from mm2/m64 into 4 packed signed word integers in mm1 using signed saturation. */
 /*  0F 6B  /r RMBoth     | Converts 2 packed signed doubleword integers from mm1 and from mm2/m64 into 4 packed signed word integers in mm1 using signed saturation. */
int packssdw (code_ptr p,
              const MmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 6B  /r RMBoth     | Converts 4 packed signed doubleword integers from xmm1 and from xxm2/m128 into 8 packed signed word integers in xxm1 using signed saturation. */
 /* 66 0F 6B  /r RMBoth     | Converts 4 packed signed doubleword integers from xmm1 and from xxm2/m128 into 8 packed signed word integers in xxm1 using signed saturation. */
int packssdw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 6B  /r RMBoth     | Converts 4 packed signed doubleword integers from xmm1 and from xxm2/m128 into 8 packed signed word integers in xxm1 using signed saturation. */
 /* 66 0F 6B  /r RMBoth     | Converts 4 packed signed doubleword integers from xmm1 and from xxm2/m128 into 8 packed signed word integers in xxm1 using signed saturation. */
int packssdw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F 63  /r RMBoth     | Converts 4 packed signed word integers from mm1 and from mm2/m64 into 8 packed signed byte integers in mm1 using signed saturation. */
 /*  0F 63  /r RMBoth     | Converts 4 packed signed word integers from mm1 and from mm2/m64 into 8 packed signed byte integers in mm1 using signed saturation. */
int packsswb (code_ptr p,
              const MmReg (&   modrm_reg),
              const MmReg (&   modrm_rm));
/*  0F 63  /r RMBoth     | Converts 4 packed signed word integers from mm1 and from mm2/m64 into 8 packed signed byte integers in mm1 using signed saturation. */
 /*  0F 63  /r RMBoth     | Converts 4 packed signed word integers from mm1 and from mm2/m64 into 8 packed signed byte integers in mm1 using signed saturation. */
int packsswb (code_ptr p,
              const MmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 63  /r RMBoth     | Converts 8 packed signed word integers from xmm1 and from xxm2/m128 into 16 packed signed byte integers in xxm1 using signed saturation. */
 /* 66 0F 63  /r RMBoth     | Converts 8 packed signed word integers from xmm1 and from xxm2/m128 into 16 packed signed byte integers in xxm1 using signed saturation. */
int packsswb (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 63  /r RMBoth     | Converts 8 packed signed word integers from xmm1 and from xxm2/m128 into 16 packed signed byte integers in xxm1 using signed saturation. */
 /* 66 0F 63  /r RMBoth     | Converts 8 packed signed word integers from xmm1 and from xxm2/m128 into 16 packed signed byte integers in xxm1 using signed saturation. */
int packsswb (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 2B  /r RMBoth     | Convert 4 packed signed doubleword integers from xmm1 and 4 packed signed doubleword integers from xmm2/m128 into 8 packed unsigned word integers in xmm1 using unsigned saturation. */
 /* 66 0F 38 2B  /r RMBoth     | Convert 4 packed signed doubleword integers from xmm1 and 4 packed signed doubleword integers from xmm2/m128 into 8 packed unsigned word integers in xmm1 using unsigned saturation. */
int packusdw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 2B  /r RMBoth     | Convert 4 packed signed doubleword integers from xmm1 and 4 packed signed doubleword integers from xmm2/m128 into 8 packed unsigned word integers in xmm1 using unsigned saturation. */
 /* 66 0F 38 2B  /r RMBoth     | Convert 4 packed signed doubleword integers from xmm1 and 4 packed signed doubleword integers from xmm2/m128 into 8 packed unsigned word integers in xmm1 using unsigned saturation. */
int packusdw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F 67  /r RMBoth     | Converts 4 signed word integers from mm and 4 signed word integers from mm/m64 into 8 unsigned byte integers in mm using unsigned saturation. */
 /*  0F 67  /r RMBoth     | Converts 4 signed word integers from mm and 4 signed word integers from mm/m64 into 8 unsigned byte integers in mm using unsigned saturation. */
int packuswb (code_ptr p,
              const MmReg (&   modrm_reg),
              const MmReg (&   modrm_rm));
/*  0F 67  /r RMBoth     | Converts 4 signed word integers from mm and 4 signed word integers from mm/m64 into 8 unsigned byte integers in mm using unsigned saturation. */
 /*  0F 67  /r RMBoth     | Converts 4 signed word integers from mm and 4 signed word integers from mm/m64 into 8 unsigned byte integers in mm using unsigned saturation. */
int packuswb (code_ptr p,
              const MmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 67  /r RMBoth     | Converts 8 signed word integers from xmm1 and 8 signed word integers from xmm2/m128 into 16 unsigned byte integers in xmm1 using unsigned saturation. */
 /* 66 0F 67  /r RMBoth     | Converts 8 signed word integers from xmm1 and 8 signed word integers from xmm2/m128 into 16 unsigned byte integers in xmm1 using unsigned saturation. */
int packuswb (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 67  /r RMBoth     | Converts 8 signed word integers from xmm1 and 8 signed word integers from xmm2/m128 into 16 unsigned byte integers in xmm1 using unsigned saturation. */
 /* 66 0F 67  /r RMBoth     | Converts 8 signed word integers from xmm1 and 8 signed word integers from xmm2/m128 into 16 unsigned byte integers in xmm1 using unsigned saturation. */
int packuswb (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F FC  /r RMBoth     | Add packed byte integers from mm/m64 and mm. */
 /*  0F FC  /r RMBoth     | Add packed byte integers from mm/m64 and mm. */
int paddb (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F FC  /r RMBoth     | Add packed byte integers from mm/m64 and mm. */
 /*  0F FC  /r RMBoth     | Add packed byte integers from mm/m64 and mm. */
int paddb (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F FC  /r RMBoth     | Add packed byte integers from xmm2/m128 and xmm1. */
 /* 66 0F FC  /r RMBoth     | Add packed byte integers from xmm2/m128 and xmm1. */
int paddb (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F FC  /r RMBoth     | Add packed byte integers from xmm2/m128 and xmm1. */
 /* 66 0F FC  /r RMBoth     | Add packed byte integers from xmm2/m128 and xmm1. */
int paddb (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F FE  /r RMBoth     | Add packed doubleword integers from mm/m64 and mm. */
 /*  0F FE  /r RMBoth     | Add packed doubleword integers from mm/m64 and mm. */
int paddd (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F FE  /r RMBoth     | Add packed doubleword integers from mm/m64 and mm. */
 /*  0F FE  /r RMBoth     | Add packed doubleword integers from mm/m64 and mm. */
int paddd (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F FE  /r RMBoth     | Add packed doubleword integers from xmm2/m128 and xmm1. */
 /* 66 0F FE  /r RMBoth     | Add packed doubleword integers from xmm2/m128 and xmm1. */
int paddd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F FE  /r RMBoth     | Add packed doubleword integers from xmm2/m128 and xmm1. */
 /* 66 0F FE  /r RMBoth     | Add packed doubleword integers from xmm2/m128 and xmm1. */
int paddd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F D4  /r RMBoth     | Add quadword integer mm2/m64 to mm1. */
 /*  0F D4  /r RMBoth     | Add quadword integer mm2/m64 to mm1. */
int paddq (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F D4  /r RMBoth     | Add quadword integer mm2/m64 to mm1. */
 /*  0F D4  /r RMBoth     | Add quadword integer mm2/m64 to mm1. */
int paddq (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F D4  /r RMBoth     | Add packed quadword integers xmm2/m128 to xmm1. */
 /* 66 0F D4  /r RMBoth     | Add packed quadword integers xmm2/m128 to xmm1. */
int paddq (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F D4  /r RMBoth     | Add packed quadword integers xmm2/m128 to xmm1. */
 /* 66 0F D4  /r RMBoth     | Add packed quadword integers xmm2/m128 to xmm1. */
int paddq (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F EC  /r RMBoth     | Add packed signed byte integers from mm/m64 and mm and saturate the results. */
 /*  0F EC  /r RMBoth     | Add packed signed byte integers from mm/m64 and mm and saturate the results. */
int paddsb (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F EC  /r RMBoth     | Add packed signed byte integers from mm/m64 and mm and saturate the results. */
 /*  0F EC  /r RMBoth     | Add packed signed byte integers from mm/m64 and mm and saturate the results. */
int paddsb (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F EC  /r RMBoth     | Add packed signed byte integers from xmm2/m128 and xmm1 saturate the results. */
 /* 66 0F EC  /r RMBoth     | Add packed signed byte integers from xmm2/m128 and xmm1 saturate the results. */
int paddsb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F EC  /r RMBoth     | Add packed signed byte integers from xmm2/m128 and xmm1 saturate the results. */
 /* 66 0F EC  /r RMBoth     | Add packed signed byte integers from xmm2/m128 and xmm1 saturate the results. */
int paddsb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F ED  /r RMBoth     | Add packed signed word integers from mm/m64 and mm and saturate the results. */
 /*  0F ED  /r RMBoth     | Add packed signed word integers from mm/m64 and mm and saturate the results. */
int paddsw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F ED  /r RMBoth     | Add packed signed word integers from mm/m64 and mm and saturate the results. */
 /*  0F ED  /r RMBoth     | Add packed signed word integers from mm/m64 and mm and saturate the results. */
int paddsw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F ED  /r RMBoth     | Add packed signed word integers from xmm2/m128 and xmm1 and saturate the results. */
 /* 66 0F ED  /r RMBoth     | Add packed signed word integers from xmm2/m128 and xmm1 and saturate the results. */
int paddsw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F ED  /r RMBoth     | Add packed signed word integers from xmm2/m128 and xmm1 and saturate the results. */
 /* 66 0F ED  /r RMBoth     | Add packed signed word integers from xmm2/m128 and xmm1 and saturate the results. */
int paddsw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F DC  /r RMBoth     | Add packed unsigned byte integers from mm/m64 and mm and saturate the results. */
 /*  0F DC  /r RMBoth     | Add packed unsigned byte integers from mm/m64 and mm and saturate the results. */
int paddusb (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F DC  /r RMBoth     | Add packed unsigned byte integers from mm/m64 and mm and saturate the results. */
 /*  0F DC  /r RMBoth     | Add packed unsigned byte integers from mm/m64 and mm and saturate the results. */
int paddusb (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F DC  /r RMBoth     | Add packed unsigned byte integers from xmm2/m128 and xmm1 saturate the results. */
 /* 66 0F DC  /r RMBoth     | Add packed unsigned byte integers from xmm2/m128 and xmm1 saturate the results. */
int paddusb (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F DC  /r RMBoth     | Add packed unsigned byte integers from xmm2/m128 and xmm1 saturate the results. */
 /* 66 0F DC  /r RMBoth     | Add packed unsigned byte integers from xmm2/m128 and xmm1 saturate the results. */
int paddusb (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F DD  /r RMBoth     | Add packed unsigned word integers from mm/m64 and mm and saturate the results. */
 /*  0F DD  /r RMBoth     | Add packed unsigned word integers from mm/m64 and mm and saturate the results. */
int paddusw (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F DD  /r RMBoth     | Add packed unsigned word integers from mm/m64 and mm and saturate the results. */
 /*  0F DD  /r RMBoth     | Add packed unsigned word integers from mm/m64 and mm and saturate the results. */
int paddusw (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F DD  /r RMBoth     | Add packed unsigned word integers from xmm2/m128 to xmm1 and saturate the results. */
 /* 66 0F DD  /r RMBoth     | Add packed unsigned word integers from xmm2/m128 to xmm1 and saturate the results. */
int paddusw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F DD  /r RMBoth     | Add packed unsigned word integers from xmm2/m128 to xmm1 and saturate the results. */
 /* 66 0F DD  /r RMBoth     | Add packed unsigned word integers from xmm2/m128 to xmm1 and saturate the results. */
int paddusw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F FD  /r RMBoth     | Add packed word integers from mm/m64 and mm. */
 /*  0F FD  /r RMBoth     | Add packed word integers from mm/m64 and mm. */
int paddw (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F FD  /r RMBoth     | Add packed word integers from mm/m64 and mm. */
 /*  0F FD  /r RMBoth     | Add packed word integers from mm/m64 and mm. */
int paddw (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F FD  /r RMBoth     | Add packed word integers from xmm2/m128 and xmm1. */
 /* 66 0F FD  /r RMBoth     | Add packed word integers from xmm2/m128 and xmm1. */
int paddw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F FD  /r RMBoth     | Add packed word integers from xmm2/m128 and xmm1. */
 /* 66 0F FD  /r RMBoth     | Add packed word integers from xmm2/m128 and xmm1. */
int paddw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 3A 0F  /r RMBoth  ib   | Concatenate destination and source operands, extract byte-aligned result shifted to the right by constant value in imm8 into mm1. */
 /*  0F 3A 0F  /r RMBoth  ib   | Concatenate destination and source operands, extract byte-aligned result shifted to the right by constant value in imm8 into mm1. */
int palignr (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm),
             imm8_t imm);
/*  0F 3A 0F  /r RMBoth  ib   | Concatenate destination and source operands, extract byte-aligned result shifted to the right by constant value in imm8 into mm1. */
 /*  0F 3A 0F  /r RMBoth  ib   | Concatenate destination and source operands, extract byte-aligned result shifted to the right by constant value in imm8 into mm1. */
int palignr (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0F  /r RMBoth  ib   | Concatenate destination and source operands, extract byte-aligned result shifted to the right by constant value in imm8 into xmm1 */
 /* 66 0F 3A 0F  /r RMBoth  ib   | Concatenate destination and source operands, extract byte-aligned result shifted to the right by constant value in imm8 into xmm1 */
int palignr (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0F  /r RMBoth  ib   | Concatenate destination and source operands, extract byte-aligned result shifted to the right by constant value in imm8 into xmm1 */
 /* 66 0F 3A 0F  /r RMBoth  ib   | Concatenate destination and source operands, extract byte-aligned result shifted to the right by constant value in imm8 into xmm1 */
int palignr (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/*  0F DB  /r RMBoth     | Bitwise AND mm/m64 and mm. */
 /*  0F DB  /r RMBoth     | Bitwise AND mm/m64 and mm. */
int pand (code_ptr p,
          const MmReg (&   modrm_reg),
          const MmReg (&   modrm_rm));
/*  0F DB  /r RMBoth     | Bitwise AND mm/m64 and mm. */
 /*  0F DB  /r RMBoth     | Bitwise AND mm/m64 and mm. */
int pand (code_ptr p,
          const MmReg (&   modrm_reg),
          const QwordPtr (&   modrm_rm));
/* 66 0F DB  /r RMBoth     | Bitwise AND of xmm2/m128 and xmm1. */
 /* 66 0F DB  /r RMBoth     | Bitwise AND of xmm2/m128 and xmm1. */
int pand (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmReg (&   modrm_rm));
/* 66 0F DB  /r RMBoth     | Bitwise AND of xmm2/m128 and xmm1. */
 /* 66 0F DB  /r RMBoth     | Bitwise AND of xmm2/m128 and xmm1. */
int pand (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmWordPtr (&   modrm_rm));
/*  0F DF  /r RMBoth     | Bitwise AND NOT of mm/m64 and mm. */
 /*  0F DF  /r RMBoth     | Bitwise AND NOT of mm/m64 and mm. */
int pandn (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F DF  /r RMBoth     | Bitwise AND NOT of mm/m64 and mm. */
 /*  0F DF  /r RMBoth     | Bitwise AND NOT of mm/m64 and mm. */
int pandn (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F DF  /r RMBoth     | Bitwise AND NOT of xmm2/m128 and xmm1. */
 /* 66 0F DF  /r RMBoth     | Bitwise AND NOT of xmm2/m128 and xmm1. */
int pandn (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F DF  /r RMBoth     | Bitwise AND NOT of xmm2/m128 and xmm1. */
 /* 66 0F DF  /r RMBoth     | Bitwise AND NOT of xmm2/m128 and xmm1. */
int pandn (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* F3 90        | Gives hint to processor that improves performance of spin-wait loops. */
 /* F3 90        | Gives hint to processor that improves performance of spin-wait loops. */
int pause (code_ptr p);
/*  0F E0  /r RMBoth     | Average packed unsigned byte integers from mm2/m64 and mm1 with rounding. */
 /*  0F E0  /r RMBoth     | Average packed unsigned byte integers from mm2/m64 and mm1 with rounding. */
int pavgb (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F E0  /r RMBoth     | Average packed unsigned byte integers from mm2/m64 and mm1 with rounding. */
 /*  0F E0  /r RMBoth     | Average packed unsigned byte integers from mm2/m64 and mm1 with rounding. */
int pavgb (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F E0  /r RMBoth     | Average packed unsigned byte integers from xmm2/m128 and xmm1 with rounding. */
 /* 66 0F E0  /r RMBoth     | Average packed unsigned byte integers from xmm2/m128 and xmm1 with rounding. */
int pavgb (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F E0  /r RMBoth     | Average packed unsigned byte integers from xmm2/m128 and xmm1 with rounding. */
 /* 66 0F E0  /r RMBoth     | Average packed unsigned byte integers from xmm2/m128 and xmm1 with rounding. */
int pavgb (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F E3  /r RMBoth     | Average packed unsigned word integers from mm2/m64 and mm1 with rounding. */
 /*  0F E3  /r RMBoth     | Average packed unsigned word integers from mm2/m64 and mm1 with rounding. */
int pavgw (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F E3  /r RMBoth     | Average packed unsigned word integers from mm2/m64 and mm1 with rounding. */
 /*  0F E3  /r RMBoth     | Average packed unsigned word integers from mm2/m64 and mm1 with rounding. */
int pavgw (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F E3  /r RMBoth     | Average packed unsigned word integers from xmm2/m128 and xmm1 with rounding. */
 /* 66 0F E3  /r RMBoth     | Average packed unsigned word integers from xmm2/m128 and xmm1 with rounding. */
int pavgw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F E3  /r RMBoth     | Average packed unsigned word integers from xmm2/m128 and xmm1 with rounding. */
 /* 66 0F E3  /r RMBoth     | Average packed unsigned word integers from xmm2/m128 and xmm1 with rounding. */
int pavgw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 10  /r RMBoth     | Select byte values from xmm1 and xmm2/m128 from mask specified in the high bit of each byte in XMM0 and store the values into xmm1. */
 /* 66 0F 38 10  /r RMBoth     | Select byte values from xmm1 and xmm2/m128 from mask specified in the high bit of each byte in XMM0 and store the values into xmm1. */
int pblendvb (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm),
              const RegXMM0 (&   unused));
/* 66 0F 38 10  /r RMBoth     | Select byte values from xmm1 and xmm2/m128 from mask specified in the high bit of each byte in XMM0 and store the values into xmm1. */
 /* 66 0F 38 10  /r RMBoth     | Select byte values from xmm1 and xmm2/m128 from mask specified in the high bit of each byte in XMM0 and store the values into xmm1. */
int pblendvb (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm),
              const RegXMM0 (&   unused));
/* 66 0F 3A 0E  /r RMBoth  ib   | Select words from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
 /* 66 0F 3A 0E  /r RMBoth  ib   | Select words from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
int pblendw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0E  /r RMBoth  ib   | Select words from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
 /* 66 0F 3A 0E  /r RMBoth  ib   | Select words from xmm1 and xmm2/m128 from mask specified in imm8 and store the values into xmm1. */
int pblendw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 44  /r RMBoth  17   | alias for PCLMULQDQ */
 /* 66 0F 3A 44  /r RMBoth  17   | alias for PCLMULQDQ */
int pclmulhqhdq (code_ptr p,
                 const XmmReg (&   modrm_reg),
                 const XmmReg (&   modrm_rm));
/* 66 0F 3A 44  /r RMBoth  17   | alias for PCLMULQDQ */
 /* 66 0F 3A 44  /r RMBoth  17   | alias for PCLMULQDQ */
int pclmulhqhdq (code_ptr p,
                 const XmmReg (&   modrm_reg),
                 const XmmWordPtr (&   modrm_rm));
/* 66 0F 3A 44  /r RMBoth  1   | alias for PCLMULQDQ */
 /* 66 0F 3A 44  /r RMBoth  1   | alias for PCLMULQDQ */
int pclmulhqlqdq (code_ptr p,
                  const XmmReg (&   modrm_reg),
                  const XmmReg (&   modrm_rm));
/* 66 0F 3A 44  /r RMBoth  1   | alias for PCLMULQDQ */
 /* 66 0F 3A 44  /r RMBoth  1   | alias for PCLMULQDQ */
int pclmulhqlqdq (code_ptr p,
                  const XmmReg (&   modrm_reg),
                  const XmmWordPtr (&   modrm_rm));
/* 66 0F 3A 44  /r RMBoth  16   | alias for PCLMULQDQ */
 /* 66 0F 3A 44  /r RMBoth  16   | alias for PCLMULQDQ */
int pclmullqhdq (code_ptr p,
                 const XmmReg (&   modrm_reg),
                 const XmmReg (&   modrm_rm));
/* 66 0F 3A 44  /r RMBoth  16   | alias for PCLMULQDQ */
 /* 66 0F 3A 44  /r RMBoth  16   | alias for PCLMULQDQ */
int pclmullqhdq (code_ptr p,
                 const XmmReg (&   modrm_reg),
                 const XmmWordPtr (&   modrm_rm));
/* 66 0F 3A 44  /r RMBoth  0   | alias for PCLMULQDQ */
 /* 66 0F 3A 44  /r RMBoth  0   | alias for PCLMULQDQ */
int pclmullqlqdq (code_ptr p,
                  const XmmReg (&   modrm_reg),
                  const XmmReg (&   modrm_rm));
/* 66 0F 3A 44  /r RMBoth  0   | alias for PCLMULQDQ */
 /* 66 0F 3A 44  /r RMBoth  0   | alias for PCLMULQDQ */
int pclmullqlqdq (code_ptr p,
                  const XmmReg (&   modrm_reg),
                  const XmmWordPtr (&   modrm_rm));
/* 66 0F 3A 44  /r RMBoth  ib   | Carry-less multiplication of one quadword of xmm1 by one quadword of xmm2/m128, stores the 128-bit result in xmm1. The immediate is used to deter- mine which quadwords of xmm1 and xmm2/m128 should be used. */
 /* 66 0F 3A 44  /r RMBoth  ib   | Carry-less multiplication of one quadword of xmm1 by one quadword of xmm2/m128, stores the 128-bit result in xmm1. The immediate is used to deter- mine which quadwords of xmm1 and xmm2/m128 should be used. */
int pclmulqdq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm),
               imm8_t imm);
/* 66 0F 3A 44  /r RMBoth  ib   | Carry-less multiplication of one quadword of xmm1 by one quadword of xmm2/m128, stores the 128-bit result in xmm1. The immediate is used to deter- mine which quadwords of xmm1 and xmm2/m128 should be used. */
 /* 66 0F 3A 44  /r RMBoth  ib   | Carry-less multiplication of one quadword of xmm1 by one quadword of xmm2/m128, stores the 128-bit result in xmm1. The immediate is used to deter- mine which quadwords of xmm1 and xmm2/m128 should be used. */
int pclmulqdq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm),
               imm8_t imm);
/*  0F 74  /r RMBoth     | Compare packed bytes in mm/m64 and mm for equality. */
 /*  0F 74  /r RMBoth     | Compare packed bytes in mm/m64 and mm for equality. */
int pcmpeqb (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F 74  /r RMBoth     | Compare packed bytes in mm/m64 and mm for equality. */
 /*  0F 74  /r RMBoth     | Compare packed bytes in mm/m64 and mm for equality. */
int pcmpeqb (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F 74  /r RMBoth     | Compare packed bytes in xmm2/m128 and xmm1 for equality. */
 /* 66 0F 74  /r RMBoth     | Compare packed bytes in xmm2/m128 and xmm1 for equality. */
int pcmpeqb (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 74  /r RMBoth     | Compare packed bytes in xmm2/m128 and xmm1 for equality. */
 /* 66 0F 74  /r RMBoth     | Compare packed bytes in xmm2/m128 and xmm1 for equality. */
int pcmpeqb (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F 76  /r RMBoth     | Compare packed doublewords in mm/m64 and mm for equality. */
 /*  0F 76  /r RMBoth     | Compare packed doublewords in mm/m64 and mm for equality. */
int pcmpeqd (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F 76  /r RMBoth     | Compare packed doublewords in mm/m64 and mm for equality. */
 /*  0F 76  /r RMBoth     | Compare packed doublewords in mm/m64 and mm for equality. */
int pcmpeqd (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F 76  /r RMBoth     | Compare packed doublewords in xmm2/m128 and xmm1 for equality. */
 /* 66 0F 76  /r RMBoth     | Compare packed doublewords in xmm2/m128 and xmm1 for equality. */
int pcmpeqd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 76  /r RMBoth     | Compare packed doublewords in xmm2/m128 and xmm1 for equality. */
 /* 66 0F 76  /r RMBoth     | Compare packed doublewords in xmm2/m128 and xmm1 for equality. */
int pcmpeqd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 29  /r RMBoth     | Compare packed qwords in xmm2/m128 and xmm1 for equality. */
 /* 66 0F 38 29  /r RMBoth     | Compare packed qwords in xmm2/m128 and xmm1 for equality. */
int pcmpeqq (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 38 29  /r RMBoth     | Compare packed qwords in xmm2/m128 and xmm1 for equality. */
 /* 66 0F 38 29  /r RMBoth     | Compare packed qwords in xmm2/m128 and xmm1 for equality. */
int pcmpeqq (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F 75  /r RMBoth     | Compare packed words in mm/m64 and mm for equality. */
 /*  0F 75  /r RMBoth     | Compare packed words in mm/m64 and mm for equality. */
int pcmpeqw (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F 75  /r RMBoth     | Compare packed words in mm/m64 and mm for equality. */
 /*  0F 75  /r RMBoth     | Compare packed words in mm/m64 and mm for equality. */
int pcmpeqw (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F 75  /r RMBoth     | Compare packed words in xmm2/m128 and xmm1 for equality. */
 /* 66 0F 75  /r RMBoth     | Compare packed words in xmm2/m128 and xmm1 for equality. */
int pcmpeqw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 75  /r RMBoth     | Compare packed words in xmm2/m128 and xmm1 for equality. */
 /* 66 0F 75  /r RMBoth     | Compare packed words in xmm2/m128 and xmm1 for equality. */
int pcmpeqw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* 66 0F 3A 61  /r RMBoth  ib   | Perform a packed comparison of string data with explicit lengths, generating an index, and storing the result in ECX. */
 /* 66 0F 3A 61  /r RMBoth  ib   | Perform a packed comparison of string data with explicit lengths, generating an index, and storing the result in ECX. */
int pcmpestri (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm),
               imm8_t imm);
/* 66 0F 3A 61  /r RMBoth  ib   | Perform a packed comparison of string data with explicit lengths, generating an index, and storing the result in ECX. */
 /* 66 0F 3A 61  /r RMBoth  ib   | Perform a packed comparison of string data with explicit lengths, generating an index, and storing the result in ECX. */
int pcmpestri (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm),
               imm8_t imm);
/* 66 0F 3A 60  /r RMBoth  ib   | Perform a packed comparison of string data with explicit lengths, generating a mask, and storing the result in XMM0 */
 /* 66 0F 3A 60  /r RMBoth  ib   | Perform a packed comparison of string data with explicit lengths, generating a mask, and storing the result in XMM0 */
int pcmpestrm (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm),
               imm8_t imm);
/* 66 0F 3A 60  /r RMBoth  ib   | Perform a packed comparison of string data with explicit lengths, generating a mask, and storing the result in XMM0 */
 /* 66 0F 3A 60  /r RMBoth  ib   | Perform a packed comparison of string data with explicit lengths, generating a mask, and storing the result in XMM0 */
int pcmpestrm (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm),
               imm8_t imm);
/*  0F 64  /r RMBoth     | Compare packed signed byte integers in mm and mm/m64 for greater than. */
 /*  0F 64  /r RMBoth     | Compare packed signed byte integers in mm and mm/m64 for greater than. */
int pcmpgtb (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F 64  /r RMBoth     | Compare packed signed byte integers in mm and mm/m64 for greater than. */
 /*  0F 64  /r RMBoth     | Compare packed signed byte integers in mm and mm/m64 for greater than. */
int pcmpgtb (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F 64  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 for greater than. */
 /* 66 0F 64  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 for greater than. */
int pcmpgtb (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 64  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 for greater than. */
 /* 66 0F 64  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 for greater than. */
int pcmpgtb (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F 66  /r RMBoth     | Compare packed signed doubleword integers in mm and mm/m64 for greater than. */
 /*  0F 66  /r RMBoth     | Compare packed signed doubleword integers in mm and mm/m64 for greater than. */
int pcmpgtd (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F 66  /r RMBoth     | Compare packed signed doubleword integers in mm and mm/m64 for greater than. */
 /*  0F 66  /r RMBoth     | Compare packed signed doubleword integers in mm and mm/m64 for greater than. */
int pcmpgtd (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F 66  /r RMBoth     | Compare packed signed doubleword integers in xmm1 and xmm2/m128 for greater than. */
 /* 66 0F 66  /r RMBoth     | Compare packed signed doubleword integers in xmm1 and xmm2/m128 for greater than. */
int pcmpgtd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 66  /r RMBoth     | Compare packed signed doubleword integers in xmm1 and xmm2/m128 for greater than. */
 /* 66 0F 66  /r RMBoth     | Compare packed signed doubleword integers in xmm1 and xmm2/m128 for greater than. */
int pcmpgtd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 37  /r RMBoth     | Compare packed qwords in xmm2/m128 and xmm1 for greater than. */
 /* 66 0F 38 37  /r RMBoth     | Compare packed qwords in xmm2/m128 and xmm1 for greater than. */
int pcmpgtq (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 38 37  /r RMBoth     | Compare packed qwords in xmm2/m128 and xmm1 for greater than. */
 /* 66 0F 38 37  /r RMBoth     | Compare packed qwords in xmm2/m128 and xmm1 for greater than. */
int pcmpgtq (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F 65  /r RMBoth     | Compare packed signed word integers in mm and mm/m64 for greater than. */
 /*  0F 65  /r RMBoth     | Compare packed signed word integers in mm and mm/m64 for greater than. */
int pcmpgtw (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F 65  /r RMBoth     | Compare packed signed word integers in mm and mm/m64 for greater than. */
 /*  0F 65  /r RMBoth     | Compare packed signed word integers in mm and mm/m64 for greater than. */
int pcmpgtw (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F 65  /r RMBoth     | Compare packed signed word integers in xmm1 and xmm2/m128 for greater than. */
 /* 66 0F 65  /r RMBoth     | Compare packed signed word integers in xmm1 and xmm2/m128 for greater than. */
int pcmpgtw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 65  /r RMBoth     | Compare packed signed word integers in xmm1 and xmm2/m128 for greater than. */
 /* 66 0F 65  /r RMBoth     | Compare packed signed word integers in xmm1 and xmm2/m128 for greater than. */
int pcmpgtw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* 66 0F 3A 63  /r RMBoth  ib   | Perform a packed comparison of string data with implicit lengths, generating an index, and storing the result in ECX. */
 /* 66 0F 3A 63  /r RMBoth  ib   | Perform a packed comparison of string data with implicit lengths, generating an index, and storing the result in ECX. */
int pcmpistri (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm),
               imm8_t imm);
/* 66 0F 3A 63  /r RMBoth  ib   | Perform a packed comparison of string data with implicit lengths, generating an index, and storing the result in ECX. */
 /* 66 0F 3A 63  /r RMBoth  ib   | Perform a packed comparison of string data with implicit lengths, generating an index, and storing the result in ECX. */
int pcmpistri (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm),
               imm8_t imm);
/* 66 0F 3A 62  /r RMBoth  ib   | Perform a packed comparison of string data with implicit lengths, generating a mask, and storing the result in XMM0. */
 /* 66 0F 3A 62  /r RMBoth  ib   | Perform a packed comparison of string data with implicit lengths, generating a mask, and storing the result in XMM0. */
int pcmpistrm (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm),
               imm8_t imm);
/* 66 0F 3A 62  /r RMBoth  ib   | Perform a packed comparison of string data with implicit lengths, generating a mask, and storing the result in XMM0. */
 /* 66 0F 3A 62  /r RMBoth  ib   | Perform a packed comparison of string data with implicit lengths, generating a mask, and storing the result in XMM0. */
int pcmpistrm (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm),
               imm8_t imm);
/* 66 0F 3A 14  /r RMBoth  ib   | Extract a byte integer value from xmm2 at the source byte offset specified by imm8 into rreg or m8. The upper bits of r32 or r64 are zeroed. */
 /* 66 0F 3A 14  /r RMBoth  ib   | Extract a byte integer value from xmm2 at the source byte offset specified by imm8 into rreg or m8. The upper bits of r32 or r64 are zeroed. */
int pextrb (code_ptr p,
            const DwordReg (&   modrm_rm),
            const XmmReg (&   modrm_reg),
            imm8_t imm);
/* 66 0F 3A 14  /r RMBoth  ib   | Extract a byte integer value from xmm2 at the source byte offset specified by imm8 into rreg or m8. The upper bits of r32 or r64 are zeroed. */
 /* 66 0F 3A 14  /r RMBoth  ib   | Extract a byte integer value from xmm2 at the source byte offset specified by imm8 into rreg or m8. The upper bits of r32 or r64 are zeroed. */
int pextrb (code_ptr p,
            const BytePtr (&   modrm_rm),
            const XmmReg (&   modrm_reg),
            imm8_t imm);
/* 66 0F 3A 16  /r RMBoth  ib   | Extract a dword integer value from xmm2 at the source dword offset specified by imm8 into r/m32. */
 /* 66 0F 3A 16  /r RMBoth  ib   | Extract a dword integer value from xmm2 at the source dword offset specified by imm8 into r/m32. */
int pextrd (code_ptr p,
            const DwordReg (&   modrm_rm),
            const XmmReg (&   modrm_reg),
            imm8_t imm);
/* 66 0F 3A 16  /r RMBoth  ib   | Extract a dword integer value from xmm2 at the source dword offset specified by imm8 into r/m32. */
 /* 66 0F 3A 16  /r RMBoth  ib   | Extract a dword integer value from xmm2 at the source dword offset specified by imm8 into r/m32. */
int pextrd (code_ptr p,
            const DwordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg),
            imm8_t imm);
/* 66 0F C5  /r RMRegOnly  ib   | Extract the word specified by imm8 from xmm and move it to reg, bits 15-0. The upper bits of r32 or r64 is zeroed. */
 /* 66 0F C5  /r RMRegOnly  ib   | Extract the word specified by imm8 from xmm and move it to reg, bits 15-0. The upper bits of r32 or r64 is zeroed. */
int pextrw (code_ptr p,
            const DwordReg (&   modrm_reg),
            const XmmReg (&   modrm_rm),
            imm8_t imm);
/* 66 0F 3A 15  /r RMBoth  ib   | Extract the word specified by imm8 from xmm and copy it to lowest 16 bits of reg or m16. Zero-extend the result in the destination, r32 or r64. */
 /* 66 0F 3A 15  /r RMBoth  ib   | Extract the word specified by imm8 from xmm and copy it to lowest 16 bits of reg or m16. Zero-extend the result in the destination, r32 or r64. */
int pextrw (code_ptr p,
            const WordPtr (&   modrm_rm),
            const XmmReg (&   modrm_reg),
            imm8_t imm);
/*  0F C5  /r RMRegOnly  ib   | Extract the word specified by imm8 from mm and move it to reg, bits 15-0. The upper bits of r32 or r64 is zeroed. */
 /*  0F C5  /r RMRegOnly  ib   | Extract the word specified by imm8 from mm and move it to reg, bits 15-0. The upper bits of r32 or r64 is zeroed. */
int pextrw (code_ptr p,
            const DwordReg (&   modrm_reg),
            const MmReg (&   modrm_rm),
            imm8_t imm);
/*  0F 38 02  /r RMBoth     | Add 32-bit signed integers horizontally, pack to MM1. */
 /*  0F 38 02  /r RMBoth     | Add 32-bit signed integers horizontally, pack to MM1. */
int phaddd (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F 38 02  /r RMBoth     | Add 32-bit signed integers horizontally, pack to MM1. */
 /*  0F 38 02  /r RMBoth     | Add 32-bit signed integers horizontally, pack to MM1. */
int phaddd (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 38 02  /r RMBoth     | Add 32-bit signed integers horizontally, pack to XMM1. */
 /* 66 0F 38 02  /r RMBoth     | Add 32-bit signed integers horizontally, pack to XMM1. */
int phaddd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 02  /r RMBoth     | Add 32-bit signed integers horizontally, pack to XMM1. */
 /* 66 0F 38 02  /r RMBoth     | Add 32-bit signed integers horizontally, pack to XMM1. */
int phaddd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 38 03  /r RMBoth     | Add 16-bit signed integers horizontally, pack saturated integers to MM1. */
 /*  0F 38 03  /r RMBoth     | Add 16-bit signed integers horizontally, pack saturated integers to MM1. */
int phaddsw (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F 38 03  /r RMBoth     | Add 16-bit signed integers horizontally, pack saturated integers to MM1. */
 /*  0F 38 03  /r RMBoth     | Add 16-bit signed integers horizontally, pack saturated integers to MM1. */
int phaddsw (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F 38 03  /r RMBoth     | Add 16-bit signed integers horizontally, pack saturated integers to XMM1. */
 /* 66 0F 38 03  /r RMBoth     | Add 16-bit signed integers horizontally, pack saturated integers to XMM1. */
int phaddsw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 38 03  /r RMBoth     | Add 16-bit signed integers horizontally, pack saturated integers to XMM1. */
 /* 66 0F 38 03  /r RMBoth     | Add 16-bit signed integers horizontally, pack saturated integers to XMM1. */
int phaddsw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F 38 01  /r RMBoth     | Add 16-bit signed integers horizontally, pack to MM1. */
 /*  0F 38 01  /r RMBoth     | Add 16-bit signed integers horizontally, pack to MM1. */
int phaddw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F 38 01  /r RMBoth     | Add 16-bit signed integers horizontally, pack to MM1. */
 /*  0F 38 01  /r RMBoth     | Add 16-bit signed integers horizontally, pack to MM1. */
int phaddw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 38 01  /r RMBoth     | Add 16-bit signed integers horizontally, pack to XMM1. */
 /* 66 0F 38 01  /r RMBoth     | Add 16-bit signed integers horizontally, pack to XMM1. */
int phaddw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 01  /r RMBoth     | Add 16-bit signed integers horizontally, pack to XMM1. */
 /* 66 0F 38 01  /r RMBoth     | Add 16-bit signed integers horizontally, pack to XMM1. */
int phaddw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 41  /r RMBoth     | Find the minimum unsigned word in xmm2/m128 and place its value in the low word of xmm1 and its index in the second-lowest word of xmm1. */
 /* 66 0F 38 41  /r RMBoth     | Find the minimum unsigned word in xmm2/m128 and place its value in the low word of xmm1 and its index in the second-lowest word of xmm1. */
int phminposuw (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/* 66 0F 38 41  /r RMBoth     | Find the minimum unsigned word in xmm2/m128 and place its value in the low word of xmm1 and its index in the second-lowest word of xmm1. */
 /* 66 0F 38 41  /r RMBoth     | Find the minimum unsigned word in xmm2/m128 and place its value in the low word of xmm1 and its index in the second-lowest word of xmm1. */
int phminposuw (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmWordPtr (&   modrm_rm));
/*  0F 38 06  /r RMBoth     | Subtract 32-bit signed integers horizontally, pack to MM1. */
 /*  0F 38 06  /r RMBoth     | Subtract 32-bit signed integers horizontally, pack to MM1. */
int phsubd (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F 38 06  /r RMBoth     | Subtract 32-bit signed integers horizontally, pack to MM1. */
 /*  0F 38 06  /r RMBoth     | Subtract 32-bit signed integers horizontally, pack to MM1. */
int phsubd (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 38 06  /r RMBoth     | Subtract 32-bit signed integers horizontally, pack to XMM1. */
 /* 66 0F 38 06  /r RMBoth     | Subtract 32-bit signed integers horizontally, pack to XMM1. */
int phsubd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 06  /r RMBoth     | Subtract 32-bit signed integers horizontally, pack to XMM1. */
 /* 66 0F 38 06  /r RMBoth     | Subtract 32-bit signed integers horizontally, pack to XMM1. */
int phsubd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 38 07  /r RMBoth     | Subtract 16-bit signed integer horizontally, pack saturated integers to MM1. */
 /*  0F 38 07  /r RMBoth     | Subtract 16-bit signed integer horizontally, pack saturated integers to MM1. */
int phsubsw (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F 38 07  /r RMBoth     | Subtract 16-bit signed integer horizontally, pack saturated integers to MM1. */
 /*  0F 38 07  /r RMBoth     | Subtract 16-bit signed integer horizontally, pack saturated integers to MM1. */
int phsubsw (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F 38 07  /r RMBoth     | Subtract 16-bit signed integer horizontally, pack saturated integers to XMM1 */
 /* 66 0F 38 07  /r RMBoth     | Subtract 16-bit signed integer horizontally, pack saturated integers to XMM1 */
int phsubsw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 38 07  /r RMBoth     | Subtract 16-bit signed integer horizontally, pack saturated integers to XMM1 */
 /* 66 0F 38 07  /r RMBoth     | Subtract 16-bit signed integer horizontally, pack saturated integers to XMM1 */
int phsubsw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F 38 05  /r RMBoth     | Subtract 16-bit signed integers horizontally, pack to MM1. */
 /*  0F 38 05  /r RMBoth     | Subtract 16-bit signed integers horizontally, pack to MM1. */
int phsubw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F 38 05  /r RMBoth     | Subtract 16-bit signed integers horizontally, pack to MM1. */
 /*  0F 38 05  /r RMBoth     | Subtract 16-bit signed integers horizontally, pack to MM1. */
int phsubw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 38 05  /r RMBoth     | Subtract 16-bit signed integers horizontally, pack to XMM1. */
 /* 66 0F 38 05  /r RMBoth     | Subtract 16-bit signed integers horizontally, pack to XMM1. */
int phsubw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 05  /r RMBoth     | Subtract 16-bit signed integers horizontally, pack to XMM1. */
 /* 66 0F 38 05  /r RMBoth     | Subtract 16-bit signed integers horizontally, pack to XMM1. */
int phsubw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 3A 20  /r RMBoth  ib   | Insert a byte integer value from r32/m8 into xmm1 at the destination element in xmm1 specified by imm8. */
 /* 66 0F 3A 20  /r RMBoth  ib   | Insert a byte integer value from r32/m8 into xmm1 at the destination element in xmm1 specified by imm8. */
int pinsrb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const DwordReg (&   modrm_rm),
            imm8_t imm);
/* 66 0F 3A 20  /r RMBoth  ib   | Insert a byte integer value from r32/m8 into xmm1 at the destination element in xmm1 specified by imm8. */
 /* 66 0F 3A 20  /r RMBoth  ib   | Insert a byte integer value from r32/m8 into xmm1 at the destination element in xmm1 specified by imm8. */
int pinsrb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const BytePtr (&   modrm_rm),
            imm8_t imm);
/* 66 0F 3A 22  /r RMBoth  ib   | Insert a dword integer value from r/m32 into the xmm1 at the destination element specified by imm8. */
 /* 66 0F 3A 22  /r RMBoth  ib   | Insert a dword integer value from r/m32 into the xmm1 at the destination element specified by imm8. */
int pinsrd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const DwordReg (&   modrm_rm),
            imm8_t imm);
/* 66 0F 3A 22  /r RMBoth  ib   | Insert a dword integer value from r/m32 into the xmm1 at the destination element specified by imm8. */
 /* 66 0F 3A 22  /r RMBoth  ib   | Insert a dword integer value from r/m32 into the xmm1 at the destination element specified by imm8. */
int pinsrd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm),
            imm8_t imm);
/*  0F C4  /r RMBoth  ib   | Insert the low word from r32 or from m16 into mm at the word position specified by imm8 */
 /*  0F C4  /r RMBoth  ib   | Insert the low word from r32 or from m16 into mm at the word position specified by imm8 */
int pinsrw (code_ptr p,
            const MmReg (&   modrm_reg),
            const DwordReg (&   modrm_rm),
            imm8_t imm);
/*  0F C4  /r RMBoth  ib   | Insert the low word from r32 or from m16 into mm at the word position specified by imm8 */
 /*  0F C4  /r RMBoth  ib   | Insert the low word from r32 or from m16 into mm at the word position specified by imm8 */
int pinsrw (code_ptr p,
            const MmReg (&   modrm_reg),
            const WordPtr (&   modrm_rm),
            imm8_t imm);
/* 66 0F C4  /r RMBoth  ib   | Move the low word of r32 or from m16 into xmm at the word position specified by imm8. */
 /* 66 0F C4  /r RMBoth  ib   | Move the low word of r32 or from m16 into xmm at the word position specified by imm8. */
int pinsrw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const DwordReg (&   modrm_rm),
            imm8_t imm);
/* 66 0F C4  /r RMBoth  ib   | Move the low word of r32 or from m16 into xmm at the word position specified by imm8. */
 /* 66 0F C4  /r RMBoth  ib   | Move the low word of r32 or from m16 into xmm at the word position specified by imm8. */
int pinsrw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const WordPtr (&   modrm_rm),
            imm8_t imm);
/*  0F 38 04  /r RMBoth     | Multiply signed and unsigned bytes, add horizontal pair of signed words, pack saturated signed-words to MM1. */
 /*  0F 38 04  /r RMBoth     | Multiply signed and unsigned bytes, add horizontal pair of signed words, pack saturated signed-words to MM1. */
int pmaddubsw (code_ptr p,
               const MmReg (&   modrm_reg),
               const MmReg (&   modrm_rm));
/*  0F 38 04  /r RMBoth     | Multiply signed and unsigned bytes, add horizontal pair of signed words, pack saturated signed-words to MM1. */
 /*  0F 38 04  /r RMBoth     | Multiply signed and unsigned bytes, add horizontal pair of signed words, pack saturated signed-words to MM1. */
int pmaddubsw (code_ptr p,
               const MmReg (&   modrm_reg),
               const QwordPtr (&   modrm_rm));
/* 66 0F 38 04  /r RMBoth     | Multiply signed and unsigned bytes, add horizontal pair of signed words, pack saturated signed-words to XMM1. */
 /* 66 0F 38 04  /r RMBoth     | Multiply signed and unsigned bytes, add horizontal pair of signed words, pack saturated signed-words to XMM1. */
int pmaddubsw (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F 38 04  /r RMBoth     | Multiply signed and unsigned bytes, add horizontal pair of signed words, pack saturated signed-words to XMM1. */
 /* 66 0F 38 04  /r RMBoth     | Multiply signed and unsigned bytes, add horizontal pair of signed words, pack saturated signed-words to XMM1. */
int pmaddubsw (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/*  0F F5  /r RMBoth     | Multiply the packed words in mm by the packed words in mm/m64, add adjacent doubleword results, and store in mm. */
 /*  0F F5  /r RMBoth     | Multiply the packed words in mm by the packed words in mm/m64, add adjacent doubleword results, and store in mm. */
int pmaddwd (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F F5  /r RMBoth     | Multiply the packed words in mm by the packed words in mm/m64, add adjacent doubleword results, and store in mm. */
 /*  0F F5  /r RMBoth     | Multiply the packed words in mm by the packed words in mm/m64, add adjacent doubleword results, and store in mm. */
int pmaddwd (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F F5  /r RMBoth     | Multiply the packed word integers in xmm1 by the packed word integers in xmm2/m128, add adjacent doubleword results, and store in xmm1. */
 /* 66 0F F5  /r RMBoth     | Multiply the packed word integers in xmm1 by the packed word integers in xmm2/m128, add adjacent doubleword results, and store in xmm1. */
int pmaddwd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F F5  /r RMBoth     | Multiply the packed word integers in xmm1 by the packed word integers in xmm2/m128, add adjacent doubleword results, and store in xmm1. */
 /* 66 0F F5  /r RMBoth     | Multiply the packed word integers in xmm1 by the packed word integers in xmm2/m128, add adjacent doubleword results, and store in xmm1. */
int pmaddwd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 3C  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
 /* 66 0F 38 3C  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
int pmaxsb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 3C  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
 /* 66 0F 38 3C  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
int pmaxsb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 3D  /r RMBoth     | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
 /* 66 0F 38 3D  /r RMBoth     | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
int pmaxsd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 3D  /r RMBoth     | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
 /* 66 0F 38 3D  /r RMBoth     | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
int pmaxsd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F EE  /r RMBoth     | Compare signed word integers in mm2/m64 and mm1 and return maximum values. */
 /*  0F EE  /r RMBoth     | Compare signed word integers in mm2/m64 and mm1 and return maximum values. */
int pmaxsw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F EE  /r RMBoth     | Compare signed word integers in mm2/m64 and mm1 and return maximum values. */
 /*  0F EE  /r RMBoth     | Compare signed word integers in mm2/m64 and mm1 and return maximum values. */
int pmaxsw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F EE  /r RMBoth     | Compare signed word integers in xmm2/m128 and xmm1 and return maximum values. */
 /* 66 0F EE  /r RMBoth     | Compare signed word integers in xmm2/m128 and xmm1 and return maximum values. */
int pmaxsw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F EE  /r RMBoth     | Compare signed word integers in xmm2/m128 and xmm1 and return maximum values. */
 /* 66 0F EE  /r RMBoth     | Compare signed word integers in xmm2/m128 and xmm1 and return maximum values. */
int pmaxsw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F DE  /r RMBoth     | Compare unsigned byte integers in mm2/m64 and mm1 and returns maximum values. */
 /*  0F DE  /r RMBoth     | Compare unsigned byte integers in mm2/m64 and mm1 and returns maximum values. */
int pmaxub (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F DE  /r RMBoth     | Compare unsigned byte integers in mm2/m64 and mm1 and returns maximum values. */
 /*  0F DE  /r RMBoth     | Compare unsigned byte integers in mm2/m64 and mm1 and returns maximum values. */
int pmaxub (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F DE  /r RMBoth     | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns maximum values. */
 /* 66 0F DE  /r RMBoth     | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns maximum values. */
int pmaxub (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F DE  /r RMBoth     | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns maximum values. */
 /* 66 0F DE  /r RMBoth     | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns maximum values. */
int pmaxub (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 3F  /r RMBoth     | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
 /* 66 0F 38 3F  /r RMBoth     | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
int pmaxud (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 3F  /r RMBoth     | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
 /* 66 0F 38 3F  /r RMBoth     | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
int pmaxud (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 3E  /r RMBoth     | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
 /* 66 0F 38 3E  /r RMBoth     | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
int pmaxuw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 3E  /r RMBoth     | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
 /* 66 0F 38 3E  /r RMBoth     | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed maximum values in xmm1. */
int pmaxuw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 38  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
 /* 66 0F 38 38  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
int pminsb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 38  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
 /* 66 0F 38 38  /r RMBoth     | Compare packed signed byte integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
int pminsb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 39  /r RMBoth     | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
 /* 66 0F 38 39  /r RMBoth     | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
int pminsd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 39  /r RMBoth     | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
 /* 66 0F 38 39  /r RMBoth     | Compare packed signed dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
int pminsd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F EA  /r RMBoth     | Compare signed word integers in mm2/m64 and mm1 and return minimum values. */
 /*  0F EA  /r RMBoth     | Compare signed word integers in mm2/m64 and mm1 and return minimum values. */
int pminsw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F EA  /r RMBoth     | Compare signed word integers in mm2/m64 and mm1 and return minimum values. */
 /*  0F EA  /r RMBoth     | Compare signed word integers in mm2/m64 and mm1 and return minimum values. */
int pminsw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F EA  /r RMBoth     | Compare signed word integers in xmm2/m128 and xmm1 and return minimum values. */
 /* 66 0F EA  /r RMBoth     | Compare signed word integers in xmm2/m128 and xmm1 and return minimum values. */
int pminsw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F EA  /r RMBoth     | Compare signed word integers in xmm2/m128 and xmm1 and return minimum values. */
 /* 66 0F EA  /r RMBoth     | Compare signed word integers in xmm2/m128 and xmm1 and return minimum values. */
int pminsw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F DA  /r RMBoth     | Compare unsigned byte integers in mm2/m64 and mm1 and returns minimum values. */
 /*  0F DA  /r RMBoth     | Compare unsigned byte integers in mm2/m64 and mm1 and returns minimum values. */
int pminub (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F DA  /r RMBoth     | Compare unsigned byte integers in mm2/m64 and mm1 and returns minimum values. */
 /*  0F DA  /r RMBoth     | Compare unsigned byte integers in mm2/m64 and mm1 and returns minimum values. */
int pminub (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F DA  /r RMBoth     | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns minimum values. */
 /* 66 0F DA  /r RMBoth     | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns minimum values. */
int pminub (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F DA  /r RMBoth     | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns minimum values. */
 /* 66 0F DA  /r RMBoth     | Compare unsigned byte integers in xmm2/m128 and xmm1 and returns minimum values. */
int pminub (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 3B  /r RMBoth     | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
 /* 66 0F 38 3B  /r RMBoth     | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
int pminud (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 3B  /r RMBoth     | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
 /* 66 0F 38 3B  /r RMBoth     | Compare packed unsigned dword integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
int pminud (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 3A  /r RMBoth     | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
 /* 66 0F 38 3A  /r RMBoth     | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
int pminuw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 3A  /r RMBoth     | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
 /* 66 0F 38 3A  /r RMBoth     | Compare packed unsigned word integers in xmm1 and xmm2/m128 and store packed minimum values in xmm1. */
int pminuw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F D7  /r RMRegOnly     | Move a byte mask of mm to r32. */
 /*  0F D7  /r RMRegOnly     | Move a byte mask of mm to r32. */
int pmovmskb (code_ptr p,
              const DwordReg (&   modrm_reg),
              const MmReg (&   modrm_rm));
/* 66 0F D7  /r RMRegOnly     | Move a byte mask of xmm to reg. The upper bits of r32 or r64 are zeroed */
 /* 66 0F D7  /r RMRegOnly     | Move a byte mask of xmm to reg. The upper bits of r32 or r64 are zeroed */
int pmovmskb (code_ptr p,
              const DwordReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 21  /r RMBoth     | Sign extend 4 packed signed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed signed 32-bit integers in xmm1. */
 /* 66 0F 38 21  /r RMBoth     | Sign extend 4 packed signed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed signed 32-bit integers in xmm1. */
int pmovsxbd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 21  /r RMBoth     | Sign extend 4 packed signed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed signed 32-bit integers in xmm1. */
 /* 66 0F 38 21  /r RMBoth     | Sign extend 4 packed signed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed signed 32-bit integers in xmm1. */
int pmovsxbd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F 38 22  /r RMBoth     | Sign extend 2 packed signed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed signed 64-bit integers in xmm1. */
 /* 66 0F 38 22  /r RMBoth     | Sign extend 2 packed signed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed signed 64-bit integers in xmm1. */
int pmovsxbq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 22  /r RMBoth     | Sign extend 2 packed signed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed signed 64-bit integers in xmm1. */
 /* 66 0F 38 22  /r RMBoth     | Sign extend 2 packed signed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed signed 64-bit integers in xmm1. */
int pmovsxbq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const WordPtr (&   modrm_rm));
/* 66 0F 38 20  /r RMBoth     | Sign extend 8 packed signed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed signed 16-bit integers in xmm1. */
 /* 66 0F 38 20  /r RMBoth     | Sign extend 8 packed signed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed signed 16-bit integers in xmm1. */
int pmovsxbw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 20  /r RMBoth     | Sign extend 8 packed signed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed signed 16-bit integers in xmm1. */
 /* 66 0F 38 20  /r RMBoth     | Sign extend 8 packed signed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed signed 16-bit integers in xmm1. */
int pmovsxbw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 38 25  /r RMBoth     | Sign extend 2 packed signed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed signed 64-bit integers in xmm1. */
 /* 66 0F 38 25  /r RMBoth     | Sign extend 2 packed signed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed signed 64-bit integers in xmm1. */
int pmovsxdq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 25  /r RMBoth     | Sign extend 2 packed signed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed signed 64-bit integers in xmm1. */
 /* 66 0F 38 25  /r RMBoth     | Sign extend 2 packed signed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed signed 64-bit integers in xmm1. */
int pmovsxdq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 38 23  /r RMBoth     | Sign extend 4 packed signed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed signed 32-bit integers in xmm1. */
 /* 66 0F 38 23  /r RMBoth     | Sign extend 4 packed signed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed signed 32-bit integers in xmm1. */
int pmovsxwd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 23  /r RMBoth     | Sign extend 4 packed signed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed signed 32-bit integers in xmm1. */
 /* 66 0F 38 23  /r RMBoth     | Sign extend 4 packed signed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed signed 32-bit integers in xmm1. */
int pmovsxwd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 38 24  /r RMBoth     | Sign extend 2 packed signed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed signed 64-bit integers in xmm1. */
 /* 66 0F 38 24  /r RMBoth     | Sign extend 2 packed signed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed signed 64-bit integers in xmm1. */
int pmovsxwq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 24  /r RMBoth     | Sign extend 2 packed signed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed signed 64-bit integers in xmm1. */
 /* 66 0F 38 24  /r RMBoth     | Sign extend 2 packed signed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed signed 64-bit integers in xmm1. */
int pmovsxwq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F 38 31  /r RMBoth     | Zero extend 4 packed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed 32-bit integers in xmm1. */
 /* 66 0F 38 31  /r RMBoth     | Zero extend 4 packed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed 32-bit integers in xmm1. */
int pmovzxbd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 31  /r RMBoth     | Zero extend 4 packed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed 32-bit integers in xmm1. */
 /* 66 0F 38 31  /r RMBoth     | Zero extend 4 packed 8-bit integers in the low 4 bytes of xmm2/m32 to 4 packed 32-bit integers in xmm1. */
int pmovzxbd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F 38 32  /r RMBoth     | Zero extend 2 packed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed 64-bit integers in xmm1. */
 /* 66 0F 38 32  /r RMBoth     | Zero extend 2 packed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed 64-bit integers in xmm1. */
int pmovzxbq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 32  /r RMBoth     | Zero extend 2 packed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed 64-bit integers in xmm1. */
 /* 66 0F 38 32  /r RMBoth     | Zero extend 2 packed 8-bit integers in the low 2 bytes of xmm2/m16 to 2 packed 64-bit integers in xmm1. */
int pmovzxbq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const WordPtr (&   modrm_rm));
/* 66 0F 38 30  /r RMBoth     | Zero extend 8 packed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed 16-bit integers in xmm1. */
 /* 66 0F 38 30  /r RMBoth     | Zero extend 8 packed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed 16-bit integers in xmm1. */
int pmovzxbw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 30  /r RMBoth     | Zero extend 8 packed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed 16-bit integers in xmm1. */
 /* 66 0F 38 30  /r RMBoth     | Zero extend 8 packed 8-bit integers in the low 8 bytes of xmm2/m64 to 8 packed 16-bit integers in xmm1. */
int pmovzxbw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 38 35  /r RMBoth     | Zero extend 2 packed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed 64-bit integers in xmm1. */
 /* 66 0F 38 35  /r RMBoth     | Zero extend 2 packed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed 64-bit integers in xmm1. */
int pmovzxdq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 35  /r RMBoth     | Zero extend 2 packed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed 64-bit integers in xmm1. */
 /* 66 0F 38 35  /r RMBoth     | Zero extend 2 packed 32-bit integers in the low 8 bytes of xmm2/m64 to 2 packed 64-bit integers in xmm1. */
int pmovzxdq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 38 33  /r RMBoth     | Zero extend 4 packed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed 32-bit integers in xmm1. */
 /* 66 0F 38 33  /r RMBoth     | Zero extend 4 packed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed 32-bit integers in xmm1. */
int pmovzxwd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 33  /r RMBoth     | Zero extend 4 packed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed 32-bit integers in xmm1. */
 /* 66 0F 38 33  /r RMBoth     | Zero extend 4 packed 16-bit integers in the low 8 bytes of xmm2/m64 to 4 packed 32-bit integers in xmm1. */
int pmovzxwd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 38 34  /r RMBoth     | Zero extend 2 packed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed 64-bit integers in xmm1. */
 /* 66 0F 38 34  /r RMBoth     | Zero extend 2 packed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed 64-bit integers in xmm1. */
int pmovzxwq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 34  /r RMBoth     | Zero extend 2 packed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed 64-bit integers in xmm1. */
 /* 66 0F 38 34  /r RMBoth     | Zero extend 2 packed 16-bit integers in the low 4 bytes of xmm2/m32 to 2 packed 64-bit integers in xmm1. */
int pmovzxwq (code_ptr p,
              const XmmReg (&   modrm_reg),
              const DwordPtr (&   modrm_rm));
/* 66 0F 38 28  /r RMBoth     | Multiply the packed signed dword integers in xmm1 and xmm2/m128 and store the quadword product in xmm1. */
 /* 66 0F 38 28  /r RMBoth     | Multiply the packed signed dword integers in xmm1 and xmm2/m128 and store the quadword product in xmm1. */
int pmuldq (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 28  /r RMBoth     | Multiply the packed signed dword integers in xmm1 and xmm2/m128 and store the quadword product in xmm1. */
 /* 66 0F 38 28  /r RMBoth     | Multiply the packed signed dword integers in xmm1 and xmm2/m128 and store the quadword product in xmm1. */
int pmuldq (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 38 0B  /r RMBoth     | Multiply 16-bit signed words, scale and round signed doublewords, pack high 16 bits to MM1. */
 /*  0F 38 0B  /r RMBoth     | Multiply 16-bit signed words, scale and round signed doublewords, pack high 16 bits to MM1. */
int pmulhrsw (code_ptr p,
              const MmReg (&   modrm_reg),
              const MmReg (&   modrm_rm));
/*  0F 38 0B  /r RMBoth     | Multiply 16-bit signed words, scale and round signed doublewords, pack high 16 bits to MM1. */
 /*  0F 38 0B  /r RMBoth     | Multiply 16-bit signed words, scale and round signed doublewords, pack high 16 bits to MM1. */
int pmulhrsw (code_ptr p,
              const MmReg (&   modrm_reg),
              const QwordPtr (&   modrm_rm));
/* 66 0F 38 0B  /r RMBoth     | Multiply 16-bit signed words, scale and round signed doublewords, pack high 16 bits to XMM1. */
 /* 66 0F 38 0B  /r RMBoth     | Multiply 16-bit signed words, scale and round signed doublewords, pack high 16 bits to XMM1. */
int pmulhrsw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 38 0B  /r RMBoth     | Multiply 16-bit signed words, scale and round signed doublewords, pack high 16 bits to XMM1. */
 /* 66 0F 38 0B  /r RMBoth     | Multiply 16-bit signed words, scale and round signed doublewords, pack high 16 bits to XMM1. */
int pmulhrsw (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F E4  /r RMBoth     | Multiply the packed unsigned word integers in mm1 register and mm2/m64, and store the high 16 bits of the results in mm1. */
 /*  0F E4  /r RMBoth     | Multiply the packed unsigned word integers in mm1 register and mm2/m64, and store the high 16 bits of the results in mm1. */
int pmulhuw (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F E4  /r RMBoth     | Multiply the packed unsigned word integers in mm1 register and mm2/m64, and store the high 16 bits of the results in mm1. */
 /*  0F E4  /r RMBoth     | Multiply the packed unsigned word integers in mm1 register and mm2/m64, and store the high 16 bits of the results in mm1. */
int pmulhuw (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F E4  /r RMBoth     | Multiply the packed unsigned word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. */
 /* 66 0F E4  /r RMBoth     | Multiply the packed unsigned word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. */
int pmulhuw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F E4  /r RMBoth     | Multiply the packed unsigned word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. */
 /* 66 0F E4  /r RMBoth     | Multiply the packed unsigned word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. */
int pmulhuw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F E5  /r RMBoth     | Multiply the packed signed word integers in mm1 register and mm2/m64, and store the high 16 bits of the results in mm1. */
 /*  0F E5  /r RMBoth     | Multiply the packed signed word integers in mm1 register and mm2/m64, and store the high 16 bits of the results in mm1. */
int pmulhw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F E5  /r RMBoth     | Multiply the packed signed word integers in mm1 register and mm2/m64, and store the high 16 bits of the results in mm1. */
 /*  0F E5  /r RMBoth     | Multiply the packed signed word integers in mm1 register and mm2/m64, and store the high 16 bits of the results in mm1. */
int pmulhw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F E5  /r RMBoth     | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. */
 /* 66 0F E5  /r RMBoth     | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. */
int pmulhw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F E5  /r RMBoth     | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. */
 /* 66 0F E5  /r RMBoth     | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the high 16 bits of the results in xmm1. */
int pmulhw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 40  /r RMBoth     | Multiply the packed dword signed integers in xmm1 and xmm2/m128 and store the low 32 bits of each product in xmm1. */
 /* 66 0F 38 40  /r RMBoth     | Multiply the packed dword signed integers in xmm1 and xmm2/m128 and store the low 32 bits of each product in xmm1. */
int pmulld (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 40  /r RMBoth     | Multiply the packed dword signed integers in xmm1 and xmm2/m128 and store the low 32 bits of each product in xmm1. */
 /* 66 0F 38 40  /r RMBoth     | Multiply the packed dword signed integers in xmm1 and xmm2/m128 and store the low 32 bits of each product in xmm1. */
int pmulld (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F D5  /r RMBoth     | Multiply the packed signed word integers in mm1 register and mm2/m64, and store the low 16 bits of the results in mm1. */
 /*  0F D5  /r RMBoth     | Multiply the packed signed word integers in mm1 register and mm2/m64, and store the low 16 bits of the results in mm1. */
int pmullw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F D5  /r RMBoth     | Multiply the packed signed word integers in mm1 register and mm2/m64, and store the low 16 bits of the results in mm1. */
 /*  0F D5  /r RMBoth     | Multiply the packed signed word integers in mm1 register and mm2/m64, and store the low 16 bits of the results in mm1. */
int pmullw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F D5  /r RMBoth     | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the low 16 bits of the results in xmm1. */
 /* 66 0F D5  /r RMBoth     | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the low 16 bits of the results in xmm1. */
int pmullw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F D5  /r RMBoth     | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the low 16 bits of the results in xmm1. */
 /* 66 0F D5  /r RMBoth     | Multiply the packed signed word integers in xmm1 and xmm2/m128, and store the low 16 bits of the results in xmm1. */
int pmullw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F F4  /r RMBoth     | Multiply unsigned doubleword integer in mm1 by unsigned doubleword integer in mm2/m64, and store the quadword result in mm1. */
 /*  0F F4  /r RMBoth     | Multiply unsigned doubleword integer in mm1 by unsigned doubleword integer in mm2/m64, and store the quadword result in mm1. */
int pmuludq (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F F4  /r RMBoth     | Multiply unsigned doubleword integer in mm1 by unsigned doubleword integer in mm2/m64, and store the quadword result in mm1. */
 /*  0F F4  /r RMBoth     | Multiply unsigned doubleword integer in mm1 by unsigned doubleword integer in mm2/m64, and store the quadword result in mm1. */
int pmuludq (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F F4  /r RMBoth     | Multiply packed unsigned doubleword integers in xmm1 by packed unsigned doubleword integers in xmm2/m128, and store the quadword results in xmm1. */
 /* 66 0F F4  /r RMBoth     | Multiply packed unsigned doubleword integers in xmm1 by packed unsigned doubleword integers in xmm2/m128, and store the quadword results in xmm1. */
int pmuludq (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F F4  /r RMBoth     | Multiply packed unsigned doubleword integers in xmm1 by packed unsigned doubleword integers in xmm2/m128, and store the quadword results in xmm1. */
 /* 66 0F F4  /r RMBoth     | Multiply packed unsigned doubleword integers in xmm1 by packed unsigned doubleword integers in xmm2/m128, and store the quadword results in xmm1. */
int pmuludq (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  58    +rd  op32  | Pop top of stack into r32; increment stack pointer. */
 /*  58    +rd  op32  | Pop top of stack into r32; increment stack pointer. */
int pop (code_ptr p,
         const DwordReg (&   radd));
/*  8F  /0 RMBoth   op32  | Pop top of stack into m32; increment stack pointer. */
 /*  8F  /0 RMBoth   op32  | Pop top of stack into m32; increment stack pointer. */
int pop (code_ptr p,
         const DwordPtr (&   modrm_rm));
/*  58    +rw  op16  | Pop top of stack into r16; increment stack pointer. */
 /*  58    +rw  op16  | Pop top of stack into r16; increment stack pointer. */
int pop (code_ptr p,
         const WordReg (&   radd));
/*  8F  /0 RMBoth   op16  | Pop top of stack into m16; increment stack pointer. */
 /*  8F  /0 RMBoth   op16  | Pop top of stack into m16; increment stack pointer. */
int pop (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  1F        | Pop top of stack into DS; increment stack pointer. */
 /*  1F        | Pop top of stack into DS; increment stack pointer. */
int pop (code_ptr p,
         const RegDS (&   unused));
/*  07        | Pop top of stack into ES; increment stack pointer. */
 /*  07        | Pop top of stack into ES; increment stack pointer. */
int pop (code_ptr p,
         const RegES (&   unused));
/*  17        | Pop top of stack into SS; increment stack pointer. */
 /*  17        | Pop top of stack into SS; increment stack pointer. */
int pop (code_ptr p,
         const RegSS (&   unused));
/*  0F A1        | Pop top of stack into FS; increment stack pointer by 16 bits. */
 /*  0F A1        | Pop top of stack into FS; increment stack pointer by 16 bits. */
int pop (code_ptr p,
         const RegFS (&   unused));
/*  0F A9        | Pop top of stack into GS; increment stack pointer by 16 bits. */
 /*  0F A9        | Pop top of stack into GS; increment stack pointer by 16 bits. */
int pop (code_ptr p,
         const RegGS (&   unused));
/*  61      op16  | Pop DI, SI, BP, BX, DX, CX, and AX. */
 /*  61      op16  | Pop DI, SI, BP, BX, DX, CX, and AX. */
int popa (code_ptr p);
/*  61      op32  | Pop EDI, ESI, EBP, EBX, EDX, ECX, and EAX. */
 /*  61      op32  | Pop EDI, ESI, EBP, EBX, EDX, ECX, and EAX. */
int popad (code_ptr p);
/* F3 0F B8  /r RMBoth   op16  | POPCNT on r/m16 */
 /* F3 0F B8  /r RMBoth   op16  | POPCNT on r/m16 */
int popcnt (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordReg (&   modrm_rm));
/* F3 0F B8  /r RMBoth   op16  | POPCNT on r/m16 */
 /* F3 0F B8  /r RMBoth   op16  | POPCNT on r/m16 */
int popcnt (code_ptr p,
            const WordReg (&   modrm_reg),
            const WordPtr (&   modrm_rm));
/* F3 0F B8  /r RMBoth   op32  | POPCNT on r/m32 */
 /* F3 0F B8  /r RMBoth   op32  | POPCNT on r/m32 */
int popcnt (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordReg (&   modrm_rm));
/* F3 0F B8  /r RMBoth   op32  | POPCNT on r/m32 */
 /* F3 0F B8  /r RMBoth   op32  | POPCNT on r/m32 */
int popcnt (code_ptr p,
            const DwordReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  9D      op16  | Pop top of stack into lower 16 bits of EFLAGS. */
 /*  9D      op16  | Pop top of stack into lower 16 bits of EFLAGS. */
int popf (code_ptr p);
/*  9D      op32  | Pop top of stack into EFLAGS. */
 /*  9D      op32  | Pop top of stack into EFLAGS. */
int popfd (code_ptr p);
/*  0F EB  /r RMBoth     | Bitwise OR of mm/m64 and mm. */
 /*  0F EB  /r RMBoth     | Bitwise OR of mm/m64 and mm. */
int por (code_ptr p,
         const MmReg (&   modrm_reg),
         const MmReg (&   modrm_rm));
/*  0F EB  /r RMBoth     | Bitwise OR of mm/m64 and mm. */
 /*  0F EB  /r RMBoth     | Bitwise OR of mm/m64 and mm. */
int por (code_ptr p,
         const MmReg (&   modrm_reg),
         const QwordPtr (&   modrm_rm));
/* 66 0F EB  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
 /* 66 0F EB  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
int por (code_ptr p,
         const XmmReg (&   modrm_reg),
         const XmmReg (&   modrm_rm));
/* 66 0F EB  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
 /* 66 0F EB  /r RMBoth     | Bitwise OR of xmm2/m128 and xmm1. */
int por (code_ptr p,
         const XmmReg (&   modrm_reg),
         const XmmWordPtr (&   modrm_rm));
/*  0F 18  /0 RMMemOnly     | Move data from m8 closer to the processor using NTA hint. */
 /*  0F 18  /0 RMMemOnly     | Move data from m8 closer to the processor using NTA hint. */
int prefetchnta (code_ptr p,
                 const BytePtr (&   modrm_rm));
/*  0F 18  /1 RMMemOnly     | Move data from m8 closer to the processor using T0 hint. */
 /*  0F 18  /1 RMMemOnly     | Move data from m8 closer to the processor using T0 hint. */
int prefetcht0 (code_ptr p,
                const BytePtr (&   modrm_rm));
/*  0F 18  /2 RMMemOnly     | Move data from m8 closer to the processor using T1 hint. */
 /*  0F 18  /2 RMMemOnly     | Move data from m8 closer to the processor using T1 hint. */
int prefetcht1 (code_ptr p,
                const BytePtr (&   modrm_rm));
/*  0F 18  /3 RMMemOnly     | Move data from m8 closer to the processor using T2 hint. */
 /*  0F 18  /3 RMMemOnly     | Move data from m8 closer to the processor using T2 hint. */
int prefetcht2 (code_ptr p,
                const BytePtr (&   modrm_rm));
/*  0F F6  /r RMBoth     | Computes the absolute differences of the packed unsigned byte integers from mm2 /m64 and mm1; differences are then summed to produce an unsigned word integer result. */
 /*  0F F6  /r RMBoth     | Computes the absolute differences of the packed unsigned byte integers from mm2 /m64 and mm1; differences are then summed to produce an unsigned word integer result. */
int psadbw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F F6  /r RMBoth     | Computes the absolute differences of the packed unsigned byte integers from mm2 /m64 and mm1; differences are then summed to produce an unsigned word integer result. */
 /*  0F F6  /r RMBoth     | Computes the absolute differences of the packed unsigned byte integers from mm2 /m64 and mm1; differences are then summed to produce an unsigned word integer result. */
int psadbw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F F6  /r RMBoth     | Computes the absolute differences of the packed unsigned byte integers from xmm2 /m128 and xmm1; the 8 low differences and 8 high differences are then summed separately to produce two unsigned word integer results. */
 /* 66 0F F6  /r RMBoth     | Computes the absolute differences of the packed unsigned byte integers from xmm2 /m128 and xmm1; the 8 low differences and 8 high differences are then summed separately to produce two unsigned word integer results. */
int psadbw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F F6  /r RMBoth     | Computes the absolute differences of the packed unsigned byte integers from xmm2 /m128 and xmm1; the 8 low differences and 8 high differences are then summed separately to produce two unsigned word integer results. */
 /* 66 0F F6  /r RMBoth     | Computes the absolute differences of the packed unsigned byte integers from xmm2 /m128 and xmm1; the 8 low differences and 8 high differences are then summed separately to produce two unsigned word integer results. */
int psadbw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 38 00  /r RMBoth     | Shuffle bytes in mm1 according to contents of mm2/m64. */
 /*  0F 38 00  /r RMBoth     | Shuffle bytes in mm1 according to contents of mm2/m64. */
int pshufb (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F 38 00  /r RMBoth     | Shuffle bytes in mm1 according to contents of mm2/m64. */
 /*  0F 38 00  /r RMBoth     | Shuffle bytes in mm1 according to contents of mm2/m64. */
int pshufb (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 38 00  /r RMBoth     | Shuffle bytes in xmm1 according to contents of xmm2/m128. */
 /* 66 0F 38 00  /r RMBoth     | Shuffle bytes in xmm1 according to contents of xmm2/m128. */
int pshufb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 00  /r RMBoth     | Shuffle bytes in xmm1 according to contents of xmm2/m128. */
 /* 66 0F 38 00  /r RMBoth     | Shuffle bytes in xmm1 according to contents of xmm2/m128. */
int pshufb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* 66 0F 70  /r RMBoth  ib   | Shuffle the doublewords in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
 /* 66 0F 70  /r RMBoth  ib   | Shuffle the doublewords in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
int pshufd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm),
            imm8_t imm);
/* 66 0F 70  /r RMBoth  ib   | Shuffle the doublewords in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
 /* 66 0F 70  /r RMBoth  ib   | Shuffle the doublewords in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
int pshufd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm),
            imm8_t imm);
/* F3 0F 70  /r RMBoth  ib   | Shuffle the high words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
 /* F3 0F 70  /r RMBoth  ib   | Shuffle the high words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
int pshufhw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* F3 0F 70  /r RMBoth  ib   | Shuffle the high words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
 /* F3 0F 70  /r RMBoth  ib   | Shuffle the high words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
int pshufhw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/* F2 0F 70  /r RMBoth  ib   | Shuffle the low words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
 /* F2 0F 70  /r RMBoth  ib   | Shuffle the low words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
int pshuflw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* F2 0F 70  /r RMBoth  ib   | Shuffle the low words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
 /* F2 0F 70  /r RMBoth  ib   | Shuffle the low words in xmm2/m128 based on the encoding in imm8 and store the result in xmm1. */
int pshuflw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/*  0F 70  /r RMBoth  ib   | Shuffle the words in mm2/m64 based on the encoding in imm8 and store the result in mm1. */
 /*  0F 70  /r RMBoth  ib   | Shuffle the words in mm2/m64 based on the encoding in imm8 and store the result in mm1. */
int pshufw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm),
            imm8_t imm);
/*  0F 70  /r RMBoth  ib   | Shuffle the words in mm2/m64 based on the encoding in imm8 and store the result in mm1. */
 /*  0F 70  /r RMBoth  ib   | Shuffle the words in mm2/m64 based on the encoding in imm8 and store the result in mm1. */
int pshufw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm),
            imm8_t imm);
/*  0F 38 08  /r RMBoth     | Negate/zero/preserve packed byte integers in mm1 depending on the corresponding sign in mm2/m64 */
 /*  0F 38 08  /r RMBoth     | Negate/zero/preserve packed byte integers in mm1 depending on the corresponding sign in mm2/m64 */
int psignb (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F 38 08  /r RMBoth     | Negate/zero/preserve packed byte integers in mm1 depending on the corresponding sign in mm2/m64 */
 /*  0F 38 08  /r RMBoth     | Negate/zero/preserve packed byte integers in mm1 depending on the corresponding sign in mm2/m64 */
int psignb (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 38 08  /r RMBoth     | Negate/zero/preserve packed byte integers in xmm1 depending on the corresponding sign in xmm2/m128. */
 /* 66 0F 38 08  /r RMBoth     | Negate/zero/preserve packed byte integers in xmm1 depending on the corresponding sign in xmm2/m128. */
int psignb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 08  /r RMBoth     | Negate/zero/preserve packed byte integers in xmm1 depending on the corresponding sign in xmm2/m128. */
 /* 66 0F 38 08  /r RMBoth     | Negate/zero/preserve packed byte integers in xmm1 depending on the corresponding sign in xmm2/m128. */
int psignb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 38 0A  /r RMBoth     | Negate/zero/preserve packed doubleword integers in mm1 depending on the corresponding sign in mm2/m128. */
 /*  0F 38 0A  /r RMBoth     | Negate/zero/preserve packed doubleword integers in mm1 depending on the corresponding sign in mm2/m128. */
int psignd (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F 38 0A  /r RMBoth     | Negate/zero/preserve packed doubleword integers in mm1 depending on the corresponding sign in mm2/m128. */
 /*  0F 38 0A  /r RMBoth     | Negate/zero/preserve packed doubleword integers in mm1 depending on the corresponding sign in mm2/m128. */
int psignd (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 38 0A  /r RMBoth     | Negate/zero/preserve packed doubleword integers in xmm1 depending on the corresponding sign in xmm2/m128. */
 /* 66 0F 38 0A  /r RMBoth     | Negate/zero/preserve packed doubleword integers in xmm1 depending on the corresponding sign in xmm2/m128. */
int psignd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 0A  /r RMBoth     | Negate/zero/preserve packed doubleword integers in xmm1 depending on the corresponding sign in xmm2/m128. */
 /* 66 0F 38 0A  /r RMBoth     | Negate/zero/preserve packed doubleword integers in xmm1 depending on the corresponding sign in xmm2/m128. */
int psignd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 38 09  /r RMBoth     | Negate/zero/preserve packed word integers in mm1 depending on the corresponding sign in mm2/m128. */
 /*  0F 38 09  /r RMBoth     | Negate/zero/preserve packed word integers in mm1 depending on the corresponding sign in mm2/m128. */
int psignw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F 38 09  /r RMBoth     | Negate/zero/preserve packed word integers in mm1 depending on the corresponding sign in mm2/m128. */
 /*  0F 38 09  /r RMBoth     | Negate/zero/preserve packed word integers in mm1 depending on the corresponding sign in mm2/m128. */
int psignw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F 38 09  /r RMBoth     | Negate/zero/preserve packed word integers in xmm1 depending on the corresponding sign in xmm2/m128. */
 /* 66 0F 38 09  /r RMBoth     | Negate/zero/preserve packed word integers in xmm1 depending on the corresponding sign in xmm2/m128. */
int psignw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 38 09  /r RMBoth     | Negate/zero/preserve packed word integers in xmm1 depending on the corresponding sign in xmm2/m128. */
 /* 66 0F 38 09  /r RMBoth     | Negate/zero/preserve packed word integers in xmm1 depending on the corresponding sign in xmm2/m128. */
int psignw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 72  /6 RMRegOnly  ib   | Shift doublewords in mm left by imm8 while shifting in 0s. */
 /*  0F 72  /6 RMRegOnly  ib   | Shift doublewords in mm left by imm8 while shifting in 0s. */
int pslld (code_ptr p,
           const MmReg (&   modrm_rm),
           imm8_t imm);
/*  0F F2  /r RMBoth     | Shift doublewords in mm left by mm/m64 while shifting in 0s. */
 /*  0F F2  /r RMBoth     | Shift doublewords in mm left by mm/m64 while shifting in 0s. */
int pslld (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F F2  /r RMBoth     | Shift doublewords in mm left by mm/m64 while shifting in 0s. */
 /*  0F F2  /r RMBoth     | Shift doublewords in mm left by mm/m64 while shifting in 0s. */
int pslld (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 72  /6 RMRegOnly  ib   | Shift doublewords in xmm1 left by imm8 while shifting in 0s. */
 /* 66 0F 72  /6 RMRegOnly  ib   | Shift doublewords in xmm1 left by imm8 while shifting in 0s. */
int pslld (code_ptr p,
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F F2  /r RMBoth     | Shift doublewords in xmm1 left by xmm2/m128 while shifting in 0s. */
 /* 66 0F F2  /r RMBoth     | Shift doublewords in xmm1 left by xmm2/m128 while shifting in 0s. */
int pslld (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F F2  /r RMBoth     | Shift doublewords in xmm1 left by xmm2/m128 while shifting in 0s. */
 /* 66 0F F2  /r RMBoth     | Shift doublewords in xmm1 left by xmm2/m128 while shifting in 0s. */
int pslld (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* 66 0F 73  /7 RMRegOnly  ib   | Shift xmm1 left by imm8 bytes while shifting in 0s. */
 /* 66 0F 73  /7 RMRegOnly  ib   | Shift xmm1 left by imm8 bytes while shifting in 0s. */
int pslldq (code_ptr p,
            const XmmReg (&   modrm_rm),
            imm8_t imm);
/*  0F 73  /6 RMRegOnly  ib   | Shift quadword in mm left by imm8 while shifting in 0s. */
 /*  0F 73  /6 RMRegOnly  ib   | Shift quadword in mm left by imm8 while shifting in 0s. */
int psllq (code_ptr p,
           const MmReg (&   modrm_rm),
           imm8_t imm);
/*  0F F3  /r RMBoth     | Shift quadword in mm left by mm/m64 while shifting in 0s. */
 /*  0F F3  /r RMBoth     | Shift quadword in mm left by mm/m64 while shifting in 0s. */
int psllq (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F F3  /r RMBoth     | Shift quadword in mm left by mm/m64 while shifting in 0s. */
 /*  0F F3  /r RMBoth     | Shift quadword in mm left by mm/m64 while shifting in 0s. */
int psllq (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 73  /6 RMRegOnly  ib   | Shift quadwords in xmm1 left by imm8 while shifting in 0s. */
 /* 66 0F 73  /6 RMRegOnly  ib   | Shift quadwords in xmm1 left by imm8 while shifting in 0s. */
int psllq (code_ptr p,
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F F3  /r RMBoth     | Shift quadwords in xmm1 left by xmm2/m128 while shifting in 0s. */
 /* 66 0F F3  /r RMBoth     | Shift quadwords in xmm1 left by xmm2/m128 while shifting in 0s. */
int psllq (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F F3  /r RMBoth     | Shift quadwords in xmm1 left by xmm2/m128 while shifting in 0s. */
 /* 66 0F F3  /r RMBoth     | Shift quadwords in xmm1 left by xmm2/m128 while shifting in 0s. */
int psllq (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 71  /6 RMRegOnly  ib   | Shift words in mm left by imm8 while shifting in 0s. */
 /*  0F 71  /6 RMRegOnly  ib   | Shift words in mm left by imm8 while shifting in 0s. */
int psllw (code_ptr p,
           const MmReg (&   modrm_rm),
           imm8_t imm);
/*  0F F1  /r RMBoth     | Shift words in mm left mm/m64 while shifting in 0s. */
 /*  0F F1  /r RMBoth     | Shift words in mm left mm/m64 while shifting in 0s. */
int psllw (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F F1  /r RMBoth     | Shift words in mm left mm/m64 while shifting in 0s. */
 /*  0F F1  /r RMBoth     | Shift words in mm left mm/m64 while shifting in 0s. */
int psllw (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 71  /6 RMRegOnly  ib   | Shift words in xmm1 left by imm8 while shifting in 0s. */
 /* 66 0F 71  /6 RMRegOnly  ib   | Shift words in xmm1 left by imm8 while shifting in 0s. */
int psllw (code_ptr p,
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F F1  /r RMBoth     | Shift words in xmm1 left by xmm2/m128 while shifting in 0s. */
 /* 66 0F F1  /r RMBoth     | Shift words in xmm1 left by xmm2/m128 while shifting in 0s. */
int psllw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F F1  /r RMBoth     | Shift words in xmm1 left by xmm2/m128 while shifting in 0s. */
 /* 66 0F F1  /r RMBoth     | Shift words in xmm1 left by xmm2/m128 while shifting in 0s. */
int psllw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 72  /4 RMRegOnly  ib   | Shift doublewords in mm right by imm8 while shifting in sign bits. */
 /*  0F 72  /4 RMRegOnly  ib   | Shift doublewords in mm right by imm8 while shifting in sign bits. */
int psrad (code_ptr p,
           const MmReg (&   modrm_rm),
           imm8_t imm);
/*  0F E2  /r RMBoth     | Shift doublewords in mm right by mm/m64 while shifting in sign bits. */
 /*  0F E2  /r RMBoth     | Shift doublewords in mm right by mm/m64 while shifting in sign bits. */
int psrad (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F E2  /r RMBoth     | Shift doublewords in mm right by mm/m64 while shifting in sign bits. */
 /*  0F E2  /r RMBoth     | Shift doublewords in mm right by mm/m64 while shifting in sign bits. */
int psrad (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 72  /4 RMRegOnly  ib   | Shift doublewords in xmm1 right by imm8 while shifting in sign bits. */
 /* 66 0F 72  /4 RMRegOnly  ib   | Shift doublewords in xmm1 right by imm8 while shifting in sign bits. */
int psrad (code_ptr p,
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F E2  /r RMBoth     | Shift doubleword in xmm1 right by xmm2 /m128 while shifting in sign bits. */
 /* 66 0F E2  /r RMBoth     | Shift doubleword in xmm1 right by xmm2 /m128 while shifting in sign bits. */
int psrad (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F E2  /r RMBoth     | Shift doubleword in xmm1 right by xmm2 /m128 while shifting in sign bits. */
 /* 66 0F E2  /r RMBoth     | Shift doubleword in xmm1 right by xmm2 /m128 while shifting in sign bits. */
int psrad (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 71  /4 RMRegOnly  ib   | Shift words in mm right by imm8 while shifting in sign bits */
 /*  0F 71  /4 RMRegOnly  ib   | Shift words in mm right by imm8 while shifting in sign bits */
int psraw (code_ptr p,
           const MmReg (&   modrm_rm),
           imm8_t imm);
/*  0F E1  /r RMBoth     | Shift words in mm right by mm/m64 while shifting in sign bits. */
 /*  0F E1  /r RMBoth     | Shift words in mm right by mm/m64 while shifting in sign bits. */
int psraw (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F E1  /r RMBoth     | Shift words in mm right by mm/m64 while shifting in sign bits. */
 /*  0F E1  /r RMBoth     | Shift words in mm right by mm/m64 while shifting in sign bits. */
int psraw (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 71  /4 RMRegOnly  ib   | Shift words in xmm1 right by imm8 while shifting in sign bits */
 /* 66 0F 71  /4 RMRegOnly  ib   | Shift words in xmm1 right by imm8 while shifting in sign bits */
int psraw (code_ptr p,
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F E1  /r RMBoth     | Shift words in xmm1 right by xmm2/m128 while shifting in sign bits. */
 /* 66 0F E1  /r RMBoth     | Shift words in xmm1 right by xmm2/m128 while shifting in sign bits. */
int psraw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F E1  /r RMBoth     | Shift words in xmm1 right by xmm2/m128 while shifting in sign bits. */
 /* 66 0F E1  /r RMBoth     | Shift words in xmm1 right by xmm2/m128 while shifting in sign bits. */
int psraw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 72  /2 RMRegOnly  ib   | Shift doublewords in mm right by imm8 while shifting in 0s. */
 /*  0F 72  /2 RMRegOnly  ib   | Shift doublewords in mm right by imm8 while shifting in 0s. */
int psrld (code_ptr p,
           const MmReg (&   modrm_rm),
           imm8_t imm);
/*  0F D2  /r RMBoth     | Shift doublewords in mm right by amount specified in mm/m64 while shifting in 0s. */
 /*  0F D2  /r RMBoth     | Shift doublewords in mm right by amount specified in mm/m64 while shifting in 0s. */
int psrld (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F D2  /r RMBoth     | Shift doublewords in mm right by amount specified in mm/m64 while shifting in 0s. */
 /*  0F D2  /r RMBoth     | Shift doublewords in mm right by amount specified in mm/m64 while shifting in 0s. */
int psrld (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 72  /2 RMRegOnly  ib   | Shift doublewords in xmm1 right by imm8 while shifting in 0s. */
 /* 66 0F 72  /2 RMRegOnly  ib   | Shift doublewords in xmm1 right by imm8 while shifting in 0s. */
int psrld (code_ptr p,
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F D2  /r RMBoth     | Shift doublewords in xmm1 right by amount specified in xmm2 /m128 while shifting in 0s. */
 /* 66 0F D2  /r RMBoth     | Shift doublewords in xmm1 right by amount specified in xmm2 /m128 while shifting in 0s. */
int psrld (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F D2  /r RMBoth     | Shift doublewords in xmm1 right by amount specified in xmm2 /m128 while shifting in 0s. */
 /* 66 0F D2  /r RMBoth     | Shift doublewords in xmm1 right by amount specified in xmm2 /m128 while shifting in 0s. */
int psrld (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* 66 0F 73  /3 RMRegOnly  ib   | Shift xmm1 right by imm8 while shifting in 0s. */
 /* 66 0F 73  /3 RMRegOnly  ib   | Shift xmm1 right by imm8 while shifting in 0s. */
int psrldq (code_ptr p,
            const XmmReg (&   modrm_rm),
            imm8_t imm);
/*  0F 73  /2 RMRegOnly  ib   | Shift mm right by imm8 while shifting in 0s. */
 /*  0F 73  /2 RMRegOnly  ib   | Shift mm right by imm8 while shifting in 0s. */
int psrlq (code_ptr p,
           const MmReg (&   modrm_rm),
           imm8_t imm);
/*  0F D3  /r RMBoth     | Shift mm right by amount specified in mm/m64 while shifting in 0s. */
 /*  0F D3  /r RMBoth     | Shift mm right by amount specified in mm/m64 while shifting in 0s. */
int psrlq (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F D3  /r RMBoth     | Shift mm right by amount specified in mm/m64 while shifting in 0s. */
 /*  0F D3  /r RMBoth     | Shift mm right by amount specified in mm/m64 while shifting in 0s. */
int psrlq (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 73  /2 RMRegOnly  ib   | Shift quadwords in xmm1 right by imm8 while shifting in 0s. */
 /* 66 0F 73  /2 RMRegOnly  ib   | Shift quadwords in xmm1 right by imm8 while shifting in 0s. */
int psrlq (code_ptr p,
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F D3  /r RMBoth     | Shift quadwords in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. */
 /* 66 0F D3  /r RMBoth     | Shift quadwords in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. */
int psrlq (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F D3  /r RMBoth     | Shift quadwords in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. */
 /* 66 0F D3  /r RMBoth     | Shift quadwords in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. */
int psrlq (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 71  /2 RMRegOnly  ib   | Shift words in mm right by imm8 while shifting in 0s. */
 /*  0F 71  /2 RMRegOnly  ib   | Shift words in mm right by imm8 while shifting in 0s. */
int psrlw (code_ptr p,
           const MmReg (&   modrm_rm),
           imm8_t imm);
/*  0F D1  /r RMBoth     | Shift words in mm right by amount specified in mm/m64 while shifting in 0s. */
 /*  0F D1  /r RMBoth     | Shift words in mm right by amount specified in mm/m64 while shifting in 0s. */
int psrlw (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F D1  /r RMBoth     | Shift words in mm right by amount specified in mm/m64 while shifting in 0s. */
 /*  0F D1  /r RMBoth     | Shift words in mm right by amount specified in mm/m64 while shifting in 0s. */
int psrlw (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F 71  /2 RMRegOnly  ib   | Shift words in xmm1 right by imm8 while shifting in 0s. */
 /* 66 0F 71  /2 RMRegOnly  ib   | Shift words in xmm1 right by imm8 while shifting in 0s. */
int psrlw (code_ptr p,
           const XmmReg (&   modrm_rm),
           imm8_t imm);
/* 66 0F D1  /r RMBoth     | Shift words in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. */
 /* 66 0F D1  /r RMBoth     | Shift words in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. */
int psrlw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F D1  /r RMBoth     | Shift words in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. */
 /* 66 0F D1  /r RMBoth     | Shift words in xmm1 right by amount specified in xmm2/m128 while shifting in 0s. */
int psrlw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F F8  /r RMBoth     | Subtract packed byte integers in mm/m64 from packed byte integers in mm. */
 /*  0F F8  /r RMBoth     | Subtract packed byte integers in mm/m64 from packed byte integers in mm. */
int psubb (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F F8  /r RMBoth     | Subtract packed byte integers in mm/m64 from packed byte integers in mm. */
 /*  0F F8  /r RMBoth     | Subtract packed byte integers in mm/m64 from packed byte integers in mm. */
int psubb (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F F8  /r RMBoth     | Subtract packed byte integers in xmm2/m128 from packed byte integers in xmm1. */
 /* 66 0F F8  /r RMBoth     | Subtract packed byte integers in xmm2/m128 from packed byte integers in xmm1. */
int psubb (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F F8  /r RMBoth     | Subtract packed byte integers in xmm2/m128 from packed byte integers in xmm1. */
 /* 66 0F F8  /r RMBoth     | Subtract packed byte integers in xmm2/m128 from packed byte integers in xmm1. */
int psubb (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F FA  /r RMBoth     | Subtract packed doubleword integers in mm/m64 from packed doubleword integers in mm. */
 /*  0F FA  /r RMBoth     | Subtract packed doubleword integers in mm/m64 from packed doubleword integers in mm. */
int psubd (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F FA  /r RMBoth     | Subtract packed doubleword integers in mm/m64 from packed doubleword integers in mm. */
 /*  0F FA  /r RMBoth     | Subtract packed doubleword integers in mm/m64 from packed doubleword integers in mm. */
int psubd (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F FA  /r RMBoth     | Subtract packed doubleword integers in xmm2/mem128 from packed doubleword integers in xmm1. */
 /* 66 0F FA  /r RMBoth     | Subtract packed doubleword integers in xmm2/mem128 from packed doubleword integers in xmm1. */
int psubd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F FA  /r RMBoth     | Subtract packed doubleword integers in xmm2/mem128 from packed doubleword integers in xmm1. */
 /* 66 0F FA  /r RMBoth     | Subtract packed doubleword integers in xmm2/mem128 from packed doubleword integers in xmm1. */
int psubd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F FB  /r RMBoth     | Subtract quadword integer in mm1 from mm2 /m64. */
 /*  0F FB  /r RMBoth     | Subtract quadword integer in mm1 from mm2 /m64. */
int psubq (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F FB  /r RMBoth     | Subtract quadword integer in mm1 from mm2 /m64. */
 /*  0F FB  /r RMBoth     | Subtract quadword integer in mm1 from mm2 /m64. */
int psubq (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F FB  /r RMBoth     | Subtract packed quadword integers in xmm1 from xmm2 /m128. */
 /* 66 0F FB  /r RMBoth     | Subtract packed quadword integers in xmm1 from xmm2 /m128. */
int psubq (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F FB  /r RMBoth     | Subtract packed quadword integers in xmm1 from xmm2 /m128. */
 /* 66 0F FB  /r RMBoth     | Subtract packed quadword integers in xmm1 from xmm2 /m128. */
int psubq (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F E8  /r RMBoth     | Subtract signed packed bytes in mm/m64 from signed packed bytes in mm and saturate results. */
 /*  0F E8  /r RMBoth     | Subtract signed packed bytes in mm/m64 from signed packed bytes in mm and saturate results. */
int psubsb (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F E8  /r RMBoth     | Subtract signed packed bytes in mm/m64 from signed packed bytes in mm and saturate results. */
 /*  0F E8  /r RMBoth     | Subtract signed packed bytes in mm/m64 from signed packed bytes in mm and saturate results. */
int psubsb (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F E8  /r RMBoth     | Subtract packed signed byte integers in xmm2/m128 from packed signed byte integers in xmm1 and saturate results. */
 /* 66 0F E8  /r RMBoth     | Subtract packed signed byte integers in xmm2/m128 from packed signed byte integers in xmm1 and saturate results. */
int psubsb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F E8  /r RMBoth     | Subtract packed signed byte integers in xmm2/m128 from packed signed byte integers in xmm1 and saturate results. */
 /* 66 0F E8  /r RMBoth     | Subtract packed signed byte integers in xmm2/m128 from packed signed byte integers in xmm1 and saturate results. */
int psubsb (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F E9  /r RMBoth     | Subtract signed packed words in mm/m64 from signed packed words in mm and saturate results. */
 /*  0F E9  /r RMBoth     | Subtract signed packed words in mm/m64 from signed packed words in mm and saturate results. */
int psubsw (code_ptr p,
            const MmReg (&   modrm_reg),
            const MmReg (&   modrm_rm));
/*  0F E9  /r RMBoth     | Subtract signed packed words in mm/m64 from signed packed words in mm and saturate results. */
 /*  0F E9  /r RMBoth     | Subtract signed packed words in mm/m64 from signed packed words in mm and saturate results. */
int psubsw (code_ptr p,
            const MmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* 66 0F E9  /r RMBoth     | Subtract packed signed word integers in xmm2/m128 from packed signed word integers in xmm1 and saturate results. */
 /* 66 0F E9  /r RMBoth     | Subtract packed signed word integers in xmm2/m128 from packed signed word integers in xmm1 and saturate results. */
int psubsw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F E9  /r RMBoth     | Subtract packed signed word integers in xmm2/m128 from packed signed word integers in xmm1 and saturate results. */
 /* 66 0F E9  /r RMBoth     | Subtract packed signed word integers in xmm2/m128 from packed signed word integers in xmm1 and saturate results. */
int psubsw (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F D8  /r RMBoth     | Subtract unsigned packed bytes in mm/m64 from unsigned packed bytes in mm and saturate result. */
 /*  0F D8  /r RMBoth     | Subtract unsigned packed bytes in mm/m64 from unsigned packed bytes in mm and saturate result. */
int psubusb (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F D8  /r RMBoth     | Subtract unsigned packed bytes in mm/m64 from unsigned packed bytes in mm and saturate result. */
 /*  0F D8  /r RMBoth     | Subtract unsigned packed bytes in mm/m64 from unsigned packed bytes in mm and saturate result. */
int psubusb (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F D8  /r RMBoth     | Subtract packed unsigned byte integers in xmm2/m128 from packed unsigned byte integers in xmm1 and saturate result. */
 /* 66 0F D8  /r RMBoth     | Subtract packed unsigned byte integers in xmm2/m128 from packed unsigned byte integers in xmm1 and saturate result. */
int psubusb (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F D8  /r RMBoth     | Subtract packed unsigned byte integers in xmm2/m128 from packed unsigned byte integers in xmm1 and saturate result. */
 /* 66 0F D8  /r RMBoth     | Subtract packed unsigned byte integers in xmm2/m128 from packed unsigned byte integers in xmm1 and saturate result. */
int psubusb (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F D9  /r RMBoth     | Subtract unsigned packed words in mm/m64 from unsigned packed words in mm and saturate result. */
 /*  0F D9  /r RMBoth     | Subtract unsigned packed words in mm/m64 from unsigned packed words in mm and saturate result. */
int psubusw (code_ptr p,
             const MmReg (&   modrm_reg),
             const MmReg (&   modrm_rm));
/*  0F D9  /r RMBoth     | Subtract unsigned packed words in mm/m64 from unsigned packed words in mm and saturate result. */
 /*  0F D9  /r RMBoth     | Subtract unsigned packed words in mm/m64 from unsigned packed words in mm and saturate result. */
int psubusw (code_ptr p,
             const MmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/* 66 0F D9  /r RMBoth     | Subtract packed unsigned word integers in xmm2/m128 from packed unsigned word integers in xmm1 and saturate result. */
 /* 66 0F D9  /r RMBoth     | Subtract packed unsigned word integers in xmm2/m128 from packed unsigned word integers in xmm1 and saturate result. */
int psubusw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F D9  /r RMBoth     | Subtract packed unsigned word integers in xmm2/m128 from packed unsigned word integers in xmm1 and saturate result. */
 /* 66 0F D9  /r RMBoth     | Subtract packed unsigned word integers in xmm2/m128 from packed unsigned word integers in xmm1 and saturate result. */
int psubusw (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/*  0F F9  /r RMBoth     | Subtract packed word integers in mm/m64 from packed word integers in mm. */
 /*  0F F9  /r RMBoth     | Subtract packed word integers in mm/m64 from packed word integers in mm. */
int psubw (code_ptr p,
           const MmReg (&   modrm_reg),
           const MmReg (&   modrm_rm));
/*  0F F9  /r RMBoth     | Subtract packed word integers in mm/m64 from packed word integers in mm. */
 /*  0F F9  /r RMBoth     | Subtract packed word integers in mm/m64 from packed word integers in mm. */
int psubw (code_ptr p,
           const MmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* 66 0F F9  /r RMBoth     | Subtract packed word integers in xmm2/m128 from packed word integers in xmm1. */
 /* 66 0F F9  /r RMBoth     | Subtract packed word integers in xmm2/m128 from packed word integers in xmm1. */
int psubw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F F9  /r RMBoth     | Subtract packed word integers in xmm2/m128 from packed word integers in xmm1. */
 /* 66 0F F9  /r RMBoth     | Subtract packed word integers in xmm2/m128 from packed word integers in xmm1. */
int psubw (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* 66 0F 38 17  /r RMBoth     | Set ZF if xmm2/m128 AND xmm1 result is all 0s. Set CF if xmm2/m128 AND NOT xmm1 result is all 0s. */
 /* 66 0F 38 17  /r RMBoth     | Set ZF if xmm2/m128 AND xmm1 result is all 0s. Set CF if xmm2/m128 AND NOT xmm1 result is all 0s. */
int ptest (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 38 17  /r RMBoth     | Set ZF if xmm2/m128 AND xmm1 result is all 0s. Set CF if xmm2/m128 AND NOT xmm1 result is all 0s. */
 /* 66 0F 38 17  /r RMBoth     | Set ZF if xmm2/m128 AND xmm1 result is all 0s. Set CF if xmm2/m128 AND NOT xmm1 result is all 0s. */
int ptest (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 68  /r RMBoth     | Unpack and interleave high- order bytes from mm and mm/m64 into mm. */
 /*  0F 68  /r RMBoth     | Unpack and interleave high- order bytes from mm and mm/m64 into mm. */
int punpckhbw (code_ptr p,
               const MmReg (&   modrm_reg),
               const MmReg (&   modrm_rm));
/*  0F 68  /r RMBoth     | Unpack and interleave high- order bytes from mm and mm/m64 into mm. */
 /*  0F 68  /r RMBoth     | Unpack and interleave high- order bytes from mm and mm/m64 into mm. */
int punpckhbw (code_ptr p,
               const MmReg (&   modrm_reg),
               const QwordPtr (&   modrm_rm));
/* 66 0F 68  /r RMBoth     | Unpack and interleave high- order bytes from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 68  /r RMBoth     | Unpack and interleave high- order bytes from xmm1 and xmm2/m128 into xmm1. */
int punpckhbw (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F 68  /r RMBoth     | Unpack and interleave high- order bytes from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 68  /r RMBoth     | Unpack and interleave high- order bytes from xmm1 and xmm2/m128 into xmm1. */
int punpckhbw (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/*  0F 6A  /r RMBoth     | Unpack and interleave high- order doublewords from mm and mm/m64 into mm. */
 /*  0F 6A  /r RMBoth     | Unpack and interleave high- order doublewords from mm and mm/m64 into mm. */
int punpckhdq (code_ptr p,
               const MmReg (&   modrm_reg),
               const MmReg (&   modrm_rm));
/*  0F 6A  /r RMBoth     | Unpack and interleave high- order doublewords from mm and mm/m64 into mm. */
 /*  0F 6A  /r RMBoth     | Unpack and interleave high- order doublewords from mm and mm/m64 into mm. */
int punpckhdq (code_ptr p,
               const MmReg (&   modrm_reg),
               const QwordPtr (&   modrm_rm));
/* 66 0F 6A  /r RMBoth     | Unpack and interleave high- order doublewords from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 6A  /r RMBoth     | Unpack and interleave high- order doublewords from xmm1 and xmm2/m128 into xmm1. */
int punpckhdq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F 6A  /r RMBoth     | Unpack and interleave high- order doublewords from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 6A  /r RMBoth     | Unpack and interleave high- order doublewords from xmm1 and xmm2/m128 into xmm1. */
int punpckhdq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/* 66 0F 6D  /r RMBoth     | Unpack and interleave high- order quadwords from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 6D  /r RMBoth     | Unpack and interleave high- order quadwords from xmm1 and xmm2/m128 into xmm1. */
int punpckhqdq (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/* 66 0F 6D  /r RMBoth     | Unpack and interleave high- order quadwords from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 6D  /r RMBoth     | Unpack and interleave high- order quadwords from xmm1 and xmm2/m128 into xmm1. */
int punpckhqdq (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmWordPtr (&   modrm_rm));
/*  0F 69  /r RMBoth     | Unpack and interleave high- order words from mm and mm/m64 into mm. */
 /*  0F 69  /r RMBoth     | Unpack and interleave high- order words from mm and mm/m64 into mm. */
int punpckhwd (code_ptr p,
               const MmReg (&   modrm_reg),
               const MmReg (&   modrm_rm));
/*  0F 69  /r RMBoth     | Unpack and interleave high- order words from mm and mm/m64 into mm. */
 /*  0F 69  /r RMBoth     | Unpack and interleave high- order words from mm and mm/m64 into mm. */
int punpckhwd (code_ptr p,
               const MmReg (&   modrm_reg),
               const QwordPtr (&   modrm_rm));
/* 66 0F 69  /r RMBoth     | Unpack and interleave high- order words from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 69  /r RMBoth     | Unpack and interleave high- order words from xmm1 and xmm2/m128 into xmm1. */
int punpckhwd (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F 69  /r RMBoth     | Unpack and interleave high- order words from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 69  /r RMBoth     | Unpack and interleave high- order words from xmm1 and xmm2/m128 into xmm1. */
int punpckhwd (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/*  0F 60  /r RMBoth     | Interleave low-order bytes from mm and mm/m32 into mm. */
 /*  0F 60  /r RMBoth     | Interleave low-order bytes from mm and mm/m32 into mm. */
int punpcklbw (code_ptr p,
               const MmReg (&   modrm_reg),
               const MmReg (&   modrm_rm));
/*  0F 60  /r RMBoth     | Interleave low-order bytes from mm and mm/m32 into mm. */
 /*  0F 60  /r RMBoth     | Interleave low-order bytes from mm and mm/m32 into mm. */
int punpcklbw (code_ptr p,
               const MmReg (&   modrm_reg),
               const DwordPtr (&   modrm_rm));
/* 66 0F 60  /r RMBoth     | Interleave low-order bytes from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 60  /r RMBoth     | Interleave low-order bytes from xmm1 and xmm2/m128 into xmm1. */
int punpcklbw (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F 60  /r RMBoth     | Interleave low-order bytes from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 60  /r RMBoth     | Interleave low-order bytes from xmm1 and xmm2/m128 into xmm1. */
int punpcklbw (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/*  0F 62  /r RMBoth     | Interleave low-order doublewords from mm and mm/m32 into mm. */
 /*  0F 62  /r RMBoth     | Interleave low-order doublewords from mm and mm/m32 into mm. */
int punpckldq (code_ptr p,
               const MmReg (&   modrm_reg),
               const MmReg (&   modrm_rm));
/*  0F 62  /r RMBoth     | Interleave low-order doublewords from mm and mm/m32 into mm. */
 /*  0F 62  /r RMBoth     | Interleave low-order doublewords from mm and mm/m32 into mm. */
int punpckldq (code_ptr p,
               const MmReg (&   modrm_reg),
               const DwordPtr (&   modrm_rm));
/* 66 0F 62  /r RMBoth     | Interleave low-order doublewords from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 62  /r RMBoth     | Interleave low-order doublewords from xmm1 and xmm2/m128 into xmm1. */
int punpckldq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F 62  /r RMBoth     | Interleave low-order doublewords from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 62  /r RMBoth     | Interleave low-order doublewords from xmm1 and xmm2/m128 into xmm1. */
int punpckldq (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/* 66 0F 6C  /r RMBoth     | Interleave low-order quadword from xmm1 and xmm2/m128 into xmm1 register. */
 /* 66 0F 6C  /r RMBoth     | Interleave low-order quadword from xmm1 and xmm2/m128 into xmm1 register. */
int punpcklqdq (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmReg (&   modrm_rm));
/* 66 0F 6C  /r RMBoth     | Interleave low-order quadword from xmm1 and xmm2/m128 into xmm1 register. */
 /* 66 0F 6C  /r RMBoth     | Interleave low-order quadword from xmm1 and xmm2/m128 into xmm1 register. */
int punpcklqdq (code_ptr p,
                const XmmReg (&   modrm_reg),
                const XmmWordPtr (&   modrm_rm));
/*  0F 61  /r RMBoth     | Interleave low-order words from mm and mm/m32 into mm. */
 /*  0F 61  /r RMBoth     | Interleave low-order words from mm and mm/m32 into mm. */
int punpcklwd (code_ptr p,
               const MmReg (&   modrm_reg),
               const MmReg (&   modrm_rm));
/*  0F 61  /r RMBoth     | Interleave low-order words from mm and mm/m32 into mm. */
 /*  0F 61  /r RMBoth     | Interleave low-order words from mm and mm/m32 into mm. */
int punpcklwd (code_ptr p,
               const MmReg (&   modrm_reg),
               const DwordPtr (&   modrm_rm));
/* 66 0F 61  /r RMBoth     | Interleave low-order words from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 61  /r RMBoth     | Interleave low-order words from xmm1 and xmm2/m128 into xmm1. */
int punpcklwd (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmReg (&   modrm_rm));
/* 66 0F 61  /r RMBoth     | Interleave low-order words from xmm1 and xmm2/m128 into xmm1. */
 /* 66 0F 61  /r RMBoth     | Interleave low-order words from xmm1 and xmm2/m128 into xmm1. */
int punpcklwd (code_ptr p,
               const XmmReg (&   modrm_reg),
               const XmmWordPtr (&   modrm_rm));
/*  50    +rd  op32  | Push r32. */
 /*  50    +rd  op32  | Push r32. */
int push (code_ptr p,
          const DwordReg (&   radd));
/*  FF  /6 RMBoth   op32  | Push r/m32. */
 /*  FF  /6 RMBoth   op32  | Push r/m32. */
int push (code_ptr p,
          const DwordPtr (&   modrm_rm));
/*  50    +rw  op16  | Push r16. */
 /*  50    +rw  op16  | Push r16. */
int push (code_ptr p,
          const WordReg (&   radd));
/*  FF  /6 RMBoth   op16  | Push r/m16. */
 /*  FF  /6 RMBoth   op16  | Push r/m16. */
int push (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  6A     ib   | Push sign-extended imm8. Stack pointer is decremented by the size of stack pointer. */
 /*  6A     ib   | Push sign-extended imm8. Stack pointer is decremented by the size of stack pointer. */
int push (code_ptr p,
          imm8_t imm);
/*  68     iw op16  | Push sign-extended imm16. Stack pointer is decremented by the size of stack pointer. */
 /*  68     iw op16  | Push sign-extended imm16. Stack pointer is decremented by the size of stack pointer. */
int push (code_ptr p,
          imm16_t imm);
/*  68     id op32  | Push sign-extended imm32. Stack pointer is decremented by the size of stack pointer. */
 /*  68     id op32  | Push sign-extended imm32. Stack pointer is decremented by the size of stack pointer. */
int push (code_ptr p,
          imm32_t imm);
/*  1E        | Push DS. */
 /*  1E        | Push DS. */
int push (code_ptr p,
          const RegDS (&   unused));
/*  06        | Push ES. */
 /*  06        | Push ES. */
int push (code_ptr p,
          const RegES (&   unused));
/*  16        | Push SS. */
 /*  16        | Push SS. */
int push (code_ptr p,
          const RegSS (&   unused));
/*  0F A0        | Push FS and decrement stack pointer by 16 bits. */
 /*  0F A0        | Push FS and decrement stack pointer by 16 bits. */
int push (code_ptr p,
          const RegFS (&   unused));
/*  0F A8        | Push GS and decrement stack pointer by 16 bits. */
 /*  0F A8        | Push GS and decrement stack pointer by 16 bits. */
int push (code_ptr p,
          const RegGS (&   unused));
/*  0E        | Push CS. */
 /*  0E        | Push CS. */
int push (code_ptr p,
          const RegCS (&   unused));
/*  60      op16  | Push AX, CX, DX, BX, original SP, BP, SI, and DI. */
 /*  60      op16  | Push AX, CX, DX, BX, original SP, BP, SI, and DI. */
int pusha (code_ptr p);
/*  60      op32  | Push EAX, ECX, EDX, EBX, original ESP, EBP, ESI, and EDI. */
 /*  60      op32  | Push EAX, ECX, EDX, EBX, original ESP, EBP, ESI, and EDI. */
int pushad (code_ptr p);
/*  9C      op16  | Push lower 16 bits of EFLAGS. */
 /*  9C      op16  | Push lower 16 bits of EFLAGS. */
int pushf (code_ptr p);
/*  9C      op32  | Push EFLAGS. */
 /*  9C      op32  | Push EFLAGS. */
int pushfd (code_ptr p);
/*  0F EF  /r RMBoth     | Bitwise XOR of mm/m64 and mm. */
 /*  0F EF  /r RMBoth     | Bitwise XOR of mm/m64 and mm. */
int pxor (code_ptr p,
          const MmReg (&   modrm_reg),
          const MmReg (&   modrm_rm));
/*  0F EF  /r RMBoth     | Bitwise XOR of mm/m64 and mm. */
 /*  0F EF  /r RMBoth     | Bitwise XOR of mm/m64 and mm. */
int pxor (code_ptr p,
          const MmReg (&   modrm_reg),
          const QwordPtr (&   modrm_rm));
/* 66 0F EF  /r RMBoth     | Bitwise XOR of xmm2/m128 and xmm1. */
 /* 66 0F EF  /r RMBoth     | Bitwise XOR of xmm2/m128 and xmm1. */
int pxor (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmReg (&   modrm_rm));
/* 66 0F EF  /r RMBoth     | Bitwise XOR of xmm2/m128 and xmm1. */
 /* 66 0F EF  /r RMBoth     | Bitwise XOR of xmm2/m128 and xmm1. */
int pxor (code_ptr p,
          const XmmReg (&   modrm_reg),
          const XmmWordPtr (&   modrm_rm));
/*  C1  /2 RMBoth  ib op32  | Rotate 33 bits (CF, r/m32) left imm8 times. */
 /*  C1  /2 RMBoth  ib op32  | Rotate 33 bits (CF, r/m32) left imm8 times. */
int rcl (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /2 RMBoth  ib op32  | Rotate 33 bits (CF, r/m32) left imm8 times. */
 /*  C1  /2 RMBoth  ib op32  | Rotate 33 bits (CF, r/m32) left imm8 times. */
int rcl (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  C1  /2 RMBoth  ib op16  | Rotate 17 bits (CF, r/m16) left imm8 times. */
 /*  C1  /2 RMBoth  ib op16  | Rotate 17 bits (CF, r/m16) left imm8 times. */
int rcl (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /2 RMBoth  ib op16  | Rotate 17 bits (CF, r/m16) left imm8 times. */
 /*  C1  /2 RMBoth  ib op16  | Rotate 17 bits (CF, r/m16) left imm8 times. */
int rcl (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  C0  /2 RMBoth  ib   | Rotate 9 bits (CF, r/m8) left imm8 times. */
 /*  C0  /2 RMBoth  ib   | Rotate 9 bits (CF, r/m8) left imm8 times. */
int rcl (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  C0  /2 RMBoth  ib   | Rotate 9 bits (CF, r/m8) left imm8 times. */
 /*  C0  /2 RMBoth  ib   | Rotate 9 bits (CF, r/m8) left imm8 times. */
int rcl (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  D2  /2 RMBoth     | Rotate 9 bits (CF, r/m8) left CL times. */
 /*  D2  /2 RMBoth     | Rotate 9 bits (CF, r/m8) left CL times. */
int rcl (code_ptr p,
         const ByteReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D2  /2 RMBoth     | Rotate 9 bits (CF, r/m8) left CL times. */
 /*  D2  /2 RMBoth     | Rotate 9 bits (CF, r/m8) left CL times. */
int rcl (code_ptr p,
         const BytePtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /2 RMBoth   op16  | Rotate 17 bits (CF, r/m16) left CL times. */
 /*  D3  /2 RMBoth   op16  | Rotate 17 bits (CF, r/m16) left CL times. */
int rcl (code_ptr p,
         const WordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /2 RMBoth   op16  | Rotate 17 bits (CF, r/m16) left CL times. */
 /*  D3  /2 RMBoth   op16  | Rotate 17 bits (CF, r/m16) left CL times. */
int rcl (code_ptr p,
         const WordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /2 RMBoth   op32  | Rotate 33 bits (CF, r/m32) left CL times. */
 /*  D3  /2 RMBoth   op32  | Rotate 33 bits (CF, r/m32) left CL times. */
int rcl (code_ptr p,
         const DwordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /2 RMBoth   op32  | Rotate 33 bits (CF, r/m32) left CL times. */
 /*  D3  /2 RMBoth   op32  | Rotate 33 bits (CF, r/m32) left CL times. */
int rcl (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  0F 53  /r RMBoth     | Computes the approximate reciprocals of the packed single-precision floating- point values in xmm2/m128 and stores the results in xmm1. */
 /*  0F 53  /r RMBoth     | Computes the approximate reciprocals of the packed single-precision floating- point values in xmm2/m128 and stores the results in xmm1. */
int rcpps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 53  /r RMBoth     | Computes the approximate reciprocals of the packed single-precision floating- point values in xmm2/m128 and stores the results in xmm1. */
 /*  0F 53  /r RMBoth     | Computes the approximate reciprocals of the packed single-precision floating- point values in xmm2/m128 and stores the results in xmm1. */
int rcpps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* F3 0F 53  /r RMBoth     | Computes the approximate reciprocal of the scalar single-precision floating- point value in xmm2/m32 and stores the result in xmm1. */
 /* F3 0F 53  /r RMBoth     | Computes the approximate reciprocal of the scalar single-precision floating- point value in xmm2/m32 and stores the result in xmm1. */
int rcpss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F3 0F 53  /r RMBoth     | Computes the approximate reciprocal of the scalar single-precision floating- point value in xmm2/m32 and stores the result in xmm1. */
 /* F3 0F 53  /r RMBoth     | Computes the approximate reciprocal of the scalar single-precision floating- point value in xmm2/m32 and stores the result in xmm1. */
int rcpss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  C1  /3 RMBoth  ib op32  | Rotate 33 bits (CF, r/m32) right imm8 times. */
 /*  C1  /3 RMBoth  ib op32  | Rotate 33 bits (CF, r/m32) right imm8 times. */
int rcr (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /3 RMBoth  ib op32  | Rotate 33 bits (CF, r/m32) right imm8 times. */
 /*  C1  /3 RMBoth  ib op32  | Rotate 33 bits (CF, r/m32) right imm8 times. */
int rcr (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  C1  /3 RMBoth  ib op16  | Rotate 17 bits (CF, r/m16) right imm8 times. */
 /*  C1  /3 RMBoth  ib op16  | Rotate 17 bits (CF, r/m16) right imm8 times. */
int rcr (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /3 RMBoth  ib op16  | Rotate 17 bits (CF, r/m16) right imm8 times. */
 /*  C1  /3 RMBoth  ib op16  | Rotate 17 bits (CF, r/m16) right imm8 times. */
int rcr (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  C0  /3 RMBoth  ib   | Rotate 9 bits (CF, r/m8) right imm8 times. */
 /*  C0  /3 RMBoth  ib   | Rotate 9 bits (CF, r/m8) right imm8 times. */
int rcr (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  C0  /3 RMBoth  ib   | Rotate 9 bits (CF, r/m8) right imm8 times. */
 /*  C0  /3 RMBoth  ib   | Rotate 9 bits (CF, r/m8) right imm8 times. */
int rcr (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  D2  /3 RMBoth     | Rotate 9 bits (CF, r/m8) right CL times. */
 /*  D2  /3 RMBoth     | Rotate 9 bits (CF, r/m8) right CL times. */
int rcr (code_ptr p,
         const ByteReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D2  /3 RMBoth     | Rotate 9 bits (CF, r/m8) right CL times. */
 /*  D2  /3 RMBoth     | Rotate 9 bits (CF, r/m8) right CL times. */
int rcr (code_ptr p,
         const BytePtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /3 RMBoth   op16  | Rotate 17 bits (CF, r/m16) right CL times. */
 /*  D3  /3 RMBoth   op16  | Rotate 17 bits (CF, r/m16) right CL times. */
int rcr (code_ptr p,
         const WordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /3 RMBoth   op16  | Rotate 17 bits (CF, r/m16) right CL times. */
 /*  D3  /3 RMBoth   op16  | Rotate 17 bits (CF, r/m16) right CL times. */
int rcr (code_ptr p,
         const WordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /3 RMBoth   op32  | Rotate 33 bits (CF, r/m32) right CL times. */
 /*  D3  /3 RMBoth   op32  | Rotate 33 bits (CF, r/m32) right CL times. */
int rcr (code_ptr p,
         const DwordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /3 RMBoth   op32  | Rotate 33 bits (CF, r/m32) right CL times. */
 /*  D3  /3 RMBoth   op32  | Rotate 33 bits (CF, r/m32) right CL times. */
int rcr (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  0F 32        | Read MSR specified by ECX into EDX:EAX. */
 /*  0F 32        | Read MSR specified by ECX into EDX:EAX. */
int rdmsr (code_ptr p);
/*  0F 33        | Read performance- monitoring counter specified by ECX into EDX:EAX. */
 /*  0F 33        | Read performance- monitoring counter specified by ECX into EDX:EAX. */
int rdpmc (code_ptr p);
/*  0F 31        | Read time-stamp counter into EDX:EAX. */
 /*  0F 31        | Read time-stamp counter into EDX:EAX. */
int rdtsc (code_ptr p);
/*  0F 01 F9        | Read 64-bit time-stamp counter and 32-bit IA32_TSC_AUX value into EDX:EAX and ECX. */
 /*  0F 01 F9        | Read 64-bit time-stamp counter and 32-bit IA32_TSC_AUX value into EDX:EAX and ECX. */
int rdtscp (code_ptr p);
/* F3 A6        | Find nonmatching bytes in ES:[(E)DI] and DS:[(E)SI]. */
 /* F3 A6        | Find nonmatching bytes in ES:[(E)DI] and DS:[(E)SI]. */
int repe_cmps (code_ptr p,
               const BytePtr_ESI (&   ptr),
               const BytePtr_ES_EDI (&   unused));
/* F3 A6        | Find nonmatching bytes in ES:[(E)DI] and DS:[(E)SI]. */
 /* F3 A6        | Find nonmatching bytes in ES:[(E)DI] and DS:[(E)SI]. */
int repe_cmps (code_ptr p,
               const BytePtr_SI (&   ptr),
               const BytePtr_ES_DI (&   unused));
/* F3 A7      op16  | Find nonmatching words in ES:[(E)DI] and DS:[(E)SI]. */
 /* F3 A7      op16  | Find nonmatching words in ES:[(E)DI] and DS:[(E)SI]. */
int repe_cmps (code_ptr p,
               const WordPtr_ESI (&   ptr),
               const WordPtr_ES_EDI (&   unused));
/* F3 A7      op16  | Find nonmatching words in ES:[(E)DI] and DS:[(E)SI]. */
 /* F3 A7      op16  | Find nonmatching words in ES:[(E)DI] and DS:[(E)SI]. */
int repe_cmps (code_ptr p,
               const WordPtr_SI (&   ptr),
               const WordPtr_ES_DI (&   unused));
/* F3 A7      op32  | Find nonmatching doublewords in ES:[(E)DI] and DS:[(E)SI]. */
 /* F3 A7      op32  | Find nonmatching doublewords in ES:[(E)DI] and DS:[(E)SI]. */
int repe_cmps (code_ptr p,
               const DwordPtr_ESI (&   ptr),
               const DwordPtr_ES_EDI (&   unused));
/* F3 A7      op32  | Find nonmatching doublewords in ES:[(E)DI] and DS:[(E)SI]. */
 /* F3 A7      op32  | Find nonmatching doublewords in ES:[(E)DI] and DS:[(E)SI]. */
int repe_cmps (code_ptr p,
               const DwordPtr_SI (&   ptr),
               const DwordPtr_ES_DI (&   unused));
/* F3 AE        | Find non-AL byte starting at ES:[(E)DI]. */
 /* F3 AE        | Find non-AL byte starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const BytePtr_ES_EDI (&   unused));
/* F3 AE        | Find non-AL byte starting at ES:[(E)DI]. */
 /* F3 AE        | Find non-AL byte starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const RegAL (&   unused1),
               const BytePtr_ES_EDI (&   unused2));
/* F3 AE        | Find non-AL byte starting at ES:[(E)DI]. */
 /* F3 AE        | Find non-AL byte starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const BytePtr_ES_DI (&   unused));
/* F3 AE        | Find non-AL byte starting at ES:[(E)DI]. */
 /* F3 AE        | Find non-AL byte starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const RegAL (&   unused1),
               const BytePtr_ES_DI (&   unused2));
/* F3 AF      op16  | Find non-AX word starting at ES:[(E)DI]. */
 /* F3 AF      op16  | Find non-AX word starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const WordPtr_ES_EDI (&   unused));
/* F3 AF      op16  | Find non-AX word starting at ES:[(E)DI]. */
 /* F3 AF      op16  | Find non-AX word starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const RegAX (&   unused1),
               const WordPtr_ES_EDI (&   unused2));
/* F3 AF      op16  | Find non-AX word starting at ES:[(E)DI]. */
 /* F3 AF      op16  | Find non-AX word starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const WordPtr_ES_DI (&   unused));
/* F3 AF      op16  | Find non-AX word starting at ES:[(E)DI]. */
 /* F3 AF      op16  | Find non-AX word starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const RegAX (&   unused1),
               const WordPtr_ES_DI (&   unused2));
/* F3 AF      op32  | Find non-EAX doubleword starting at ES:[(E)DI]. */
 /* F3 AF      op32  | Find non-EAX doubleword starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const DwordPtr_ES_EDI (&   unused));
/* F3 AF      op32  | Find non-EAX doubleword starting at ES:[(E)DI]. */
 /* F3 AF      op32  | Find non-EAX doubleword starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const RegEAX (&   unused1),
               const DwordPtr_ES_EDI (&   unused2));
/* F3 AF      op32  | Find non-EAX doubleword starting at ES:[(E)DI]. */
 /* F3 AF      op32  | Find non-EAX doubleword starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const DwordPtr_ES_DI (&   unused));
/* F3 AF      op32  | Find non-EAX doubleword starting at ES:[(E)DI]. */
 /* F3 AF      op32  | Find non-EAX doubleword starting at ES:[(E)DI]. */
int repe_scas (code_ptr p,
               const RegEAX (&   unused1),
               const DwordPtr_ES_DI (&   unused2));
/* F2 A6        | Find matching bytes in ES:[(E)DI] and DS:[(E)SI]. */
 /* F2 A6        | Find matching bytes in ES:[(E)DI] and DS:[(E)SI]. */
int repne_cmps (code_ptr p,
                const BytePtr_ESI (&   ptr),
                const BytePtr_ES_EDI (&   unused));
/* F2 A6        | Find matching bytes in ES:[(E)DI] and DS:[(E)SI]. */
 /* F2 A6        | Find matching bytes in ES:[(E)DI] and DS:[(E)SI]. */
int repne_cmps (code_ptr p,
                const BytePtr_SI (&   ptr),
                const BytePtr_ES_DI (&   unused));
/* F2 A7      op16  | Find matching words in ES:[(E)DI] and DS:[(E)SI]. */
 /* F2 A7      op16  | Find matching words in ES:[(E)DI] and DS:[(E)SI]. */
int repne_cmps (code_ptr p,
                const WordPtr_ESI (&   ptr),
                const WordPtr_ES_EDI (&   unused));
/* F2 A7      op16  | Find matching words in ES:[(E)DI] and DS:[(E)SI]. */
 /* F2 A7      op16  | Find matching words in ES:[(E)DI] and DS:[(E)SI]. */
int repne_cmps (code_ptr p,
                const WordPtr_SI (&   ptr),
                const WordPtr_ES_DI (&   unused));
/* F2 A7      op32  | Find matching doublewords in ES:[(E)DI] and DS:[(E)SI]. */
 /* F2 A7      op32  | Find matching doublewords in ES:[(E)DI] and DS:[(E)SI]. */
int repne_cmps (code_ptr p,
                const DwordPtr_ESI (&   ptr),
                const DwordPtr_ES_EDI (&   unused));
/* F2 A7      op32  | Find matching doublewords in ES:[(E)DI] and DS:[(E)SI]. */
 /* F2 A7      op32  | Find matching doublewords in ES:[(E)DI] and DS:[(E)SI]. */
int repne_cmps (code_ptr p,
                const DwordPtr_SI (&   ptr),
                const DwordPtr_ES_DI (&   unused));
/* F2 AE        | Find AL, starting at ES:[(E)DI]. */
 /* F2 AE        | Find AL, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const BytePtr_ES_EDI (&   unused));
/* F2 AE        | Find AL, starting at ES:[(E)DI]. */
 /* F2 AE        | Find AL, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const RegAL (&   unused1),
                const BytePtr_ES_EDI (&   unused2));
/* F2 AE        | Find AL, starting at ES:[(E)DI]. */
 /* F2 AE        | Find AL, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const BytePtr_ES_DI (&   unused));
/* F2 AE        | Find AL, starting at ES:[(E)DI]. */
 /* F2 AE        | Find AL, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const RegAL (&   unused1),
                const BytePtr_ES_DI (&   unused2));
/* F2 AF      op16  | Find AX, starting at ES:[(E)DI]. */
 /* F2 AF      op16  | Find AX, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const WordPtr_ES_EDI (&   unused));
/* F2 AF      op16  | Find AX, starting at ES:[(E)DI]. */
 /* F2 AF      op16  | Find AX, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const RegAX (&   unused1),
                const WordPtr_ES_EDI (&   unused2));
/* F2 AF      op16  | Find AX, starting at ES:[(E)DI]. */
 /* F2 AF      op16  | Find AX, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const WordPtr_ES_DI (&   unused));
/* F2 AF      op16  | Find AX, starting at ES:[(E)DI]. */
 /* F2 AF      op16  | Find AX, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const RegAX (&   unused1),
                const WordPtr_ES_DI (&   unused2));
/* F2 AF      op32  | Find EAX, starting at ES:[(E)DI]. */
 /* F2 AF      op32  | Find EAX, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const DwordPtr_ES_EDI (&   unused));
/* F2 AF      op32  | Find EAX, starting at ES:[(E)DI]. */
 /* F2 AF      op32  | Find EAX, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const RegEAX (&   unused1),
                const DwordPtr_ES_EDI (&   unused2));
/* F2 AF      op32  | Find EAX, starting at ES:[(E)DI]. */
 /* F2 AF      op32  | Find EAX, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const DwordPtr_ES_DI (&   unused));
/* F2 AF      op32  | Find EAX, starting at ES:[(E)DI]. */
 /* F2 AF      op32  | Find EAX, starting at ES:[(E)DI]. */
int repne_scas (code_ptr p,
                const RegEAX (&   unused1),
                const DwordPtr_ES_DI (&   unused2));
/* F3 6C        | Input (E)CX bytes from port DX into ES:[(E)DI]. */
 /* F3 6C        | Input (E)CX bytes from port DX into ES:[(E)DI]. */
int rep_ins (code_ptr p,
             const BytePtr_ES_EDI (&   unused1),
             const RegDX (&   unused2));
/* F3 6C        | Input (E)CX bytes from port DX into ES:[(E)DI]. */
 /* F3 6C        | Input (E)CX bytes from port DX into ES:[(E)DI]. */
int rep_ins (code_ptr p,
             const BytePtr_ES_DI (&   unused1),
             const RegDX (&   unused2));
/* F3 6D      op16  | Input (E)CX words from port DX into ES:[(E)DI.] */
 /* F3 6D      op16  | Input (E)CX words from port DX into ES:[(E)DI.] */
int rep_ins (code_ptr p,
             const WordPtr_ES_EDI (&   unused1),
             const RegDX (&   unused2));
/* F3 6D      op16  | Input (E)CX words from port DX into ES:[(E)DI.] */
 /* F3 6D      op16  | Input (E)CX words from port DX into ES:[(E)DI.] */
int rep_ins (code_ptr p,
             const WordPtr_ES_DI (&   unused1),
             const RegDX (&   unused2));
/* F3 6D      op32  | Input (E)CX doublewords from port DX into ES:[(E)DI]. */
 /* F3 6D      op32  | Input (E)CX doublewords from port DX into ES:[(E)DI]. */
int rep_ins (code_ptr p,
             const DwordPtr_ES_EDI (&   unused1),
             const RegDX (&   unused2));
/* F3 6D      op32  | Input (E)CX doublewords from port DX into ES:[(E)DI]. */
 /* F3 6D      op32  | Input (E)CX doublewords from port DX into ES:[(E)DI]. */
int rep_ins (code_ptr p,
             const DwordPtr_ES_DI (&   unused1),
             const RegDX (&   unused2));
/* F3 AC        | Load (E)CX bytes from DS:[(E)SI] to AL. */
 /* F3 AC        | Load (E)CX bytes from DS:[(E)SI] to AL. */
int rep_lods (code_ptr p,
              const BytePtr_ESI (&   ptr));
/* F3 AC        | Load (E)CX bytes from DS:[(E)SI] to AL. */
 /* F3 AC        | Load (E)CX bytes from DS:[(E)SI] to AL. */
int rep_lods (code_ptr p,
              const RegAL (&   unused),
              const BytePtr_ESI (&   ptr));
/* F3 AC        | Load (E)CX bytes from DS:[(E)SI] to AL. */
 /* F3 AC        | Load (E)CX bytes from DS:[(E)SI] to AL. */
int rep_lods (code_ptr p,
              const BytePtr_SI (&   ptr));
/* F3 AC        | Load (E)CX bytes from DS:[(E)SI] to AL. */
 /* F3 AC        | Load (E)CX bytes from DS:[(E)SI] to AL. */
int rep_lods (code_ptr p,
              const RegAL (&   unused),
              const BytePtr_SI (&   ptr));
/* F3 AD      op16  | Load (E)CX words from DS:[(E)SI] to AX. */
 /* F3 AD      op16  | Load (E)CX words from DS:[(E)SI] to AX. */
int rep_lods (code_ptr p,
              const WordPtr_ESI (&   ptr));
/* F3 AD      op16  | Load (E)CX words from DS:[(E)SI] to AX. */
 /* F3 AD      op16  | Load (E)CX words from DS:[(E)SI] to AX. */
int rep_lods (code_ptr p,
              const RegAX (&   unused),
              const WordPtr_ESI (&   ptr));
/* F3 AD      op16  | Load (E)CX words from DS:[(E)SI] to AX. */
 /* F3 AD      op16  | Load (E)CX words from DS:[(E)SI] to AX. */
int rep_lods (code_ptr p,
              const WordPtr_SI (&   ptr));
/* F3 AD      op16  | Load (E)CX words from DS:[(E)SI] to AX. */
 /* F3 AD      op16  | Load (E)CX words from DS:[(E)SI] to AX. */
int rep_lods (code_ptr p,
              const RegAX (&   unused),
              const WordPtr_SI (&   ptr));
/* F3 AD      op32  | Load (E)CX doublewords from DS:[(E)SI] to EAX. */
 /* F3 AD      op32  | Load (E)CX doublewords from DS:[(E)SI] to EAX. */
int rep_lods (code_ptr p,
              const DwordPtr_ESI (&   ptr));
/* F3 AD      op32  | Load (E)CX doublewords from DS:[(E)SI] to EAX. */
 /* F3 AD      op32  | Load (E)CX doublewords from DS:[(E)SI] to EAX. */
int rep_lods (code_ptr p,
              const RegEAX (&   unused),
              const DwordPtr_ESI (&   ptr));
/* F3 AD      op32  | Load (E)CX doublewords from DS:[(E)SI] to EAX. */
 /* F3 AD      op32  | Load (E)CX doublewords from DS:[(E)SI] to EAX. */
int rep_lods (code_ptr p,
              const DwordPtr_SI (&   ptr));
/* F3 AD      op32  | Load (E)CX doublewords from DS:[(E)SI] to EAX. */
 /* F3 AD      op32  | Load (E)CX doublewords from DS:[(E)SI] to EAX. */
int rep_lods (code_ptr p,
              const RegEAX (&   unused),
              const DwordPtr_SI (&   ptr));
/* F3 A4        | Move (E)CX bytes from DS:[(E)SI] to ES:[(E)DI]. */
 /* F3 A4        | Move (E)CX bytes from DS:[(E)SI] to ES:[(E)DI]. */
int rep_movs (code_ptr p,
              const BytePtr_ES_EDI (&   unused),
              const BytePtr_ESI (&   ptr));
/* F3 A4        | Move (E)CX bytes from DS:[(E)SI] to ES:[(E)DI]. */
 /* F3 A4        | Move (E)CX bytes from DS:[(E)SI] to ES:[(E)DI]. */
int rep_movs (code_ptr p,
              const BytePtr_ES_DI (&   unused),
              const BytePtr_SI (&   ptr));
/* F3 A5      op16  | Move (E)CX words from DS:[(E)SI] to ES:[(E)DI]. */
 /* F3 A5      op16  | Move (E)CX words from DS:[(E)SI] to ES:[(E)DI]. */
int rep_movs (code_ptr p,
              const WordPtr_ES_EDI (&   unused),
              const WordPtr_ESI (&   ptr));
/* F3 A5      op16  | Move (E)CX words from DS:[(E)SI] to ES:[(E)DI]. */
 /* F3 A5      op16  | Move (E)CX words from DS:[(E)SI] to ES:[(E)DI]. */
int rep_movs (code_ptr p,
              const WordPtr_ES_DI (&   unused),
              const WordPtr_SI (&   ptr));
/* F3 A5      op32  | Move (E)CX doublewords from DS:[(E)SI] to ES:[(E)DI]. */
 /* F3 A5      op32  | Move (E)CX doublewords from DS:[(E)SI] to ES:[(E)DI]. */
int rep_movs (code_ptr p,
              const DwordPtr_ES_EDI (&   unused),
              const DwordPtr_ESI (&   ptr));
/* F3 A5      op32  | Move (E)CX doublewords from DS:[(E)SI] to ES:[(E)DI]. */
 /* F3 A5      op32  | Move (E)CX doublewords from DS:[(E)SI] to ES:[(E)DI]. */
int rep_movs (code_ptr p,
              const DwordPtr_ES_DI (&   unused),
              const DwordPtr_SI (&   ptr));
/* F3 6E        | Output (E)CX bytes from DS:[(E)SI] to port DX. */
 /* F3 6E        | Output (E)CX bytes from DS:[(E)SI] to port DX. */
int rep_outs (code_ptr p,
              const RegDX (&   unused),
              const BytePtr_ESI (&   ptr));
/* F3 6E        | Output (E)CX bytes from DS:[(E)SI] to port DX. */
 /* F3 6E        | Output (E)CX bytes from DS:[(E)SI] to port DX. */
int rep_outs (code_ptr p,
              const RegDX (&   unused),
              const BytePtr_SI (&   ptr));
/* F3 6F      op16  | Output (E)CX words from DS:[(E)SI] to port DX. */
 /* F3 6F      op16  | Output (E)CX words from DS:[(E)SI] to port DX. */
int rep_outs (code_ptr p,
              const RegDX (&   unused),
              const WordPtr_ESI (&   ptr));
/* F3 6F      op16  | Output (E)CX words from DS:[(E)SI] to port DX. */
 /* F3 6F      op16  | Output (E)CX words from DS:[(E)SI] to port DX. */
int rep_outs (code_ptr p,
              const RegDX (&   unused),
              const WordPtr_SI (&   ptr));
/* F3 6F      op32  | Output (E)CX doublewords from DS:[(E)SI] to port DX. */
 /* F3 6F      op32  | Output (E)CX doublewords from DS:[(E)SI] to port DX. */
int rep_outs (code_ptr p,
              const RegDX (&   unused),
              const DwordPtr_ESI (&   ptr));
/* F3 6F      op32  | Output (E)CX doublewords from DS:[(E)SI] to port DX. */
 /* F3 6F      op32  | Output (E)CX doublewords from DS:[(E)SI] to port DX. */
int rep_outs (code_ptr p,
              const RegDX (&   unused),
              const DwordPtr_SI (&   ptr));
/* F3 AA        | Fill (E)CX bytes at ES:[(E)DI] with AL. */
 /* F3 AA        | Fill (E)CX bytes at ES:[(E)DI] with AL. */
int rep_stos (code_ptr p,
              const BytePtr_ES_EDI (&   unused));
/* F3 AA        | Fill (E)CX bytes at ES:[(E)DI] with AL. */
 /* F3 AA        | Fill (E)CX bytes at ES:[(E)DI] with AL. */
int rep_stos (code_ptr p,
              const BytePtr_ES_EDI (&   unused1),
              const RegAL (&   unused2));
/* F3 AA        | Fill (E)CX bytes at ES:[(E)DI] with AL. */
 /* F3 AA        | Fill (E)CX bytes at ES:[(E)DI] with AL. */
int rep_stos (code_ptr p,
              const BytePtr_ES_DI (&   unused));
/* F3 AA        | Fill (E)CX bytes at ES:[(E)DI] with AL. */
 /* F3 AA        | Fill (E)CX bytes at ES:[(E)DI] with AL. */
int rep_stos (code_ptr p,
              const BytePtr_ES_DI (&   unused1),
              const RegAL (&   unused2));
/* F3 AB      op16  | Fill (E)CX words at ES:[(E)DI] with AX. */
 /* F3 AB      op16  | Fill (E)CX words at ES:[(E)DI] with AX. */
int rep_stos (code_ptr p,
              const WordPtr_ES_EDI (&   unused));
/* F3 AB      op16  | Fill (E)CX words at ES:[(E)DI] with AX. */
 /* F3 AB      op16  | Fill (E)CX words at ES:[(E)DI] with AX. */
int rep_stos (code_ptr p,
              const WordPtr_ES_EDI (&   unused1),
              const RegAX (&   unused2));
/* F3 AB      op16  | Fill (E)CX words at ES:[(E)DI] with AX. */
 /* F3 AB      op16  | Fill (E)CX words at ES:[(E)DI] with AX. */
int rep_stos (code_ptr p,
              const WordPtr_ES_DI (&   unused));
/* F3 AB      op16  | Fill (E)CX words at ES:[(E)DI] with AX. */
 /* F3 AB      op16  | Fill (E)CX words at ES:[(E)DI] with AX. */
int rep_stos (code_ptr p,
              const WordPtr_ES_DI (&   unused1),
              const RegAX (&   unused2));
/* F3 AB      op32  | Fill (E)CX doublewords at ES:[(E)DI] with EAX. */
 /* F3 AB      op32  | Fill (E)CX doublewords at ES:[(E)DI] with EAX. */
int rep_stos (code_ptr p,
              const DwordPtr_ES_EDI (&   unused));
/* F3 AB      op32  | Fill (E)CX doublewords at ES:[(E)DI] with EAX. */
 /* F3 AB      op32  | Fill (E)CX doublewords at ES:[(E)DI] with EAX. */
int rep_stos (code_ptr p,
              const DwordPtr_ES_EDI (&   unused1),
              const RegEAX (&   unused2));
/* F3 AB      op32  | Fill (E)CX doublewords at ES:[(E)DI] with EAX. */
 /* F3 AB      op32  | Fill (E)CX doublewords at ES:[(E)DI] with EAX. */
int rep_stos (code_ptr p,
              const DwordPtr_ES_DI (&   unused));
/* F3 AB      op32  | Fill (E)CX doublewords at ES:[(E)DI] with EAX. */
 /* F3 AB      op32  | Fill (E)CX doublewords at ES:[(E)DI] with EAX. */
int rep_stos (code_ptr p,
              const DwordPtr_ES_DI (&   unused1),
              const RegEAX (&   unused2));
/*  C3        | Near return to calling procedure. */
 /*  C3        | Near return to calling procedure. */
int ret (code_ptr p);
/*  C2     iw   | Near return to calling procedure and pop imm16 bytes from stack. */
 /*  C2     iw   | Near return to calling procedure and pop imm16 bytes from stack. */
int ret (code_ptr p,
         imm16_t imm);
/*  CB        | Far return to calling procedure. */
 /*  CB        | Far return to calling procedure. */
int retf (code_ptr p);
/*  CA     iw   | Far return to calling procedure and pop imm16 bytes from stack. */
 /*  CA     iw   | Far return to calling procedure and pop imm16 bytes from stack. */
int retf (code_ptr p,
          imm16_t imm);
/*  C1  /0 RMBoth  ib op32  | Rotate 32 bits r/m32 left imm8 times. */
 /*  C1  /0 RMBoth  ib op32  | Rotate 32 bits r/m32 left imm8 times. */
int rol (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /0 RMBoth  ib op32  | Rotate 32 bits r/m32 left imm8 times. */
 /*  C1  /0 RMBoth  ib op32  | Rotate 32 bits r/m32 left imm8 times. */
int rol (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  C1  /0 RMBoth  ib op16  | Rotate 16 bits r/m16 left imm8 times. */
 /*  C1  /0 RMBoth  ib op16  | Rotate 16 bits r/m16 left imm8 times. */
int rol (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /0 RMBoth  ib op16  | Rotate 16 bits r/m16 left imm8 times. */
 /*  C1  /0 RMBoth  ib op16  | Rotate 16 bits r/m16 left imm8 times. */
int rol (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  C0  /0 RMBoth  ib   | Rotate 8 bits r/m8 left imm8 times. */
 /*  C0  /0 RMBoth  ib   | Rotate 8 bits r/m8 left imm8 times. */
int rol (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  C0  /0 RMBoth  ib   | Rotate 8 bits r/m8 left imm8 times. */
 /*  C0  /0 RMBoth  ib   | Rotate 8 bits r/m8 left imm8 times. */
int rol (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  D2  /0 RMBoth     | Rotate 8 bits r/m8 left CL times. */
 /*  D2  /0 RMBoth     | Rotate 8 bits r/m8 left CL times. */
int rol (code_ptr p,
         const ByteReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D2  /0 RMBoth     | Rotate 8 bits r/m8 left CL times. */
 /*  D2  /0 RMBoth     | Rotate 8 bits r/m8 left CL times. */
int rol (code_ptr p,
         const BytePtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /0 RMBoth   op16  | Rotate 16 bits r/m16 left CL times. */
 /*  D3  /0 RMBoth   op16  | Rotate 16 bits r/m16 left CL times. */
int rol (code_ptr p,
         const WordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /0 RMBoth   op16  | Rotate 16 bits r/m16 left CL times. */
 /*  D3  /0 RMBoth   op16  | Rotate 16 bits r/m16 left CL times. */
int rol (code_ptr p,
         const WordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /0 RMBoth   op32  | Rotate 32 bits r/m32 left CL times. */
 /*  D3  /0 RMBoth   op32  | Rotate 32 bits r/m32 left CL times. */
int rol (code_ptr p,
         const DwordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /0 RMBoth   op32  | Rotate 32 bits r/m32 left CL times. */
 /*  D3  /0 RMBoth   op32  | Rotate 32 bits r/m32 left CL times. */
int rol (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  C1  /1 RMBoth  ib op32  | Rotate 32 bits r/m32 right imm8 times. */
 /*  C1  /1 RMBoth  ib op32  | Rotate 32 bits r/m32 right imm8 times. */
int ror (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /1 RMBoth  ib op32  | Rotate 32 bits r/m32 right imm8 times. */
 /*  C1  /1 RMBoth  ib op32  | Rotate 32 bits r/m32 right imm8 times. */
int ror (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  C1  /1 RMBoth  ib op16  | Rotate 16 bits r/m16 right imm8 times. */
 /*  C1  /1 RMBoth  ib op16  | Rotate 16 bits r/m16 right imm8 times. */
int ror (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /1 RMBoth  ib op16  | Rotate 16 bits r/m16 right imm8 times. */
 /*  C1  /1 RMBoth  ib op16  | Rotate 16 bits r/m16 right imm8 times. */
int ror (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  C0  /1 RMBoth  ib   | Rotate 8 bits r/m16 right imm8 times. */
 /*  C0  /1 RMBoth  ib   | Rotate 8 bits r/m16 right imm8 times. */
int ror (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  C0  /1 RMBoth  ib   | Rotate 8 bits r/m16 right imm8 times. */
 /*  C0  /1 RMBoth  ib   | Rotate 8 bits r/m16 right imm8 times. */
int ror (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  D2  /1 RMBoth     | Rotate 8 bits r/m8 right CL times. */
 /*  D2  /1 RMBoth     | Rotate 8 bits r/m8 right CL times. */
int ror (code_ptr p,
         const ByteReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D2  /1 RMBoth     | Rotate 8 bits r/m8 right CL times. */
 /*  D2  /1 RMBoth     | Rotate 8 bits r/m8 right CL times. */
int ror (code_ptr p,
         const BytePtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /1 RMBoth   op16  | Rotate 16 bits r/m16 right CL times. */
 /*  D3  /1 RMBoth   op16  | Rotate 16 bits r/m16 right CL times. */
int ror (code_ptr p,
         const WordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /1 RMBoth   op16  | Rotate 16 bits r/m16 right CL times. */
 /*  D3  /1 RMBoth   op16  | Rotate 16 bits r/m16 right CL times. */
int ror (code_ptr p,
         const WordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /1 RMBoth   op32  | Rotate 32 bits r/m32 right CL times. */
 /*  D3  /1 RMBoth   op32  | Rotate 32 bits r/m32 right CL times. */
int ror (code_ptr p,
         const DwordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /1 RMBoth   op32  | Rotate 32 bits r/m32 right CL times. */
 /*  D3  /1 RMBoth   op32  | Rotate 32 bits r/m32 right CL times. */
int ror (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const RegCL (&   unused));
/* 66 0F 3A 09  /r RMBoth  ib   | Round packed double precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. */
 /* 66 0F 3A 09  /r RMBoth  ib   | Round packed double precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. */
int roundpd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 09  /r RMBoth  ib   | Round packed double precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. */
 /* 66 0F 3A 09  /r RMBoth  ib   | Round packed double precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. */
int roundpd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 08  /r RMBoth  ib   | Round packed single precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. */
 /* 66 0F 3A 08  /r RMBoth  ib   | Round packed single precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. */
int roundps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 08  /r RMBoth  ib   | Round packed single precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. */
 /* 66 0F 3A 08  /r RMBoth  ib   | Round packed single precision floating-point values in xmm2/m128 and place the result in xmm1. The rounding mode is determined by imm8. */
int roundps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0B  /r RMBoth  ib   | Round the low packed double precision floating- point value in xmm2/m64 and place the result in xmm1. The rounding mode is determined by imm8. */
 /* 66 0F 3A 0B  /r RMBoth  ib   | Round the low packed double precision floating- point value in xmm2/m64 and place the result in xmm1. The rounding mode is determined by imm8. */
int roundsd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0B  /r RMBoth  ib   | Round the low packed double precision floating- point value in xmm2/m64 and place the result in xmm1. The rounding mode is determined by imm8. */
 /* 66 0F 3A 0B  /r RMBoth  ib   | Round the low packed double precision floating- point value in xmm2/m64 and place the result in xmm1. The rounding mode is determined by imm8. */
int roundsd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0A  /r RMBoth  ib   | Round the low packed single precision floating-point value in xmm2/m32 and place the result in xmm1. The rounding mode is determined by imm8. */
 /* 66 0F 3A 0A  /r RMBoth  ib   | Round the low packed single precision floating-point value in xmm2/m32 and place the result in xmm1. The rounding mode is determined by imm8. */
int roundss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm),
             imm8_t imm);
/* 66 0F 3A 0A  /r RMBoth  ib   | Round the low packed single precision floating-point value in xmm2/m32 and place the result in xmm1. The rounding mode is determined by imm8. */
 /* 66 0F 3A 0A  /r RMBoth  ib   | Round the low packed single precision floating-point value in xmm2/m32 and place the result in xmm1. The rounding mode is determined by imm8. */
int roundss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm),
             imm8_t imm);
/*  0F AA        | Resume operation of interrupted program. */
 /*  0F AA        | Resume operation of interrupted program. */
int rsm (code_ptr p);
/*  0F 52  /r RMBoth     | Computes the approximate reciprocals of the square roots of the packed single- precision floating-point values in xmm2/m128 and stores the results in xmm1. */
 /*  0F 52  /r RMBoth     | Computes the approximate reciprocals of the square roots of the packed single- precision floating-point values in xmm2/m128 and stores the results in xmm1. */
int rsqrtps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/*  0F 52  /r RMBoth     | Computes the approximate reciprocals of the square roots of the packed single- precision floating-point values in xmm2/m128 and stores the results in xmm1. */
 /*  0F 52  /r RMBoth     | Computes the approximate reciprocals of the square roots of the packed single- precision floating-point values in xmm2/m128 and stores the results in xmm1. */
int rsqrtps (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmWordPtr (&   modrm_rm));
/* F3 0F 52  /r RMBoth     | Computes the approximate reciprocal of the square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. */
 /* F3 0F 52  /r RMBoth     | Computes the approximate reciprocal of the square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. */
int rsqrtss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* F3 0F 52  /r RMBoth     | Computes the approximate reciprocal of the square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. */
 /* F3 0F 52  /r RMBoth     | Computes the approximate reciprocal of the square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. */
int rsqrtss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/*  9E        | Loads SF, ZF, AF, PF, and CF from AH into EFLAGS register. */
 /*  9E        | Loads SF, ZF, AF, PF, and CF from AH into EFLAGS register. */
int sahf (code_ptr p);
/*  C1  /4 RMBoth  ib op32  | Multiply r/m32 by 2, imm8 times. */
 /*  C1  /4 RMBoth  ib op32  | Multiply r/m32 by 2, imm8 times. */
int sal (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /4 RMBoth  ib op32  | Multiply r/m32 by 2, imm8 times. */
 /*  C1  /4 RMBoth  ib op32  | Multiply r/m32 by 2, imm8 times. */
int sal (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  C1  /4 RMBoth  ib op16  | Multiply r/m16 by 2, imm8 times. */
 /*  C1  /4 RMBoth  ib op16  | Multiply r/m16 by 2, imm8 times. */
int sal (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /4 RMBoth  ib op16  | Multiply r/m16 by 2, imm8 times. */
 /*  C1  /4 RMBoth  ib op16  | Multiply r/m16 by 2, imm8 times. */
int sal (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  C0  /4 RMBoth  ib   | Multiply r/m8 by 2, imm8 times. */
 /*  C0  /4 RMBoth  ib   | Multiply r/m8 by 2, imm8 times. */
int sal (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  C0  /4 RMBoth  ib   | Multiply r/m8 by 2, imm8 times. */
 /*  C0  /4 RMBoth  ib   | Multiply r/m8 by 2, imm8 times. */
int sal (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  D2  /4 RMBoth     | Multiply r/m8 by 2, CL times. */
 /*  D2  /4 RMBoth     | Multiply r/m8 by 2, CL times. */
int sal (code_ptr p,
         const ByteReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D2  /4 RMBoth     | Multiply r/m8 by 2, CL times. */
 /*  D2  /4 RMBoth     | Multiply r/m8 by 2, CL times. */
int sal (code_ptr p,
         const BytePtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /4 RMBoth   op16  | Multiply r/m16 by 2, CL times. */
 /*  D3  /4 RMBoth   op16  | Multiply r/m16 by 2, CL times. */
int sal (code_ptr p,
         const WordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /4 RMBoth   op16  | Multiply r/m16 by 2, CL times. */
 /*  D3  /4 RMBoth   op16  | Multiply r/m16 by 2, CL times. */
int sal (code_ptr p,
         const WordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /4 RMBoth   op32  | Multiply r/m32 by 2, CL times. */
 /*  D3  /4 RMBoth   op32  | Multiply r/m32 by 2, CL times. */
int sal (code_ptr p,
         const DwordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /4 RMBoth   op32  | Multiply r/m32 by 2, CL times. */
 /*  D3  /4 RMBoth   op32  | Multiply r/m32 by 2, CL times. */
int sal (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  C1  /7 RMBoth  ib op32  | Signed divide* r/m32 by 2, imm8 times. */
 /*  C1  /7 RMBoth  ib op32  | Signed divide* r/m32 by 2, imm8 times. */
int sar (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /7 RMBoth  ib op32  | Signed divide* r/m32 by 2, imm8 times. */
 /*  C1  /7 RMBoth  ib op32  | Signed divide* r/m32 by 2, imm8 times. */
int sar (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  C1  /7 RMBoth  ib op16  | Signed divide* r/m16 by 2, imm8 times. */
 /*  C1  /7 RMBoth  ib op16  | Signed divide* r/m16 by 2, imm8 times. */
int sar (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /7 RMBoth  ib op16  | Signed divide* r/m16 by 2, imm8 times. */
 /*  C1  /7 RMBoth  ib op16  | Signed divide* r/m16 by 2, imm8 times. */
int sar (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  C0  /7 RMBoth  ib   | Signed divide* r/m8 by 2, imm8 time. */
 /*  C0  /7 RMBoth  ib   | Signed divide* r/m8 by 2, imm8 time. */
int sar (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  C0  /7 RMBoth  ib   | Signed divide* r/m8 by 2, imm8 time. */
 /*  C0  /7 RMBoth  ib   | Signed divide* r/m8 by 2, imm8 time. */
int sar (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  D2  /7 RMBoth     | Signed divide* r/m8 by 2, CL times. */
 /*  D2  /7 RMBoth     | Signed divide* r/m8 by 2, CL times. */
int sar (code_ptr p,
         const ByteReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D2  /7 RMBoth     | Signed divide* r/m8 by 2, CL times. */
 /*  D2  /7 RMBoth     | Signed divide* r/m8 by 2, CL times. */
int sar (code_ptr p,
         const BytePtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /7 RMBoth   op16  | Signed divide* r/m16 by 2, CL times. */
 /*  D3  /7 RMBoth   op16  | Signed divide* r/m16 by 2, CL times. */
int sar (code_ptr p,
         const WordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /7 RMBoth   op16  | Signed divide* r/m16 by 2, CL times. */
 /*  D3  /7 RMBoth   op16  | Signed divide* r/m16 by 2, CL times. */
int sar (code_ptr p,
         const WordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /7 RMBoth   op32  | Signed divide* r/m32 by 2, CL times. */
 /*  D3  /7 RMBoth   op32  | Signed divide* r/m32 by 2, CL times. */
int sar (code_ptr p,
         const DwordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /7 RMBoth   op32  | Signed divide* r/m32 by 2, CL times. */
 /*  D3  /7 RMBoth   op32  | Signed divide* r/m32 by 2, CL times. */
int sar (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  1D     id op32  | Subtract with borrow imm32 from EAX. */
 /*  1D     id op32  | Subtract with borrow imm32 from EAX. */
int sbb (code_ptr p,
         const RegEAX (&   unused),
         imm32_t imm);
/*  83  /3 RMBoth  ib op32  | Subtract with borrow sign- extended imm8 from r/m32. */
 /*  83  /3 RMBoth  ib op32  | Subtract with borrow sign- extended imm8 from r/m32. */
int sbb (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm8_t imm);
/*  81  /3 RMBoth  id op32  | Subtract with borrow imm32 from r/m32. */
 /*  81  /3 RMBoth  id op32  | Subtract with borrow imm32 from r/m32. */
int sbb (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm32_t imm);
/*  81  /3 RMBoth  id op32  | Subtract with borrow imm32 from r/m32. */
 /*  81  /3 RMBoth  id op32  | Subtract with borrow imm32 from r/m32. */
int sbb (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm32_t imm);
/*  83  /3 RMBoth  ib op32  | Subtract with borrow sign- extended imm8 from r/m32. */
 /*  83  /3 RMBoth  ib op32  | Subtract with borrow sign- extended imm8 from r/m32. */
int sbb (code_ptr p,
         const RegEAX (&   modrm_rm),
         imm8_t imm);
/*  83  /3 RMBoth  ib op32  | Subtract with borrow sign- extended imm8 from r/m32. */
 /*  83  /3 RMBoth  ib op32  | Subtract with borrow sign- extended imm8 from r/m32. */
int sbb (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  1D     iw op16  | Subtract with borrow imm16 from AX. */
 /*  1D     iw op16  | Subtract with borrow imm16 from AX. */
int sbb (code_ptr p,
         const RegAX (&   unused),
         imm16_t imm);
/*  83  /3 RMBoth  ib op16  | Subtract with borrow sign- extended imm8 from r/m16. */
 /*  83  /3 RMBoth  ib op16  | Subtract with borrow sign- extended imm8 from r/m16. */
int sbb (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm8_t imm);
/*  81  /3 RMBoth  iw op16  | Subtract with borrow imm16 from r/m16. */
 /*  81  /3 RMBoth  iw op16  | Subtract with borrow imm16 from r/m16. */
int sbb (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm16_t imm);
/*  81  /3 RMBoth  iw op16  | Subtract with borrow imm16 from r/m16. */
 /*  81  /3 RMBoth  iw op16  | Subtract with borrow imm16 from r/m16. */
int sbb (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm16_t imm);
/*  83  /3 RMBoth  ib op16  | Subtract with borrow sign- extended imm8 from r/m16. */
 /*  83  /3 RMBoth  ib op16  | Subtract with borrow sign- extended imm8 from r/m16. */
int sbb (code_ptr p,
         const RegAX (&   modrm_rm),
         imm8_t imm);
/*  83  /3 RMBoth  ib op16  | Subtract with borrow sign- extended imm8 from r/m16. */
 /*  83  /3 RMBoth  ib op16  | Subtract with borrow sign- extended imm8 from r/m16. */
int sbb (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  1B  /r RMBoth   op32  | Subtract with borrow r/m32 from r32. */
 /*  1B  /r RMBoth   op32  | Subtract with borrow r/m32 from r32. */
int sbb (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  1B  /r RMBoth   op32  | Subtract with borrow r/m32 from r32. */
 /*  1B  /r RMBoth   op32  | Subtract with borrow r/m32 from r32. */
int sbb (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  19  /r RMBoth   op32  | Subtract with borrow r32 from r/m32. */
 /*  19  /r RMBoth   op32  | Subtract with borrow r32 from r/m32. */
int sbb (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  1B  /r RMBoth   op16  | Subtract with borrow r/m16 from r16. */
 /*  1B  /r RMBoth   op16  | Subtract with borrow r/m16 from r16. */
int sbb (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  1B  /r RMBoth   op16  | Subtract with borrow r/m16 from r16. */
 /*  1B  /r RMBoth   op16  | Subtract with borrow r/m16 from r16. */
int sbb (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  19  /r RMBoth   op16  | Subtract with borrow r16 from r/m16. */
 /*  19  /r RMBoth   op16  | Subtract with borrow r16 from r/m16. */
int sbb (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  1A  /r RMBoth     | Subtract with borrow r/m8 from r8. */
 /*  1A  /r RMBoth     | Subtract with borrow r/m8 from r8. */
int sbb (code_ptr p,
         const ByteReg (&   modrm_reg),
         const ByteReg (&   modrm_rm));
/*  1A  /r RMBoth     | Subtract with borrow r/m8 from r8. */
 /*  1A  /r RMBoth     | Subtract with borrow r/m8 from r8. */
int sbb (code_ptr p,
         const ByteReg (&   modrm_reg),
         const BytePtr (&   modrm_rm));
/*  18  /r RMBoth     | Subtract with borrow r8 from r/m8. */
 /*  18  /r RMBoth     | Subtract with borrow r8 from r/m8. */
int sbb (code_ptr p,
         const BytePtr (&   modrm_rm),
         const ByteReg (&   modrm_reg));
/*  1C     ib   | Subtract with borrow imm8 from AL. */
 /*  1C     ib   | Subtract with borrow imm8 from AL. */
int sbb (code_ptr p,
         const RegAL (&   unused),
         imm8_t imm);
/*  80  /3 RMBoth  ib   | Subtract with borrow imm8 from r/m8. */
 /*  80  /3 RMBoth  ib   | Subtract with borrow imm8 from r/m8. */
int sbb (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  80  /3 RMBoth  ib   | Subtract with borrow imm8 from r/m8. */
 /*  80  /3 RMBoth  ib   | Subtract with borrow imm8 from r/m8. */
int sbb (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  AE        | Compare AL with byte at ES:(E)DI or RDI, then set status flags.* */
 /*  AE        | Compare AL with byte at ES:(E)DI or RDI, then set status flags.* */
int scas (code_ptr p,
          const BytePtr_ES_EDI (&   unused));
/*  AE        | Compare AL with byte at ES:(E)DI or RDI, then set status flags.* */
 /*  AE        | Compare AL with byte at ES:(E)DI or RDI, then set status flags.* */
int scas (code_ptr p,
          const RegAL (&   unused1),
          const BytePtr_ES_EDI (&   unused2));
/*  AE        | Compare AL with byte at ES:(E)DI or RDI, then set status flags.* */
 /*  AE        | Compare AL with byte at ES:(E)DI or RDI, then set status flags.* */
int scas (code_ptr p,
          const BytePtr_ES_DI (&   unused));
/*  AE        | Compare AL with byte at ES:(E)DI or RDI, then set status flags.* */
 /*  AE        | Compare AL with byte at ES:(E)DI or RDI, then set status flags.* */
int scas (code_ptr p,
          const RegAL (&   unused1),
          const BytePtr_ES_DI (&   unused2));
/*  AF      op16  | Compare AX with word at ES:(E)DI or RDI, then set status flags.* */
 /*  AF      op16  | Compare AX with word at ES:(E)DI or RDI, then set status flags.* */
int scas (code_ptr p,
          const WordPtr_ES_EDI (&   unused));
/*  AF      op16  | Compare AX with word at ES:(E)DI or RDI, then set status flags.* */
 /*  AF      op16  | Compare AX with word at ES:(E)DI or RDI, then set status flags.* */
int scas (code_ptr p,
          const RegAX (&   unused1),
          const WordPtr_ES_EDI (&   unused2));
/*  AF      op16  | Compare AX with word at ES:(E)DI or RDI, then set status flags.* */
 /*  AF      op16  | Compare AX with word at ES:(E)DI or RDI, then set status flags.* */
int scas (code_ptr p,
          const WordPtr_ES_DI (&   unused));
/*  AF      op16  | Compare AX with word at ES:(E)DI or RDI, then set status flags.* */
 /*  AF      op16  | Compare AX with word at ES:(E)DI or RDI, then set status flags.* */
int scas (code_ptr p,
          const RegAX (&   unused1),
          const WordPtr_ES_DI (&   unused2));
/*  AF      op32  | Compare EAX with doubleword at ES(E)DI or RDI then set status flags.* */
 /*  AF      op32  | Compare EAX with doubleword at ES(E)DI or RDI then set status flags.* */
int scas (code_ptr p,
          const DwordPtr_ES_EDI (&   unused));
/*  AF      op32  | Compare EAX with doubleword at ES(E)DI or RDI then set status flags.* */
 /*  AF      op32  | Compare EAX with doubleword at ES(E)DI or RDI then set status flags.* */
int scas (code_ptr p,
          const RegEAX (&   unused1),
          const DwordPtr_ES_EDI (&   unused2));
/*  AF      op32  | Compare EAX with doubleword at ES(E)DI or RDI then set status flags.* */
 /*  AF      op32  | Compare EAX with doubleword at ES(E)DI or RDI then set status flags.* */
int scas (code_ptr p,
          const DwordPtr_ES_DI (&   unused));
/*  AF      op32  | Compare EAX with doubleword at ES(E)DI or RDI then set status flags.* */
 /*  AF      op32  | Compare EAX with doubleword at ES(E)DI or RDI then set status flags.* */
int scas (code_ptr p,
          const RegEAX (&   unused1),
          const DwordPtr_ES_DI (&   unused2));
/*  AE        | Compare AL with byte at ES:(E)DI or RDI then set status flags.* */
 /*  AE        | Compare AL with byte at ES:(E)DI or RDI then set status flags.* */
int scasb (code_ptr p);
/*  AF      op32  | Compare EAX with doubleword at ES:(E)DI or RDI then set status flags.* */
 /*  AF      op32  | Compare EAX with doubleword at ES:(E)DI or RDI then set status flags.* */
int scasd (code_ptr p);
/*  AF      op16  | Compare AX with word at ES:(E)DI or RDI then set status flags.* */
 /*  AF      op16  | Compare AX with word at ES:(E)DI or RDI then set status flags.* */
int scasw (code_ptr p);
/*  0F 97  /r RMBoth     | Set byte if above (CF=0 and ZF=0). */
 /*  0F 97  /r RMBoth     | Set byte if above (CF=0 and ZF=0). */
int seta (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 97  /r RMBoth     | Set byte if above (CF=0 and ZF=0). */
 /*  0F 97  /r RMBoth     | Set byte if above (CF=0 and ZF=0). */
int seta (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 93  /r RMBoth     | Set byte if above or equal (CF=0). */
 /*  0F 93  /r RMBoth     | Set byte if above or equal (CF=0). */
int setae (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 93  /r RMBoth     | Set byte if above or equal (CF=0). */
 /*  0F 93  /r RMBoth     | Set byte if above or equal (CF=0). */
int setae (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 92  /r RMBoth     | Set byte if below (CF=1). */
 /*  0F 92  /r RMBoth     | Set byte if below (CF=1). */
int setb (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 92  /r RMBoth     | Set byte if below (CF=1). */
 /*  0F 92  /r RMBoth     | Set byte if below (CF=1). */
int setb (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 96  /r RMBoth     | Set byte if below or equal (CF=1 or ZF=1). */
 /*  0F 96  /r RMBoth     | Set byte if below or equal (CF=1 or ZF=1). */
int setbe (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 96  /r RMBoth     | Set byte if below or equal (CF=1 or ZF=1). */
 /*  0F 96  /r RMBoth     | Set byte if below or equal (CF=1 or ZF=1). */
int setbe (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 92  /r RMBoth     | Set byte if carry (CF=1). */
 /*  0F 92  /r RMBoth     | Set byte if carry (CF=1). */
int setc (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 92  /r RMBoth     | Set byte if carry (CF=1). */
 /*  0F 92  /r RMBoth     | Set byte if carry (CF=1). */
int setc (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 94  /r RMBoth     | Set byte if equal (ZF=1). */
 /*  0F 94  /r RMBoth     | Set byte if equal (ZF=1). */
int sete (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 94  /r RMBoth     | Set byte if equal (ZF=1). */
 /*  0F 94  /r RMBoth     | Set byte if equal (ZF=1). */
int sete (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 9F  /r RMBoth     | Set byte if greater (ZF=0 and SF=OF). */
 /*  0F 9F  /r RMBoth     | Set byte if greater (ZF=0 and SF=OF). */
int setg (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 9F  /r RMBoth     | Set byte if greater (ZF=0 and SF=OF). */
 /*  0F 9F  /r RMBoth     | Set byte if greater (ZF=0 and SF=OF). */
int setg (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 9D  /r RMBoth     | Set byte if greater or equal (SF=OF). */
 /*  0F 9D  /r RMBoth     | Set byte if greater or equal (SF=OF). */
int setge (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 9D  /r RMBoth     | Set byte if greater or equal (SF=OF). */
 /*  0F 9D  /r RMBoth     | Set byte if greater or equal (SF=OF). */
int setge (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 9C  /r RMBoth     | Set byte if less (SF≠ OF). */
 /*  0F 9C  /r RMBoth     | Set byte if less (SF≠ OF). */
int setl (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 9C  /r RMBoth     | Set byte if less (SF≠ OF). */
 /*  0F 9C  /r RMBoth     | Set byte if less (SF≠ OF). */
int setl (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 9E  /r RMBoth     | Set byte if less or equal (ZF=1 or SF≠ OF). */
 /*  0F 9E  /r RMBoth     | Set byte if less or equal (ZF=1 or SF≠ OF). */
int setle (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 9E  /r RMBoth     | Set byte if less or equal (ZF=1 or SF≠ OF). */
 /*  0F 9E  /r RMBoth     | Set byte if less or equal (ZF=1 or SF≠ OF). */
int setle (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 96  /r RMBoth     | Set byte if not above (CF=1 or ZF=1). */
 /*  0F 96  /r RMBoth     | Set byte if not above (CF=1 or ZF=1). */
int setna (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 96  /r RMBoth     | Set byte if not above (CF=1 or ZF=1). */
 /*  0F 96  /r RMBoth     | Set byte if not above (CF=1 or ZF=1). */
int setna (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 92  /r RMBoth     | Set byte if not above or equal (CF=1). */
 /*  0F 92  /r RMBoth     | Set byte if not above or equal (CF=1). */
int setnae (code_ptr p,
            const ByteReg (&   modrm_rm));
/*  0F 92  /r RMBoth     | Set byte if not above or equal (CF=1). */
 /*  0F 92  /r RMBoth     | Set byte if not above or equal (CF=1). */
int setnae (code_ptr p,
            const BytePtr (&   modrm_rm));
/*  0F 93  /r RMBoth     | Set byte if not below (CF=0). */
 /*  0F 93  /r RMBoth     | Set byte if not below (CF=0). */
int setnb (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 93  /r RMBoth     | Set byte if not below (CF=0). */
 /*  0F 93  /r RMBoth     | Set byte if not below (CF=0). */
int setnb (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 97  /r RMBoth     | Set byte if not below or equal (CF=0 and ZF=0). */
 /*  0F 97  /r RMBoth     | Set byte if not below or equal (CF=0 and ZF=0). */
int setnbe (code_ptr p,
            const ByteReg (&   modrm_rm));
/*  0F 97  /r RMBoth     | Set byte if not below or equal (CF=0 and ZF=0). */
 /*  0F 97  /r RMBoth     | Set byte if not below or equal (CF=0 and ZF=0). */
int setnbe (code_ptr p,
            const BytePtr (&   modrm_rm));
/*  0F 93  /r RMBoth     | Set byte if not carry (CF=0). */
 /*  0F 93  /r RMBoth     | Set byte if not carry (CF=0). */
int setnc (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 93  /r RMBoth     | Set byte if not carry (CF=0). */
 /*  0F 93  /r RMBoth     | Set byte if not carry (CF=0). */
int setnc (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 95  /r RMBoth     | Set byte if not equal (ZF=0). */
 /*  0F 95  /r RMBoth     | Set byte if not equal (ZF=0). */
int setne (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 95  /r RMBoth     | Set byte if not equal (ZF=0). */
 /*  0F 95  /r RMBoth     | Set byte if not equal (ZF=0). */
int setne (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 9E  /r RMBoth     | Set byte if not greater (ZF=1 or SF≠ OF) */
 /*  0F 9E  /r RMBoth     | Set byte if not greater (ZF=1 or SF≠ OF) */
int setng (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 9E  /r RMBoth     | Set byte if not greater (ZF=1 or SF≠ OF) */
 /*  0F 9E  /r RMBoth     | Set byte if not greater (ZF=1 or SF≠ OF) */
int setng (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 9C  /r RMBoth     | Set byte if not greater or equal (SF≠ OF). */
 /*  0F 9C  /r RMBoth     | Set byte if not greater or equal (SF≠ OF). */
int setnge (code_ptr p,
            const ByteReg (&   modrm_rm));
/*  0F 9C  /r RMBoth     | Set byte if not greater or equal (SF≠ OF). */
 /*  0F 9C  /r RMBoth     | Set byte if not greater or equal (SF≠ OF). */
int setnge (code_ptr p,
            const BytePtr (&   modrm_rm));
/*  0F 9D  /r RMBoth     | Set byte if not less (SF=OF). */
 /*  0F 9D  /r RMBoth     | Set byte if not less (SF=OF). */
int setnl (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 9D  /r RMBoth     | Set byte if not less (SF=OF). */
 /*  0F 9D  /r RMBoth     | Set byte if not less (SF=OF). */
int setnl (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 9F  /r RMBoth     | Set byte if not less or equal (ZF=0 and SF=OF). */
 /*  0F 9F  /r RMBoth     | Set byte if not less or equal (ZF=0 and SF=OF). */
int setnle (code_ptr p,
            const ByteReg (&   modrm_rm));
/*  0F 9F  /r RMBoth     | Set byte if not less or equal (ZF=0 and SF=OF). */
 /*  0F 9F  /r RMBoth     | Set byte if not less or equal (ZF=0 and SF=OF). */
int setnle (code_ptr p,
            const BytePtr (&   modrm_rm));
/*  0F 91  /r RMBoth     | Set byte if not overflow (OF=0). */
 /*  0F 91  /r RMBoth     | Set byte if not overflow (OF=0). */
int setno (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 91  /r RMBoth     | Set byte if not overflow (OF=0). */
 /*  0F 91  /r RMBoth     | Set byte if not overflow (OF=0). */
int setno (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 9B  /r RMBoth     | Set byte if not parity (PF=0). */
 /*  0F 9B  /r RMBoth     | Set byte if not parity (PF=0). */
int setnp (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 9B  /r RMBoth     | Set byte if not parity (PF=0). */
 /*  0F 9B  /r RMBoth     | Set byte if not parity (PF=0). */
int setnp (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 99  /r RMBoth     | Set byte if not sign (SF=0). */
 /*  0F 99  /r RMBoth     | Set byte if not sign (SF=0). */
int setns (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 99  /r RMBoth     | Set byte if not sign (SF=0). */
 /*  0F 99  /r RMBoth     | Set byte if not sign (SF=0). */
int setns (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 95  /r RMBoth     | Set byte if not zero (ZF=0). */
 /*  0F 95  /r RMBoth     | Set byte if not zero (ZF=0). */
int setnz (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 95  /r RMBoth     | Set byte if not zero (ZF=0). */
 /*  0F 95  /r RMBoth     | Set byte if not zero (ZF=0). */
int setnz (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 90  /r RMBoth     | Set byte if overflow (OF=1) */
 /*  0F 90  /r RMBoth     | Set byte if overflow (OF=1) */
int seto (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 90  /r RMBoth     | Set byte if overflow (OF=1) */
 /*  0F 90  /r RMBoth     | Set byte if overflow (OF=1) */
int seto (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 9A  /r RMBoth     | Set byte if parity (PF=1). */
 /*  0F 9A  /r RMBoth     | Set byte if parity (PF=1). */
int setp (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 9A  /r RMBoth     | Set byte if parity (PF=1). */
 /*  0F 9A  /r RMBoth     | Set byte if parity (PF=1). */
int setp (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 9A  /r RMBoth     | Set byte if parity even (PF=1). */
 /*  0F 9A  /r RMBoth     | Set byte if parity even (PF=1). */
int setpe (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 9A  /r RMBoth     | Set byte if parity even (PF=1). */
 /*  0F 9A  /r RMBoth     | Set byte if parity even (PF=1). */
int setpe (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 9B  /r RMBoth     | Set byte if parity odd (PF=0). */
 /*  0F 9B  /r RMBoth     | Set byte if parity odd (PF=0). */
int setpo (code_ptr p,
           const ByteReg (&   modrm_rm));
/*  0F 9B  /r RMBoth     | Set byte if parity odd (PF=0). */
 /*  0F 9B  /r RMBoth     | Set byte if parity odd (PF=0). */
int setpo (code_ptr p,
           const BytePtr (&   modrm_rm));
/*  0F 98  /r RMBoth     | Set byte if sign (SF=1). */
 /*  0F 98  /r RMBoth     | Set byte if sign (SF=1). */
int sets (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 98  /r RMBoth     | Set byte if sign (SF=1). */
 /*  0F 98  /r RMBoth     | Set byte if sign (SF=1). */
int sets (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F 94  /r RMBoth     | Set byte if zero (ZF=1). */
 /*  0F 94  /r RMBoth     | Set byte if zero (ZF=1). */
int setz (code_ptr p,
          const ByteReg (&   modrm_rm));
/*  0F 94  /r RMBoth     | Set byte if zero (ZF=1). */
 /*  0F 94  /r RMBoth     | Set byte if zero (ZF=1). */
int setz (code_ptr p,
          const BytePtr (&   modrm_rm));
/*  0F AE  /7 RMRegOnly     | Serializes store operations. */
 /*  0F AE  /7 RMRegOnly     | Serializes store operations. */
int sfence (code_ptr p);
/*  0F 01  /0 RMMemOnly     | Store GDTR to m. */
 /*  0F 01  /0 RMMemOnly     | Store GDTR to m. */
int sgdt (code_ptr p,
          const VoidPtr (&   modrm_rm));
/*  C1  /4 RMBoth  ib op32  | Multiply r/m32 by 2, imm8 times. */
 /*  C1  /4 RMBoth  ib op32  | Multiply r/m32 by 2, imm8 times. */
int shl (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /4 RMBoth  ib op32  | Multiply r/m32 by 2, imm8 times. */
 /*  C1  /4 RMBoth  ib op32  | Multiply r/m32 by 2, imm8 times. */
int shl (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  C1  /4 RMBoth  ib op16  | Multiply r/m16 by 2, imm8 times. */
 /*  C1  /4 RMBoth  ib op16  | Multiply r/m16 by 2, imm8 times. */
int shl (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /4 RMBoth  ib op16  | Multiply r/m16 by 2, imm8 times. */
 /*  C1  /4 RMBoth  ib op16  | Multiply r/m16 by 2, imm8 times. */
int shl (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  C0  /4 RMBoth  ib   | Multiply r/m8 by 2, imm8 times. */
 /*  C0  /4 RMBoth  ib   | Multiply r/m8 by 2, imm8 times. */
int shl (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  C0  /4 RMBoth  ib   | Multiply r/m8 by 2, imm8 times. */
 /*  C0  /4 RMBoth  ib   | Multiply r/m8 by 2, imm8 times. */
int shl (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  D2  /4 RMBoth     | Multiply r/m8 by 2, CL times. */
 /*  D2  /4 RMBoth     | Multiply r/m8 by 2, CL times. */
int shl (code_ptr p,
         const ByteReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D2  /4 RMBoth     | Multiply r/m8 by 2, CL times. */
 /*  D2  /4 RMBoth     | Multiply r/m8 by 2, CL times. */
int shl (code_ptr p,
         const BytePtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /4 RMBoth   op16  | Multiply r/m16 by 2, CL times. */
 /*  D3  /4 RMBoth   op16  | Multiply r/m16 by 2, CL times. */
int shl (code_ptr p,
         const WordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /4 RMBoth   op16  | Multiply r/m16 by 2, CL times. */
 /*  D3  /4 RMBoth   op16  | Multiply r/m16 by 2, CL times. */
int shl (code_ptr p,
         const WordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /4 RMBoth   op32  | Multiply r/m32 by 2, CL times. */
 /*  D3  /4 RMBoth   op32  | Multiply r/m32 by 2, CL times. */
int shl (code_ptr p,
         const DwordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /4 RMBoth   op32  | Multiply r/m32 by 2, CL times. */
 /*  D3  /4 RMBoth   op32  | Multiply r/m32 by 2, CL times. */
int shl (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  0F A4  /r RMBoth  ib op16  | Shift r/m16 to left imm8 places while shifting bits from r16 in from the right. */
 /*  0F A4  /r RMBoth  ib op16  | Shift r/m16 to left imm8 places while shifting bits from r16 in from the right. */
int shld (code_ptr p,
          const WordReg (&   modrm_rm),
          const WordReg (&   modrm_reg),
          imm8_t imm);
/*  0F A4  /r RMBoth  ib op16  | Shift r/m16 to left imm8 places while shifting bits from r16 in from the right. */
 /*  0F A4  /r RMBoth  ib op16  | Shift r/m16 to left imm8 places while shifting bits from r16 in from the right. */
int shld (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg),
          imm8_t imm);
/*  0F A5  /r RMBoth   op16  | Shift r/m16 to left CL places while shifting bits from r16 in from the right. */
 /*  0F A5  /r RMBoth   op16  | Shift r/m16 to left CL places while shifting bits from r16 in from the right. */
int shld (code_ptr p,
          const WordReg (&   modrm_rm),
          const WordReg (&   modrm_reg),
          const RegCL (&   unused));
/*  0F A5  /r RMBoth   op16  | Shift r/m16 to left CL places while shifting bits from r16 in from the right. */
 /*  0F A5  /r RMBoth   op16  | Shift r/m16 to left CL places while shifting bits from r16 in from the right. */
int shld (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg),
          const RegCL (&   unused));
/*  0F A4  /r RMBoth  ib op32  | Shift r/m32 to left imm8 places while shifting bits from r32 in from the right. */
 /*  0F A4  /r RMBoth  ib op32  | Shift r/m32 to left imm8 places while shifting bits from r32 in from the right. */
int shld (code_ptr p,
          const DwordReg (&   modrm_rm),
          const DwordReg (&   modrm_reg),
          imm8_t imm);
/*  0F A4  /r RMBoth  ib op32  | Shift r/m32 to left imm8 places while shifting bits from r32 in from the right. */
 /*  0F A4  /r RMBoth  ib op32  | Shift r/m32 to left imm8 places while shifting bits from r32 in from the right. */
int shld (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg),
          imm8_t imm);
/*  0F A5  /r RMBoth   op32  | Shift r/m32 to left CL places while shifting bits from r32 in from the right. */
 /*  0F A5  /r RMBoth   op32  | Shift r/m32 to left CL places while shifting bits from r32 in from the right. */
int shld (code_ptr p,
          const DwordReg (&   modrm_rm),
          const DwordReg (&   modrm_reg),
          const RegCL (&   unused));
/*  0F A5  /r RMBoth   op32  | Shift r/m32 to left CL places while shifting bits from r32 in from the right. */
 /*  0F A5  /r RMBoth   op32  | Shift r/m32 to left CL places while shifting bits from r32 in from the right. */
int shld (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg),
          const RegCL (&   unused));
/*  C1  /5 RMBoth  ib op32  | Unsigned divide r/m32 by 2, imm8 times. */
 /*  C1  /5 RMBoth  ib op32  | Unsigned divide r/m32 by 2, imm8 times. */
int shr (code_ptr p,
         const DwordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /5 RMBoth  ib op32  | Unsigned divide r/m32 by 2, imm8 times. */
 /*  C1  /5 RMBoth  ib op32  | Unsigned divide r/m32 by 2, imm8 times. */
int shr (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  C1  /5 RMBoth  ib op16  | Unsigned divide r/m16 by 2, imm8 times. */
 /*  C1  /5 RMBoth  ib op16  | Unsigned divide r/m16 by 2, imm8 times. */
int shr (code_ptr p,
         const WordReg (&   modrm_rm),
         imm8_t imm);
/*  C1  /5 RMBoth  ib op16  | Unsigned divide r/m16 by 2, imm8 times. */
 /*  C1  /5 RMBoth  ib op16  | Unsigned divide r/m16 by 2, imm8 times. */
int shr (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  C0  /5 RMBoth  ib   | Unsigned divide r/m8 by 2, imm8 times. */
 /*  C0  /5 RMBoth  ib   | Unsigned divide r/m8 by 2, imm8 times. */
int shr (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  C0  /5 RMBoth  ib   | Unsigned divide r/m8 by 2, imm8 times. */
 /*  C0  /5 RMBoth  ib   | Unsigned divide r/m8 by 2, imm8 times. */
int shr (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/*  D2  /5 RMBoth     | Unsigned divide r/m8 by 2, CL times. */
 /*  D2  /5 RMBoth     | Unsigned divide r/m8 by 2, CL times. */
int shr (code_ptr p,
         const ByteReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D2  /5 RMBoth     | Unsigned divide r/m8 by 2, CL times. */
 /*  D2  /5 RMBoth     | Unsigned divide r/m8 by 2, CL times. */
int shr (code_ptr p,
         const BytePtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /5 RMBoth   op16  | Unsigned divide r/m16 by 2, CL times */
 /*  D3  /5 RMBoth   op16  | Unsigned divide r/m16 by 2, CL times */
int shr (code_ptr p,
         const WordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /5 RMBoth   op16  | Unsigned divide r/m16 by 2, CL times */
 /*  D3  /5 RMBoth   op16  | Unsigned divide r/m16 by 2, CL times */
int shr (code_ptr p,
         const WordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /5 RMBoth   op32  | Unsigned divide r/m32 by 2, CL times. */
 /*  D3  /5 RMBoth   op32  | Unsigned divide r/m32 by 2, CL times. */
int shr (code_ptr p,
         const DwordReg (&   modrm_rm),
         const RegCL (&   unused));
/*  D3  /5 RMBoth   op32  | Unsigned divide r/m32 by 2, CL times. */
 /*  D3  /5 RMBoth   op32  | Unsigned divide r/m32 by 2, CL times. */
int shr (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const RegCL (&   unused));
/*  0F AC  /r RMBoth  ib op16  | Shift r/m16 to right imm8 places while shifting bits from r16 in from the left. */
 /*  0F AC  /r RMBoth  ib op16  | Shift r/m16 to right imm8 places while shifting bits from r16 in from the left. */
int shrd (code_ptr p,
          const WordReg (&   modrm_rm),
          const WordReg (&   modrm_reg),
          imm8_t imm);
/*  0F AC  /r RMBoth  ib op16  | Shift r/m16 to right imm8 places while shifting bits from r16 in from the left. */
 /*  0F AC  /r RMBoth  ib op16  | Shift r/m16 to right imm8 places while shifting bits from r16 in from the left. */
int shrd (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg),
          imm8_t imm);
/*  0F AD  /r RMBoth   op16  | Shift r/m16 to right CL places while shifting bits from r16 in from the left. */
 /*  0F AD  /r RMBoth   op16  | Shift r/m16 to right CL places while shifting bits from r16 in from the left. */
int shrd (code_ptr p,
          const WordReg (&   modrm_rm),
          const WordReg (&   modrm_reg),
          const RegCL (&   unused));
/*  0F AD  /r RMBoth   op16  | Shift r/m16 to right CL places while shifting bits from r16 in from the left. */
 /*  0F AD  /r RMBoth   op16  | Shift r/m16 to right CL places while shifting bits from r16 in from the left. */
int shrd (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg),
          const RegCL (&   unused));
/*  0F AC  /r RMBoth  ib op32  | Shift r/m32 to right imm8 places while shifting bits from r32 in from the left. */
 /*  0F AC  /r RMBoth  ib op32  | Shift r/m32 to right imm8 places while shifting bits from r32 in from the left. */
int shrd (code_ptr p,
          const DwordReg (&   modrm_rm),
          const DwordReg (&   modrm_reg),
          imm8_t imm);
/*  0F AC  /r RMBoth  ib op32  | Shift r/m32 to right imm8 places while shifting bits from r32 in from the left. */
 /*  0F AC  /r RMBoth  ib op32  | Shift r/m32 to right imm8 places while shifting bits from r32 in from the left. */
int shrd (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg),
          imm8_t imm);
/*  0F AD  /r RMBoth   op32  | Shift r/m32 to right CL places while shifting bits from r32 in from the left. */
 /*  0F AD  /r RMBoth   op32  | Shift r/m32 to right CL places while shifting bits from r32 in from the left. */
int shrd (code_ptr p,
          const DwordReg (&   modrm_rm),
          const DwordReg (&   modrm_reg),
          const RegCL (&   unused));
/*  0F AD  /r RMBoth   op32  | Shift r/m32 to right CL places while shifting bits from r32 in from the left. */
 /*  0F AD  /r RMBoth   op32  | Shift r/m32 to right CL places while shifting bits from r32 in from the left. */
int shrd (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg),
          const RegCL (&   unused));
/* 66 0F C6  /r RMBoth  ib   | Shuffle packed double- precision floating-point values selected by imm8 from xmm1 and xmm2/m128 to xmm1. */
 /* 66 0F C6  /r RMBoth  ib   | Shuffle packed double- precision floating-point values selected by imm8 from xmm1 and xmm2/m128 to xmm1. */
int shufpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm),
            imm8_t imm);
/* 66 0F C6  /r RMBoth  ib   | Shuffle packed double- precision floating-point values selected by imm8 from xmm1 and xmm2/m128 to xmm1. */
 /* 66 0F C6  /r RMBoth  ib   | Shuffle packed double- precision floating-point values selected by imm8 from xmm1 and xmm2/m128 to xmm1. */
int shufpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm),
            imm8_t imm);
/*  0F C6  /r RMBoth  ib   | Shuffle packed single- precision floating-point values selected by imm8 from xmm1 and xmm1/m128 to xmm1. */
 /*  0F C6  /r RMBoth  ib   | Shuffle packed single- precision floating-point values selected by imm8 from xmm1 and xmm1/m128 to xmm1. */
int shufps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm),
            imm8_t imm);
/*  0F C6  /r RMBoth  ib   | Shuffle packed single- precision floating-point values selected by imm8 from xmm1 and xmm1/m128 to xmm1. */
 /*  0F C6  /r RMBoth  ib   | Shuffle packed single- precision floating-point values selected by imm8 from xmm1 and xmm1/m128 to xmm1. */
int shufps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm),
            imm8_t imm);
/*  0F 01  /1 RMMemOnly     | Store IDTR to m. */
 /*  0F 01  /1 RMMemOnly     | Store IDTR to m. */
int sidt (code_ptr p,
          const VoidPtr (&   modrm_rm));
/*  0F 00  /0 RMBoth     | Stores segment selector from LDTR in r/m16. */
 /*  0F 00  /0 RMBoth     | Stores segment selector from LDTR in r/m16. */
int sldt (code_ptr p,
          const WordReg (&   modrm_rm));
/*  0F 00  /0 RMBoth     | Stores segment selector from LDTR in r/m16. */
 /*  0F 00  /0 RMBoth     | Stores segment selector from LDTR in r/m16. */
int sldt (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  0F 01  /4 RMBoth   op16  | Store machine status word to r/m16. */
 /*  0F 01  /4 RMBoth   op16  | Store machine status word to r/m16. */
int smsw (code_ptr p,
          const WordReg (&   modrm_rm));
/*  0F 01  /4 RMBoth   op32  | Store machine status word in low-order 16 bits of r32/m16; high-order 16 bits of r32 are undefined. */
 /*  0F 01  /4 RMBoth   op32  | Store machine status word in low-order 16 bits of r32/m16; high-order 16 bits of r32 are undefined. */
int smsw (code_ptr p,
          const DwordReg (&   modrm_rm));
/*  0F 01  /4 RMBoth     | Store machine status word to r/m16. */
 /*  0F 01  /4 RMBoth     | Store machine status word to r/m16. */
int smsw (code_ptr p,
          const WordPtr (&   modrm_rm));
/* 66 0F 51  /r RMBoth     | Computes square roots of the packed double-precision floating-point values in xmm2/m128 and stores the results in xmm1. */
 /* 66 0F 51  /r RMBoth     | Computes square roots of the packed double-precision floating-point values in xmm2/m128 and stores the results in xmm1. */
int sqrtpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* 66 0F 51  /r RMBoth     | Computes square roots of the packed double-precision floating-point values in xmm2/m128 and stores the results in xmm1. */
 /* 66 0F 51  /r RMBoth     | Computes square roots of the packed double-precision floating-point values in xmm2/m128 and stores the results in xmm1. */
int sqrtpd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/*  0F 51  /r RMBoth     | Computes square roots of the packed single-precision floating-point values in xmm2/m128 and stores the results in xmm1. */
 /*  0F 51  /r RMBoth     | Computes square roots of the packed single-precision floating-point values in xmm2/m128 and stores the results in xmm1. */
int sqrtps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/*  0F 51  /r RMBoth     | Computes square roots of the packed single-precision floating-point values in xmm2/m128 and stores the results in xmm1. */
 /*  0F 51  /r RMBoth     | Computes square roots of the packed single-precision floating-point values in xmm2/m128 and stores the results in xmm1. */
int sqrtps (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmWordPtr (&   modrm_rm));
/* F2 0F 51  /r RMBoth     | Computes square root of the low double-precision floating-point value in xmm2/m64 and stores the results in xmm1. */
 /* F2 0F 51  /r RMBoth     | Computes square root of the low double-precision floating-point value in xmm2/m64 and stores the results in xmm1. */
int sqrtsd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* F2 0F 51  /r RMBoth     | Computes square root of the low double-precision floating-point value in xmm2/m64 and stores the results in xmm1. */
 /* F2 0F 51  /r RMBoth     | Computes square root of the low double-precision floating-point value in xmm2/m64 and stores the results in xmm1. */
int sqrtsd (code_ptr p,
            const XmmReg (&   modrm_reg),
            const QwordPtr (&   modrm_rm));
/* F3 0F 51  /r RMBoth     | Computes square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. */
 /* F3 0F 51  /r RMBoth     | Computes square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. */
int sqrtss (code_ptr p,
            const XmmReg (&   modrm_reg),
            const XmmReg (&   modrm_rm));
/* F3 0F 51  /r RMBoth     | Computes square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. */
 /* F3 0F 51  /r RMBoth     | Computes square root of the low single-precision floating-point value in xmm2/m32 and stores the results in xmm1. */
int sqrtss (code_ptr p,
            const XmmReg (&   modrm_reg),
            const DwordPtr (&   modrm_rm));
/*  F9        | Set CF flag. */
 /*  F9        | Set CF flag. */
int stc (code_ptr p);
/*  FD        | Set DF flag. */
 /*  FD        | Set DF flag. */
int std_ (code_ptr p);
/*  FB        | Set interrupt flag; external, maskable interrupts enabled at the end of the next instruction. */
 /*  FB        | Set interrupt flag; external, maskable interrupts enabled at the end of the next instruction. */
int sti (code_ptr p);
/*  0F AE  /3 RMMemOnly     | Store contents of MXCSR register to m32. */
 /*  0F AE  /3 RMMemOnly     | Store contents of MXCSR register to m32. */
int stmxcsr (code_ptr p,
             const DwordPtr (&   modrm_rm));
/*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
 /*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
int stos (code_ptr p,
          const BytePtr_ES_EDI (&   unused));
/*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
 /*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
int stos (code_ptr p,
          const BytePtr_ES_EDI (&   unused1),
          const RegAL (&   unused2));
/*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
 /*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
int stos (code_ptr p,
          const BytePtr_ES_DI (&   unused));
/*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
 /*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
int stos (code_ptr p,
          const BytePtr_ES_DI (&   unused1),
          const RegAL (&   unused2));
/*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
 /*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
int stos (code_ptr p,
          const WordPtr_ES_EDI (&   unused));
/*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
 /*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
int stos (code_ptr p,
          const WordPtr_ES_EDI (&   unused1),
          const RegAX (&   unused2));
/*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
 /*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
int stos (code_ptr p,
          const WordPtr_ES_DI (&   unused));
/*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
 /*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
int stos (code_ptr p,
          const WordPtr_ES_DI (&   unused1),
          const RegAX (&   unused2));
/*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
 /*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
int stos (code_ptr p,
          const DwordPtr_ES_EDI (&   unused));
/*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
 /*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
int stos (code_ptr p,
          const DwordPtr_ES_EDI (&   unused1),
          const RegEAX (&   unused2));
/*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
 /*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
int stos (code_ptr p,
          const DwordPtr_ES_DI (&   unused));
/*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
 /*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
int stos (code_ptr p,
          const DwordPtr_ES_DI (&   unused1),
          const RegEAX (&   unused2));
/*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
 /*  AA        | For legacy mode, store AL at address ES:(E)DI; For 64-bit mode store AL at address RDI or EDI. */
int stosb (code_ptr p);
/*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
 /*  AB      op32  | For legacy mode, store EAX at address ES:(E)DI; For 64- bit mode store EAX at address RDI or EDI. */
int stosd (code_ptr p);
/*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
 /*  AB      op16  | For legacy mode, store AX at address ES:(E)DI; For 64- bit mode store AX at address RDI or EDI. */
int stosw (code_ptr p);
/*  0F 00  /1 RMBoth     | Stores segment selector from TR in r/m16. */
 /*  0F 00  /1 RMBoth     | Stores segment selector from TR in r/m16. */
int str (code_ptr p,
         const WordReg (&   modrm_rm));
/*  0F 00  /1 RMBoth     | Stores segment selector from TR in r/m16. */
 /*  0F 00  /1 RMBoth     | Stores segment selector from TR in r/m16. */
int str (code_ptr p,
         const WordPtr (&   modrm_rm));
/*  2D     id op32  | Subtract imm32 from EAX. */
 /*  2D     id op32  | Subtract imm32 from EAX. */
int sub (code_ptr p,
         const RegEAX (&   unused),
         imm32_t imm);
/*  83  /5 RMBoth  ib op32  | Subtract sign-extended imm8 from r/m32. */
 /*  83  /5 RMBoth  ib op32  | Subtract sign-extended imm8 from r/m32. */
int sub (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm8_t imm);
/*  81  /5 RMBoth  id op32  | Subtract imm32 from r/m32. */
 /*  81  /5 RMBoth  id op32  | Subtract imm32 from r/m32. */
int sub (code_ptr p,
         const DwordReg_m_EAX (&   modrm_rm),
         imm32_t imm);
/*  81  /5 RMBoth  id op32  | Subtract imm32 from r/m32. */
 /*  81  /5 RMBoth  id op32  | Subtract imm32 from r/m32. */
int sub (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm32_t imm);
/*  83  /5 RMBoth  ib op32  | Subtract sign-extended imm8 from r/m32. */
 /*  83  /5 RMBoth  ib op32  | Subtract sign-extended imm8 from r/m32. */
int sub (code_ptr p,
         const RegEAX (&   modrm_rm),
         imm8_t imm);
/*  83  /5 RMBoth  ib op32  | Subtract sign-extended imm8 from r/m32. */
 /*  83  /5 RMBoth  ib op32  | Subtract sign-extended imm8 from r/m32. */
int sub (code_ptr p,
         const DwordPtr (&   modrm_rm),
         imm8_t imm);
/*  2D     iw op16  | Subtract imm16 from AX. */
 /*  2D     iw op16  | Subtract imm16 from AX. */
int sub (code_ptr p,
         const RegAX (&   unused),
         imm16_t imm);
/*  83  /5 RMBoth  ib op16  | Subtract sign-extended imm8 from r/m16. */
 /*  83  /5 RMBoth  ib op16  | Subtract sign-extended imm8 from r/m16. */
int sub (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm8_t imm);
/*  81  /5 RMBoth  iw op16  | Subtract imm16 from r/m16. */
 /*  81  /5 RMBoth  iw op16  | Subtract imm16 from r/m16. */
int sub (code_ptr p,
         const WordReg_m_AX (&   modrm_rm),
         imm16_t imm);
/*  81  /5 RMBoth  iw op16  | Subtract imm16 from r/m16. */
 /*  81  /5 RMBoth  iw op16  | Subtract imm16 from r/m16. */
int sub (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm16_t imm);
/*  83  /5 RMBoth  ib op16  | Subtract sign-extended imm8 from r/m16. */
 /*  83  /5 RMBoth  ib op16  | Subtract sign-extended imm8 from r/m16. */
int sub (code_ptr p,
         const RegAX (&   modrm_rm),
         imm8_t imm);
/*  83  /5 RMBoth  ib op16  | Subtract sign-extended imm8 from r/m16. */
 /*  83  /5 RMBoth  ib op16  | Subtract sign-extended imm8 from r/m16. */
int sub (code_ptr p,
         const WordPtr (&   modrm_rm),
         imm8_t imm);
/*  2B  /r RMBoth   op32  | Subtract r/m32 from r32. */
 /*  2B  /r RMBoth   op32  | Subtract r/m32 from r32. */
int sub (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordReg (&   modrm_rm));
/*  2B  /r RMBoth   op32  | Subtract r/m32 from r32. */
 /*  2B  /r RMBoth   op32  | Subtract r/m32 from r32. */
int sub (code_ptr p,
         const DwordReg (&   modrm_reg),
         const DwordPtr (&   modrm_rm));
/*  29  /r RMBoth   op32  | Subtract r32 from r/m32. */
 /*  29  /r RMBoth   op32  | Subtract r32 from r/m32. */
int sub (code_ptr p,
         const DwordPtr (&   modrm_rm),
         const DwordReg (&   modrm_reg));
/*  2B  /r RMBoth   op16  | Subtract r/m16 from r16. */
 /*  2B  /r RMBoth   op16  | Subtract r/m16 from r16. */
int sub (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordReg (&   modrm_rm));
/*  2B  /r RMBoth   op16  | Subtract r/m16 from r16. */
 /*  2B  /r RMBoth   op16  | Subtract r/m16 from r16. */
int sub (code_ptr p,
         const WordReg (&   modrm_reg),
         const WordPtr (&   modrm_rm));
/*  29  /r RMBoth   op16  | Subtract r16 from r/m16. */
 /*  29  /r RMBoth   op16  | Subtract r16 from r/m16. */
int sub (code_ptr p,
         const WordPtr (&   modrm_rm),
         const WordReg (&   modrm_reg));
/*  2A  /r RMBoth     | Subtract r/m8 from r8. */
 /*  2A  /r RMBoth     | Subtract r/m8 from r8. */
int sub (code_ptr p,
         const ByteReg (&   modrm_reg),
         const ByteReg (&   modrm_rm));
/*  2A  /r RMBoth     | Subtract r/m8 from r8. */
 /*  2A  /r RMBoth     | Subtract r/m8 from r8. */
int sub (code_ptr p,
         const ByteReg (&   modrm_reg),
         const BytePtr (&   modrm_rm));
/*  28  /r RMBoth     | Subtract r8 from r/m8. */
 /*  28  /r RMBoth     | Subtract r8 from r/m8. */
int sub (code_ptr p,
         const BytePtr (&   modrm_rm),
         const ByteReg (&   modrm_reg));
/*  2C     ib   | Subtract imm8 from AL. */
 /*  2C     ib   | Subtract imm8 from AL. */
int sub (code_ptr p,
         const RegAL (&   unused),
         imm8_t imm);
/*  80  /5 RMBoth  ib   | Subtract imm8 from r/m8. */
 /*  80  /5 RMBoth  ib   | Subtract imm8 from r/m8. */
int sub (code_ptr p,
         const ByteReg (&   modrm_rm),
         imm8_t imm);
/*  80  /5 RMBoth  ib   | Subtract imm8 from r/m8. */
 /*  80  /5 RMBoth  ib   | Subtract imm8 from r/m8. */
int sub (code_ptr p,
         const BytePtr (&   modrm_rm),
         imm8_t imm);
/* 66 0F 5C  /r RMBoth     | Subtract packed double- precision floating-point values in xmm2/m128 from xmm1. */
 /* 66 0F 5C  /r RMBoth     | Subtract packed double- precision floating-point values in xmm2/m128 from xmm1. */
int subpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 5C  /r RMBoth     | Subtract packed double- precision floating-point values in xmm2/m128 from xmm1. */
 /* 66 0F 5C  /r RMBoth     | Subtract packed double- precision floating-point values in xmm2/m128 from xmm1. */
int subpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 5C  /r RMBoth     | Subtract packed single- precision floating-point values in xmm2/mem from xmm1. */
 /*  0F 5C  /r RMBoth     | Subtract packed single- precision floating-point values in xmm2/mem from xmm1. */
int subps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 5C  /r RMBoth     | Subtract packed single- precision floating-point values in xmm2/mem from xmm1. */
 /*  0F 5C  /r RMBoth     | Subtract packed single- precision floating-point values in xmm2/mem from xmm1. */
int subps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/* F2 0F 5C  /r RMBoth     | Subtracts the low double- precision floating-point values in xmm2/mem64 from xmm1. */
 /* F2 0F 5C  /r RMBoth     | Subtracts the low double- precision floating-point values in xmm2/mem64 from xmm1. */
int subsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F2 0F 5C  /r RMBoth     | Subtracts the low double- precision floating-point values in xmm2/mem64 from xmm1. */
 /* F2 0F 5C  /r RMBoth     | Subtracts the low double- precision floating-point values in xmm2/mem64 from xmm1. */
int subsd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const QwordPtr (&   modrm_rm));
/* F3 0F 5C  /r RMBoth     | Subtract the lower single- precision floating-point values in xmm2/m32 from xmm1. */
 /* F3 0F 5C  /r RMBoth     | Subtract the lower single- precision floating-point values in xmm2/m32 from xmm1. */
int subss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* F3 0F 5C  /r RMBoth     | Subtract the lower single- precision floating-point values in xmm2/m32 from xmm1. */
 /* F3 0F 5C  /r RMBoth     | Subtract the lower single- precision floating-point values in xmm2/m32 from xmm1. */
int subss (code_ptr p,
           const XmmReg (&   modrm_reg),
           const DwordPtr (&   modrm_rm));
/*  0F 34        | Fast call to privilege level 0 system procedures. */
 /*  0F 34        | Fast call to privilege level 0 system procedures. */
int sysenter (code_ptr p);
/*  0F 35        | Fast return to privilege level 3 user code. */
 /*  0F 35        | Fast return to privilege level 3 user code. */
int sysexit (code_ptr p);
/*  A8     ib   | AND imm8 with AL; set SF, ZF, PF according to result. */
 /*  A8     ib   | AND imm8 with AL; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const RegAL (&   unused),
          imm8_t imm);
/*  A9     iw op16  | AND imm16 with AX; set SF, ZF, PF according to result. */
 /*  A9     iw op16  | AND imm16 with AX; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const RegAX (&   unused),
          imm16_t imm);
/*  A9     id op32  | AND imm32 with EAX; set SF, ZF, PF according to result. */
 /*  A9     id op32  | AND imm32 with EAX; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const RegEAX (&   unused),
          imm32_t imm);
/*  F6  /0 RMBoth  ib   | AND imm8 with r/m8; set SF, ZF, PF according to result. */
 /*  F6  /0 RMBoth  ib   | AND imm8 with r/m8; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const ByteReg (&   modrm_rm),
          imm8_t imm);
/*  F6  /0 RMBoth  ib   | AND imm8 with r/m8; set SF, ZF, PF according to result. */
 /*  F6  /0 RMBoth  ib   | AND imm8 with r/m8; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const BytePtr (&   modrm_rm),
          imm8_t imm);
/*  84  /r RMBoth     | AND r8 with r/m8; set SF, ZF, PF according to result. */
 /*  84  /r RMBoth     | AND r8 with r/m8; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const ByteReg (&   modrm_rm),
          const ByteReg (&   modrm_reg));
/*  84  /r RMBoth     | AND r8 with r/m8; set SF, ZF, PF according to result. */
 /*  84  /r RMBoth     | AND r8 with r/m8; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const BytePtr (&   modrm_rm),
          const ByteReg (&   modrm_reg));
/*  F7  /0 RMBoth  iw op16  | AND imm16 with r/m16; set SF, ZF, PF according to result. */
 /*  F7  /0 RMBoth  iw op16  | AND imm16 with r/m16; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const WordReg (&   modrm_rm),
          imm16_t imm);
/*  F7  /0 RMBoth  iw op16  | AND imm16 with r/m16; set SF, ZF, PF according to result. */
 /*  F7  /0 RMBoth  iw op16  | AND imm16 with r/m16; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const WordPtr (&   modrm_rm),
          imm16_t imm);
/*  85  /r RMBoth   op16  | AND r16 with r/m16; set SF, ZF, PF according to result. */
 /*  85  /r RMBoth   op16  | AND r16 with r/m16; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const WordReg (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  85  /r RMBoth   op16  | AND r16 with r/m16; set SF, ZF, PF according to result. */
 /*  85  /r RMBoth   op16  | AND r16 with r/m16; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  F7  /0 RMBoth  id op32  | AND imm32 with r/m32; set SF, ZF, PF according to result. */
 /*  F7  /0 RMBoth  id op32  | AND imm32 with r/m32; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const DwordReg (&   modrm_rm),
          imm32_t imm);
/*  F7  /0 RMBoth  id op32  | AND imm32 with r/m32; set SF, ZF, PF according to result. */
 /*  F7  /0 RMBoth  id op32  | AND imm32 with r/m32; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const DwordPtr (&   modrm_rm),
          imm32_t imm);
/*  85  /r RMBoth   op32  | AND r32 with r/m32; set SF, ZF, PF according to result. */
 /*  85  /r RMBoth   op32  | AND r32 with r/m32; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const DwordReg (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/*  85  /r RMBoth   op32  | AND r32 with r/m32; set SF, ZF, PF according to result. */
 /*  85  /r RMBoth   op32  | AND r32 with r/m32; set SF, ZF, PF according to result. */
int test (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/* 66 0F 2E  /r RMBoth     | Compares (unordered) the low double-precision floating-point values in xmm1 and xmm2/m64 and set the EFLAGS accordingly. */
 /* 66 0F 2E  /r RMBoth     | Compares (unordered) the low double-precision floating-point values in xmm1 and xmm2/m64 and set the EFLAGS accordingly. */
int ucomisd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/* 66 0F 2E  /r RMBoth     | Compares (unordered) the low double-precision floating-point values in xmm1 and xmm2/m64 and set the EFLAGS accordingly. */
 /* 66 0F 2E  /r RMBoth     | Compares (unordered) the low double-precision floating-point values in xmm1 and xmm2/m64 and set the EFLAGS accordingly. */
int ucomisd (code_ptr p,
             const XmmReg (&   modrm_reg),
             const QwordPtr (&   modrm_rm));
/*  0F 2E  /r RMBoth     | Compare lower single- precision floating-point value in xmm1 register with lower single-precision floating-point value in xmm2/mem and set the status flags accordingly. */
 /*  0F 2E  /r RMBoth     | Compare lower single- precision floating-point value in xmm1 register with lower single-precision floating-point value in xmm2/mem and set the status flags accordingly. */
int ucomiss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const XmmReg (&   modrm_rm));
/*  0F 2E  /r RMBoth     | Compare lower single- precision floating-point value in xmm1 register with lower single-precision floating-point value in xmm2/mem and set the status flags accordingly. */
 /*  0F 2E  /r RMBoth     | Compare lower single- precision floating-point value in xmm1 register with lower single-precision floating-point value in xmm2/mem and set the status flags accordingly. */
int ucomiss (code_ptr p,
             const XmmReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/*  0F 0B        | Raise invalid opcode exception. */
 /*  0F 0B        | Raise invalid opcode exception. */
int ud2 (code_ptr p);
/* 66 0F 15  /r RMBoth     | Unpacks and Interleaves double-precision floating- point values from high quadwords of xmm1 and xmm2/m128. */
 /* 66 0F 15  /r RMBoth     | Unpacks and Interleaves double-precision floating- point values from high quadwords of xmm1 and xmm2/m128. */
int unpckhpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 15  /r RMBoth     | Unpacks and Interleaves double-precision floating- point values from high quadwords of xmm1 and xmm2/m128. */
 /* 66 0F 15  /r RMBoth     | Unpacks and Interleaves double-precision floating- point values from high quadwords of xmm1 and xmm2/m128. */
int unpckhpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F 15  /r RMBoth     | Unpacks and Interleaves single-precision floating- point values from high quadwords of xmm1 and xmm2/mem into xmm1. */
 /*  0F 15  /r RMBoth     | Unpacks and Interleaves single-precision floating- point values from high quadwords of xmm1 and xmm2/mem into xmm1. */
int unpckhps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F 15  /r RMBoth     | Unpacks and Interleaves single-precision floating- point values from high quadwords of xmm1 and xmm2/mem into xmm1. */
 /*  0F 15  /r RMBoth     | Unpacks and Interleaves single-precision floating- point values from high quadwords of xmm1 and xmm2/mem into xmm1. */
int unpckhps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/* 66 0F 14  /r RMBoth     | Unpacks and Interleaves double-precision floating- point values from low quadwords of xmm1 and xmm2/m128. */
 /* 66 0F 14  /r RMBoth     | Unpacks and Interleaves double-precision floating- point values from low quadwords of xmm1 and xmm2/m128. */
int unpcklpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/* 66 0F 14  /r RMBoth     | Unpacks and Interleaves double-precision floating- point values from low quadwords of xmm1 and xmm2/m128. */
 /* 66 0F 14  /r RMBoth     | Unpacks and Interleaves double-precision floating- point values from low quadwords of xmm1 and xmm2/m128. */
int unpcklpd (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F 14  /r RMBoth     | Unpacks and Interleaves single-precision floating- point values from low quadwords of xmm1 and xmm2/mem into xmm1. */
 /*  0F 14  /r RMBoth     | Unpacks and Interleaves single-precision floating- point values from low quadwords of xmm1 and xmm2/mem into xmm1. */
int unpcklps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmReg (&   modrm_rm));
/*  0F 14  /r RMBoth     | Unpacks and Interleaves single-precision floating- point values from low quadwords of xmm1 and xmm2/mem into xmm1. */
 /*  0F 14  /r RMBoth     | Unpacks and Interleaves single-precision floating- point values from low quadwords of xmm1 and xmm2/mem into xmm1. */
int unpcklps (code_ptr p,
              const XmmReg (&   modrm_reg),
              const XmmWordPtr (&   modrm_rm));
/*  0F 00  /4 RMBoth     | Set ZF=1 if segment specified with r/m16 can be read. */
 /*  0F 00  /4 RMBoth     | Set ZF=1 if segment specified with r/m16 can be read. */
int verr (code_ptr p,
          const WordReg (&   modrm_rm));
/*  0F 00  /4 RMBoth     | Set ZF=1 if segment specified with r/m16 can be read. */
 /*  0F 00  /4 RMBoth     | Set ZF=1 if segment specified with r/m16 can be read. */
int verr (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  0F 00  /5 RMBoth     | Set ZF=1 if segment specified with r/m16 can be written. */
 /*  0F 00  /5 RMBoth     | Set ZF=1 if segment specified with r/m16 can be written. */
int verw (code_ptr p,
          const WordReg (&   modrm_rm));
/*  0F 00  /5 RMBoth     | Set ZF=1 if segment specified with r/m16 can be written. */
 /*  0F 00  /5 RMBoth     | Set ZF=1 if segment specified with r/m16 can be written. */
int verw (code_ptr p,
          const WordPtr (&   modrm_rm));
/*  0F 01 C1        | Call to VM monitor by causing VM exit. */
 /*  0F 01 C1        | Call to VM monitor by causing VM exit. */
int vmcall (code_ptr p);
/* 66 0F C7  /6 RMMemOnly     | Copy VMCS data to VMCS region in memory. */
 /* 66 0F C7  /6 RMMemOnly     | Copy VMCS data to VMCS region in memory. */
int vmclear (code_ptr p,
             const QwordPtr (&   modrm_rm));
/*  0F 01 C2        | Launch virtual machine managed by current VMCS. */
 /*  0F 01 C2        | Launch virtual machine managed by current VMCS. */
int vmlaunch (code_ptr p);
/*  0F C7  /6 RMMemOnly     | Loads the current VMCS pointer from memory. */
 /*  0F C7  /6 RMMemOnly     | Loads the current VMCS pointer from memory. */
int vmptrld (code_ptr p,
             const QwordPtr (&   modrm_rm));
/*  0F C7  /7 RMMemOnly     | Stores the current VMCS pointer into memory. */
 /*  0F C7  /7 RMMemOnly     | Stores the current VMCS pointer into memory. */
int vmptrst (code_ptr p,
             const QwordPtr (&   modrm_rm));
/*  0F 78  /r RMBoth     | Reads a specified VMCS field (outside 64-bit mode). */
 /*  0F 78  /r RMBoth     | Reads a specified VMCS field (outside 64-bit mode). */
int vmread (code_ptr p,
            const DwordReg (&   modrm_rm),
            const DwordReg (&   modrm_reg));
/*  0F 78  /r RMBoth     | Reads a specified VMCS field (outside 64-bit mode). */
 /*  0F 78  /r RMBoth     | Reads a specified VMCS field (outside 64-bit mode). */
int vmread (code_ptr p,
            const DwordPtr (&   modrm_rm),
            const DwordReg (&   modrm_reg));
/*  0F 01 C3        | Resume virtual machine managed by current VMCS. */
 /*  0F 01 C3        | Resume virtual machine managed by current VMCS. */
int vmresume (code_ptr p);
/*  0F 79  /r RMBoth     | Writes a specified VMCS field (outside 64-bit mode) */
 /*  0F 79  /r RMBoth     | Writes a specified VMCS field (outside 64-bit mode) */
int vmwrite (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordReg (&   modrm_rm));
/*  0F 79  /r RMBoth     | Writes a specified VMCS field (outside 64-bit mode) */
 /*  0F 79  /r RMBoth     | Writes a specified VMCS field (outside 64-bit mode) */
int vmwrite (code_ptr p,
             const DwordReg (&   modrm_reg),
             const DwordPtr (&   modrm_rm));
/*  0F 01 C4        | Leaves VMX operation. */
 /*  0F 01 C4        | Leaves VMX operation. */
int vmxoff (code_ptr p);
/* F3 0F C7  /6 RMMemOnly     | Enter VMX root operation. */
 /* F3 0F C7  /6 RMMemOnly     | Enter VMX root operation. */
int vmxon (code_ptr p,
           const QwordPtr (&   modrm_rm));
/*  9B        | Check pending unmasked floating-point exceptions. */
 /*  9B        | Check pending unmasked floating-point exceptions. */
int wait_ (code_ptr p);
/*  0F 09        | Write back and flush Internal caches; initiate writing-back and flushing of external caches. */
 /*  0F 09        | Write back and flush Internal caches; initiate writing-back and flushing of external caches. */
int wbinvd (code_ptr p);
/*  0F 30        | Write the value in EDX:EAX to MSR specified by ECX. */
 /*  0F 30        | Write the value in EDX:EAX to MSR specified by ECX. */
int wrmsr (code_ptr p);
/*  0F C0  /r RMBoth     | Exchange r8 and r/m8; load sum into r/m8. */
 /*  0F C0  /r RMBoth     | Exchange r8 and r/m8; load sum into r/m8. */
int xadd (code_ptr p,
          const ByteReg (&   modrm_rm),
          const ByteReg (&   modrm_reg));
/*  0F C0  /r RMBoth     | Exchange r8 and r/m8; load sum into r/m8. */
 /*  0F C0  /r RMBoth     | Exchange r8 and r/m8; load sum into r/m8. */
int xadd (code_ptr p,
          const BytePtr (&   modrm_rm),
          const ByteReg (&   modrm_reg));
/*  0F C1  /r RMBoth   op16  | Exchange r16 and r/m16; load sum into r/m16. */
 /*  0F C1  /r RMBoth   op16  | Exchange r16 and r/m16; load sum into r/m16. */
int xadd (code_ptr p,
          const WordReg (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  0F C1  /r RMBoth   op16  | Exchange r16 and r/m16; load sum into r/m16. */
 /*  0F C1  /r RMBoth   op16  | Exchange r16 and r/m16; load sum into r/m16. */
int xadd (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  0F C1  /r RMBoth   op32  | Exchange r32 and r/m32; load sum into r/m32. */
 /*  0F C1  /r RMBoth   op32  | Exchange r32 and r/m32; load sum into r/m32. */
int xadd (code_ptr p,
          const DwordReg (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/*  0F C1  /r RMBoth   op32  | Exchange r32 and r/m32; load sum into r/m32. */
 /*  0F C1  /r RMBoth   op32  | Exchange r32 and r/m32; load sum into r/m32. */
int xadd (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/*  87  /r RMBoth   op32  | Exchange r32 with doubleword from r/m32. */
 /*  87  /r RMBoth   op32  | Exchange r32 with doubleword from r/m32. */
int xchg (code_ptr p,
          const RegEAX (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/*  87  /r RMBoth   op32  | Exchange r32 with doubleword from r/m32. */
 /*  87  /r RMBoth   op32  | Exchange r32 with doubleword from r/m32. */
int xchg (code_ptr p,
          const DwordReg_m_EAX (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/*  90    +rd  op32  | Exchange EAX with r32. */
 /*  90    +rd  op32  | Exchange EAX with r32. */
int xchg (code_ptr p,
          const DwordReg_m_EAX (&   radd),
          const RegEAX (&   unused));
/*  87  /r RMBoth   op32  | Exchange r32 with doubleword from r/m32. */
 /*  87  /r RMBoth   op32  | Exchange r32 with doubleword from r/m32. */
int xchg (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/*  87  /r RMBoth   op32  | Exchange doubleword from r/m32 with r32. */
 /*  87  /r RMBoth   op32  | Exchange doubleword from r/m32 with r32. */
int xchg (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordPtr (&   modrm_rm));
/*  87  /r RMBoth   op16  | Exchange r16 with word from r/m16. */
 /*  87  /r RMBoth   op16  | Exchange r16 with word from r/m16. */
int xchg (code_ptr p,
          const RegAX (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  87  /r RMBoth   op16  | Exchange r16 with word from r/m16. */
 /*  87  /r RMBoth   op16  | Exchange r16 with word from r/m16. */
int xchg (code_ptr p,
          const WordReg_m_AX (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  90    +rw  op16  | Exchange AX with r16. */
 /*  90    +rw  op16  | Exchange AX with r16. */
int xchg (code_ptr p,
          const WordReg_m_AX (&   radd),
          const RegAX (&   unused));
/*  87  /r RMBoth   op16  | Exchange r16 with word from r/m16. */
 /*  87  /r RMBoth   op16  | Exchange r16 with word from r/m16. */
int xchg (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  87  /r RMBoth   op16  | Exchange word from r/m16 with r16. */
 /*  87  /r RMBoth   op16  | Exchange word from r/m16 with r16. */
int xchg (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordPtr (&   modrm_rm));
/*  86  /r RMBoth     | Exchange byte from r/m8 with r8 (byte register). */
 /*  86  /r RMBoth     | Exchange byte from r/m8 with r8 (byte register). */
int xchg (code_ptr p,
          const ByteReg (&   modrm_reg),
          const ByteReg (&   modrm_rm));
/*  86  /r RMBoth     | Exchange byte from r/m8 with r8 (byte register). */
 /*  86  /r RMBoth     | Exchange byte from r/m8 with r8 (byte register). */
int xchg (code_ptr p,
          const ByteReg (&   modrm_reg),
          const BytePtr (&   modrm_rm));
/*  86  /r RMBoth     | Exchange r8 (byte register) with byte from r/m8. */
 /*  86  /r RMBoth     | Exchange r8 (byte register) with byte from r/m8. */
int xchg (code_ptr p,
          const BytePtr (&   modrm_rm),
          const ByteReg (&   modrm_reg));
/*  0F 01 D0        | Reads an XCR specified by ECX into EDX:EAX. */
 /*  0F 01 D0        | Reads an XCR specified by ECX into EDX:EAX. */
int xgetbv (code_ptr p);
/*  D7        | Set AL to memory byte DS:[(E)BX + unsigned AL]. */
 /*  D7        | Set AL to memory byte DS:[(E)BX + unsigned AL]. */
int xlat (code_ptr p,
          const BytePtr_EBX (&   ptr));
/*  D7        | Set AL to memory byte DS:[(E)BX + unsigned AL]. */
 /*  D7        | Set AL to memory byte DS:[(E)BX + unsigned AL]. */
int xlat (code_ptr p,
          const BytePtr_BX (&   ptr));
/*  D7        | Set AL to memory byte DS:[(E)BX + unsigned AL]. */
 /*  D7        | Set AL to memory byte DS:[(E)BX + unsigned AL]. */
int xlatb (code_ptr p);
/*  35     id op32  | EAX XOR imm32. */
 /*  35     id op32  | EAX XOR imm32. */
int xor_ (code_ptr p,
          const RegEAX (&   unused),
          imm32_t imm);
/*  83  /6 RMBoth  ib op32  | r/m32 XOR imm8 (sign- extended). */
 /*  83  /6 RMBoth  ib op32  | r/m32 XOR imm8 (sign- extended). */
int xor_ (code_ptr p,
          const DwordReg_m_EAX (&   modrm_rm),
          imm8_t imm);
/*  81  /6 RMBoth  id op32  | r/m32 XOR imm32. */
 /*  81  /6 RMBoth  id op32  | r/m32 XOR imm32. */
int xor_ (code_ptr p,
          const DwordReg_m_EAX (&   modrm_rm),
          imm32_t imm);
/*  81  /6 RMBoth  id op32  | r/m32 XOR imm32. */
 /*  81  /6 RMBoth  id op32  | r/m32 XOR imm32. */
int xor_ (code_ptr p,
          const DwordPtr (&   modrm_rm),
          imm32_t imm);
/*  83  /6 RMBoth  ib op32  | r/m32 XOR imm8 (sign- extended). */
 /*  83  /6 RMBoth  ib op32  | r/m32 XOR imm8 (sign- extended). */
int xor_ (code_ptr p,
          const RegEAX (&   modrm_rm),
          imm8_t imm);
/*  83  /6 RMBoth  ib op32  | r/m32 XOR imm8 (sign- extended). */
 /*  83  /6 RMBoth  ib op32  | r/m32 XOR imm8 (sign- extended). */
int xor_ (code_ptr p,
          const DwordPtr (&   modrm_rm),
          imm8_t imm);
/*  35     iw op16  | AX XOR imm16. */
 /*  35     iw op16  | AX XOR imm16. */
int xor_ (code_ptr p,
          const RegAX (&   unused),
          imm16_t imm);
/*  83  /6 RMBoth  ib op16  | r/m16 XOR imm8 (sign- extended). */
 /*  83  /6 RMBoth  ib op16  | r/m16 XOR imm8 (sign- extended). */
int xor_ (code_ptr p,
          const WordReg_m_AX (&   modrm_rm),
          imm8_t imm);
/*  81  /6 RMBoth  iw op16  | r/m16 XOR imm16. */
 /*  81  /6 RMBoth  iw op16  | r/m16 XOR imm16. */
int xor_ (code_ptr p,
          const WordReg_m_AX (&   modrm_rm),
          imm16_t imm);
/*  81  /6 RMBoth  iw op16  | r/m16 XOR imm16. */
 /*  81  /6 RMBoth  iw op16  | r/m16 XOR imm16. */
int xor_ (code_ptr p,
          const WordPtr (&   modrm_rm),
          imm16_t imm);
/*  83  /6 RMBoth  ib op16  | r/m16 XOR imm8 (sign- extended). */
 /*  83  /6 RMBoth  ib op16  | r/m16 XOR imm8 (sign- extended). */
int xor_ (code_ptr p,
          const RegAX (&   modrm_rm),
          imm8_t imm);
/*  83  /6 RMBoth  ib op16  | r/m16 XOR imm8 (sign- extended). */
 /*  83  /6 RMBoth  ib op16  | r/m16 XOR imm8 (sign- extended). */
int xor_ (code_ptr p,
          const WordPtr (&   modrm_rm),
          imm8_t imm);
/*  33  /r RMBoth   op32  | r32 XOR r/m32. */
 /*  33  /r RMBoth   op32  | r32 XOR r/m32. */
int xor_ (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordReg (&   modrm_rm));
/*  33  /r RMBoth   op32  | r32 XOR r/m32. */
 /*  33  /r RMBoth   op32  | r32 XOR r/m32. */
int xor_ (code_ptr p,
          const DwordReg (&   modrm_reg),
          const DwordPtr (&   modrm_rm));
/*  31  /r RMBoth   op32  | r/m32 XOR r32. */
 /*  31  /r RMBoth   op32  | r/m32 XOR r32. */
int xor_ (code_ptr p,
          const DwordPtr (&   modrm_rm),
          const DwordReg (&   modrm_reg));
/*  33  /r RMBoth   op16  | r16 XOR r/m16. */
 /*  33  /r RMBoth   op16  | r16 XOR r/m16. */
int xor_ (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordReg (&   modrm_rm));
/*  33  /r RMBoth   op16  | r16 XOR r/m16. */
 /*  33  /r RMBoth   op16  | r16 XOR r/m16. */
int xor_ (code_ptr p,
          const WordReg (&   modrm_reg),
          const WordPtr (&   modrm_rm));
/*  31  /r RMBoth   op16  | r/m16 XOR r16. */
 /*  31  /r RMBoth   op16  | r/m16 XOR r16. */
int xor_ (code_ptr p,
          const WordPtr (&   modrm_rm),
          const WordReg (&   modrm_reg));
/*  32  /r RMBoth     | r8 XOR r/m8. */
 /*  32  /r RMBoth     | r8 XOR r/m8. */
int xor_ (code_ptr p,
          const ByteReg (&   modrm_reg),
          const ByteReg (&   modrm_rm));
/*  32  /r RMBoth     | r8 XOR r/m8. */
 /*  32  /r RMBoth     | r8 XOR r/m8. */
int xor_ (code_ptr p,
          const ByteReg (&   modrm_reg),
          const BytePtr (&   modrm_rm));
/*  30  /r RMBoth     | r/m8 XOR r8. */
 /*  30  /r RMBoth     | r/m8 XOR r8. */
int xor_ (code_ptr p,
          const BytePtr (&   modrm_rm),
          const ByteReg (&   modrm_reg));
/*  34     ib   | AL XOR imm8. */
 /*  34     ib   | AL XOR imm8. */
int xor_ (code_ptr p,
          const RegAL (&   unused),
          imm8_t imm);
/*  80  /6 RMBoth  ib   | r/m8 XOR imm8. */
 /*  80  /6 RMBoth  ib   | r/m8 XOR imm8. */
int xor_ (code_ptr p,
          const ByteReg (&   modrm_rm),
          imm8_t imm);
/*  80  /6 RMBoth  ib   | r/m8 XOR imm8. */
 /*  80  /6 RMBoth  ib   | r/m8 XOR imm8. */
int xor_ (code_ptr p,
          const BytePtr (&   modrm_rm),
          imm8_t imm);
/* 66 0F 57  /r RMBoth     | Bitwise exclusive-OR of xmm2/m128 and xmm1. */
 /* 66 0F 57  /r RMBoth     | Bitwise exclusive-OR of xmm2/m128 and xmm1. */
int xorpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/* 66 0F 57  /r RMBoth     | Bitwise exclusive-OR of xmm2/m128 and xmm1. */
 /* 66 0F 57  /r RMBoth     | Bitwise exclusive-OR of xmm2/m128 and xmm1. */
int xorpd (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F 57  /r RMBoth     | Bitwise exclusive-OR of xmm2/m128 and xmm1. */
 /*  0F 57  /r RMBoth     | Bitwise exclusive-OR of xmm2/m128 and xmm1. */
int xorps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmReg (&   modrm_rm));
/*  0F 57  /r RMBoth     | Bitwise exclusive-OR of xmm2/m128 and xmm1. */
 /*  0F 57  /r RMBoth     | Bitwise exclusive-OR of xmm2/m128 and xmm1. */
int xorps (code_ptr p,
           const XmmReg (&   modrm_reg),
           const XmmWordPtr (&   modrm_rm));
/*  0F AE  /5 RMMemOnly     | Restore processor extended states from memory. The states are specified by EDX:EAX */
 /*  0F AE  /5 RMMemOnly     | Restore processor extended states from memory. The states are specified by EDX:EAX */
int xrstor (code_ptr p,
            const VoidPtr (&   modrm_rm));
/*  0F AE  /4 RMMemOnly     | Save processor extended states to memory. The states are specified by EDX:EAX */
 /*  0F AE  /4 RMMemOnly     | Save processor extended states to memory. The states are specified by EDX:EAX */
int xsave (code_ptr p,
           const VoidPtr (&   modrm_rm));
/*  0F 01 D1        | Write the value in EDX:EAX to the XCR specified by ECX. */
 /*  0F 01 D1        | Write the value in EDX:EAX to the XCR specified by ECX. */
int xsetbv (code_ptr p);
}
}
}
}
#else

#endif