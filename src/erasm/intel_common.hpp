/* 
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
   along with ERASM++; see the file COPYING.  If not see
   <http://www.gnu.org/licenses/>.  */
#ifndef MY_ERASM_INTEL_COMMON_HPP
#define MY_ERASM_INTEL_COMMON_HPP

#include "common_macros.hpp"
#include <assert.h>
#include <cstring>
#include <limits>
#include "erasm/x64_implementation_defined.hpp"


namespace erasm { namespace x64 { 

namespace impl {

// g++ is sometimes very bad at native code generation for x86/x64
// The code assumes n is statically determined.
// If not,the amount of code bloat would not be acceptable for most cases.

#ifdef CUSTOM_MEMCPY
inline
void* memcpy(void *out,const void*in,size_t n)
{
   byte_t * p = (byte_t *) out;
   const byte_t * q = (const byte_t *) in;

   switch(n) {
   case 0:
      break;
   case 1:
      p[0] = q[0];
      break;
   case 2:
      p[0] = q[0];
      p[1] = q[1];
      break;
   case 3:
      p[0] = q[0];
      p[1] = q[1];
      p[2] = q[2];
      break;
   case 4:
      p[0] = q[0];
      p[1] = q[1];
      p[2] = q[2];
      p[3] = q[3];
      break;
   case 5:
      p[0] = q[0];
      p[1] = q[1];
      p[2] = q[2];
      p[3] = q[3];
      p[4] = q[4];
      break;
   case 6:
      p[0] = q[0];
      p[1] = q[1];
      p[2] = q[2];
      p[3] = q[3];
      p[4] = q[4];
      p[5] = q[5];
      break;

   default:
      std::memcpy(out,in,n);
   }
   return out;
}
#else
inline
void* memcpy(void *out,const void*in,size_t n)
{ return std::memcpy(out,in,n); }
#endif


template<class X,class Y,class Z> 
inline bool oneof(const X& x, const Y& a,const Z& b)
{ return x == a || x == b  ; }

template<class X,class Y,class Z,class W> 
inline bool oneof(const X& x, const Y& a,const Z& b,const W& c)
{ return x == a || x == b || x == c ; }

template<class X,class Y,class Z,class W,class U> 
inline bool oneof(const X& x, const Y& a,const Z& b,const W& c,const U& d)
{ return x == a || x == b || x == c || x == d ; }

template<class X> 
inline bool in_range(const X& x,const X& a,const X& b)
{ return a <= x && x <= b ; }


inline int disp_size_map(int d,int x0,int x1,int x2,int x4)
{
   assert(impl::oneof(d,0,1,2,4));
   switch(d) {
   case 0: 
      return x0;
   case 1:
      return x1;
   case 2:
      return x2;
   default:
      return x4;
   }
}

inline int minimum_representable_bytes(disp16_type x)
{ 
   const int disp8_type_min = std::numeric_limits<disp8_type>::min();
   const int disp8_type_max = std::numeric_limits<disp8_type>::max();
   assert(disp8_type_min == -128);
   assert(disp8_type_max == 127);
   if (x == 0) {
      return 0;
   }  else if (disp8_type_min  <= x && x<= disp8_type_max ) {
      return 1;
   }  else {
      return 2;
   }
}

inline int minimum_representable_bytes(disp32_type x)
{ 
   const int disp8_type_min = std::numeric_limits<disp8_type>::min();
   const int disp8_type_max = std::numeric_limits<disp8_type>::max();
   const int disp16_type_min = std::numeric_limits<disp16_type>::min();
   const int disp16_type_max = std::numeric_limits<disp16_type>::max();

   assert(disp8_type_min == -128);
   assert(disp8_type_max == 127);
	 
   assert(disp16_type_min == -32768);
   assert(disp16_type_max == 32767);

   if (x == 0) {
      return 0;
   }  else if (disp8_type_min  <= x && x<= disp8_type_max ) {
      return 1;
   }  else if (disp16_type_min <= x && x<= disp16_type_max ) {
      return 2;
   }  else {
      return 4;
   }
}


inline disp8_type round8(disp32_type disp)
{
   /*
     The C++ Standard 4.7.3
     If the destination type is signed,the value is unchanged if it can be represented in 
     the destination type;otherwise,the value is implementation-defined.
   */
   disp8_type d = (disp8_type) disp;
   assert(d == disp);
   return d;
}

inline disp16_type round16(disp32_type disp)
{
   /*
     The C++ Standard 4.7.3
     If the destination type is signed,the value is unchanged if it can be represented in 
     the destination type;otherwise,the value is implementation-defined.
   */
   disp16_type d = (disp16_type) disp;
   assert(d == disp);
   return d;
}

} // namespace impl


enum AddressingMode
{
   ADDRESS_MODE_16,
   ADDRESS_MODE_32,
   ADDRESS_MODE_64
};


struct ModrmConstHandle
{
   const ModrmBytes & rep_;
   AddressingMode address_mode_;

   ModrmConstHandle(const ModrmBytes& m,AddressingMode address_mode)
      : rep_(m),address_mode_(address_mode)
      {}
   ModrmConstHandle(const_code_ptr p,AddressingMode address_mode)
      : rep_( * (const ModrmBytes *) p ) ,address_mode_(address_mode)
      {}

   const ModrmBytes& get_rep() const  
      { return rep_ ; }

   AddressingMode get_address_mode() const 
      { return address_mode_ ; }

   bool is16bit() const 
      { return address_mode_ == ADDRESS_MODE_16 ; }

   int  get_disp_size() const 
      { return is16bit() ? get_disp_size16() : get_disp_size32() ; }

   disp32_type get_disp() const  
      { return is16bit() ? get_disp16() : get_disp32() ; }


   bool  has_sib () const 
      {	 return ! is16bit() && rep_.mod != 3  && rep_.rm == 4; }

   int size () const 
      { return 1 + (has_sib() ? 1 : 0) + get_disp_size() ; }

   int copyTo(ModrmBytes& m) const 
      {
	 int n = size();
	 impl::memcpy(m.raw_bytes,rep_.raw_bytes,n);
	 return n;
      }

   int encode(code_ptr p) const 
      { 
	 assert(p);
	 int n = size();
	 impl::memcpy(p,rep_.raw_bytes,n);
	 return n;
      }

   int get_mod() const 
      { return rep_.mod ; }
   int get_op_reg() const 
      { return rep_.op_reg ; }
   int get_rm() const 
      { return rep_.rm ; }
   int get_sib_base() const 
      { return rep_.sib.base ; }
   int get_sib_index() const 
      { return rep_.sib.index ; }
   int get_sib_ss() const 
      { return rep_.sib.ss ; }

   disp8_type get_disp8() const
      {	 return has_sib() ? rep_.sib.disp8 : rep_.disp8 ; }
   disp16_type get_disp16() const
      {	 
	 assert(! has_sib ());
	 return rep_.disp16;
      }
   disp32_type get_disp32() const
      { return has_sib() ? rep_.sib.disp32 : rep_.disp32 ; }

private:

   int  get_disp_size32() const 
      {
	 assert(0 <= rep_.mod && rep_.mod <= 3 );
	 switch(rep_.mod) {
	 case 0:
	    switch(rep_.rm) {
	    case 4:   return rep_.sib.base == 5 ? 4 : 0;
	    case 5:   return 4;
	    default:  return 0;
	    }
	 case 1:
	    return 1;
	 case 2:
	    return 4;
	 default:
	    return 0;
	 }; 
      }

   int  get_disp_size16() const
      {
	 assert(impl::oneof(rep_.mod,0u,1u,2u,3u));
	 switch(rep_.mod) {
	 case 0:
	    switch(rep_.rm) {
	    case 6:   return 2;
	    default:  return 0;
	    }
	 case 1:
	    return 1;
	 case 2:
	    return 2;
	 default:
	    return 0;
	 }; 
      }

};

extern const char * const byte_reg_names [];
extern const char * const byte_reg_rex_names [];
extern const char * const word_reg_names [] ;
extern const char * const dword_reg_names [] ;
extern const char * const qword_reg_names [] ;
extern const char * const mm_reg_names [] ;
extern const char * const xmm_reg_names [] ;
extern const char * const st_reg_names [] ;
extern const char * const seg_reg_names [] ;
extern const char * const cr_reg_names [] ;
extern const char * const dr_reg_names [] ;

}}


#endif // MY_ERASM_INTEL_COMMON_HPP
