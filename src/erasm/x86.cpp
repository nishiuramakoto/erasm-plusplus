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
#define ERASM_NO_META_ASSERT 1
#include "erasm/x86.hpp"
#include <limits>
#include "common_macros.hpp"

namespace erasm {  namespace x86 {  

PtrProxyContainer<void> void_ptr;		
PtrProxyContainer<uint8_t> byte_ptr;		
PtrProxyContainer<uint16_t> word_ptr;		
PtrProxyContainer<uint32_t> dword_ptr;	
PtrProxyContainer<uint64_t> qword_ptr;
PtrProxyContainer<MmWord> mmword_ptr;
PtrProxyContainer<XmmWord> xmmword_ptr;
PtrProxyContainer<Fword>   fword_ptr;
PtrProxyContainer<Oword>   oword_ptr;
PtrProxyContainer<Tbyte>   tbyte_ptr;
PtrProxyContainer<Real4>   real4_ptr;
PtrProxyContainer<Real8>   real8_ptr;
PtrProxyContainer<Real10>  real10_ptr;
PtrProxyContainer<FarPtr16> far16_ptr;
PtrProxyContainer<FarPtr32> far32_ptr;

OffsetProxyContainer<uint8_t> byte_offset;		
OffsetProxyContainer<uint16_t> word_offset;		
OffsetProxyContainer<uint32_t> dword_offset;	
OffsetProxyContainer<uint64_t> qword_offset;

}}

