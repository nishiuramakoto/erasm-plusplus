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
#ifndef ERASM_X64_PREFIX
#define ERASM_X64_PREFIX

namespace erasm {  namespace x64 {
int prefix_operand(code_ptr p);
int prefix_address(code_ptr p);
int prefix_fs(code_ptr p);
int prefix_gs(code_ptr p);

}}


#endif // ERASM_X64_PREFIX
