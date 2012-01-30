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
#ifndef ERASM_FASTSTREAM_HPP
#define ERASM_FASTSTREAM_HPP

#include <iostream>
#include <stdio.h>
#include <assert.h>
#include <erasm/common_macros.hpp>

namespace erasm {

namespace impl {
typedef char charT;
typedef std::char_traits<charT> char_traits ;
}

class nullbuf : public std::basic_streambuf<impl::charT,impl::char_traits>
{
public:
   typedef impl::char_traits traits;

   explicit nullbuf()
      : count_(0)
      { }
   void reset() 
      { count_ = 0 ;}
   int get_count() const
      { return count_ ; }

protected:
   virtual int_type overflow (int_type c = traits::eof())
      {
	 if (c == traits::eof())  {
	    return traits::not_eof(c);
	 }
	 count_ ++;
	 return c;
      }
   int count_;
};

class nullstream : public  std::ostream
{
public:
   typedef std::ostream base;
   explicit nullstream()
      : base(&buf) , buf()
      {}
   int get_count() const
      { return buf.get_count() ;}
   void reset() 
      { buf.reset() ; }

private:
   nullbuf buf;
};


template<int BufSize = 100>
class mybuf : public std::basic_streambuf<impl::charT,impl::char_traits>
{
public:
   typedef impl::char_traits traits;
   typedef impl::charT char_type;

   explicit mybuf(FILE* file = stdout)
      : file_(file)
      {  reset() ; }
   virtual ~mybuf()
      {
	 // If an exception is thrown in a destructor,
	 // terminate() is called.
	 // If this is a concern,you should manually call flush().
	 flush(pos());
      }

   int pos() const
      { return pptr() - pbase() ; }
   size_t flush(size_t n)
      {
	 size_t m = fwrite(buf_,sizeof(char_type),n,file_);
	 assert(m == n);
	 reset();
	 return m;
      }


protected:
   void reset()
      {  
	 setp(&buf_[0],&buf_[buffer_size]); 
      }

   // virtual int sync()
   //    {
   // 	 DUMP2(pos(),buffer_size);
   // 	 return 0;
   //    }
   // virtual streamsize xsputn(const char_type* s,streamsize n)
   //    {
   // 	 DUMP2(pos(),n);
   // 	 reset();
   // 	 return n;
   //    }

   
   virtual int_type overflow (int_type c = traits::eof())
      {
	 if (c == traits::eof())  {
	    flush(pos());
	    return traits::not_eof(c);
	 } 
	 
	 buf_[buffer_size] = c;
	 flush(buffer_size+1);
	 return c;	    
      }
   
   FILE* file_;

   enum { buffer_size = BufSize };

   char_type buf_[buffer_size+1];
};

template<int BufSize= 1024 - 1>
   class ofaststream : public  std::ostream
{
public:
   typedef std::ostream base;
   explicit ofaststream(FILE* file = stdout)
      : base(&buf) , buf(file)
      {}

private:
   mybuf<BufSize> buf;
};

extern nullstream cout_counter;
extern ofaststream<500>  cout;
extern ofaststream<500>  cerr;

}


#endif // ERASM_FASTSTREAM_HPP
