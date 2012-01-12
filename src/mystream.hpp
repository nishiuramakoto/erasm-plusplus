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
#ifndef MY_MYSTREAM_HPP
#define MY_MYSTREAM_HPP

#include <iostream>
#include <stdio.h>
#include <assert.h>
#include <erasm/common_macros.hpp>

namespace my {
using namespace std;

typedef char charT;
typedef char_traits<charT> traits ;

class nullbuf : public basic_streambuf<charT,traits>
{
public:
   explicit nullbuf()
      : count_(0)
      { }
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

class nullstream : public  ostream
{
public:
   typedef ostream base;
   explicit nullstream()
      : base(&buf) , buf()
      {}
   int get_count() const
      { return buf.get_count() ;}

private:
   nullbuf buf;
};



template<int BufSize = 100>
class mybuf : public basic_streambuf<charT,traits>
{
public:
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

   charT buf_[buffer_size+1];
};

template<int BufSize= 1024 - 1>
class omystream : public  ostream
{
public:
   typedef ostream base;
   explicit omystream(FILE* file = stdout)
      : base(&buf) , buf(file)
      {}

private:
   mybuf<BufSize> buf;
};

}


#endif // MY_MYSTREAM_HPP
