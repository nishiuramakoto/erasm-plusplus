#include "erasm/x86_addr32_data32.hpp"
#include <iostream>

typedef int (*pf_t) (int);

int main(int argc,char**argv)
{
   using namespace std;
   using namespace erasm::x86;
   using namespace erasm::x86::addr32;
   using namespace erasm::x86::addr32::data32;

   byte_t buf[100];
   byte_t *p = buf;
   int32_t x = 2;

   p += mov(p,eax,dword_ptr[esp+4]);
   p += add(p,eax,x);
   p += ret(p);

   pf_t f = (pf_t)buf;   
   int len = p - buf;
   cout  << "code length=" << len << endl
	 << "result=" << f(argc) << endl;
   return 0;
}
