#include <erasm/x64_addr64_data32.hpp>
#include <iostream>

typedef int (*pf_t) (int);

int main(int argc,char**argv)
{
   using namespace std;
   using namespace erasm::x64;
   using namespace erasm::x64::addr64;
   using namespace erasm::x64::addr64::data32;

   byte_t   buf[100];
   code_ptr p = buf;
   imm32_t  x = 2;

   p += mov(p,rax,qword_ptr[rsp+8]);
   p += add(p,rax,x);
   p += ret(p);

   pf_t f = (pf_t)buf;   
   int len = p - buf;

   cout  << "result="      << f(argc) << endl
	 << "code length=" << len     << endl;
	 
   return 0;
}
