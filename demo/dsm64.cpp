#include <erasm/dsm_x64_util.hpp>
#include <erasm/dsm_x64.hpp>
#include <erasm/faststream.hpp>

using namespace std;
using namespace erasm::x64;
using namespace erasm::x64::addr64::data32;

struct MyDsm : public SimpleDsm<>
{
   MyDsm(const_code_ptr start,ostream& os = erasm::cout)
      : SimpleDsm(start,os)
      {}

   using SimpleDsm::action;

   action_result_type
   action(const Ret& insn)
      {
	 os() << "Matched:" << Ret::mnemonic << endl;
	 return finish(insn);
      }

   template<class Op>
   action_result_type
   action(const Push& insn,const Op& op)
      {
	 os() << "Matched:" << Push::mnemonic
	      << " " << op << endl;
	 return next(insn);
      }

};

int f(int x)
{
   return x+1;
}

int main(int argc,char**argv)
{
   const_code_ptr p = (const_code_ptr) f;
   MyDsm   mydsm(p);

   const_code_ptr end = decode(p,mydsm);

   if (mydsm.failed()) {
      erasm::cout << "Disassembly failed at:"
		  << hex << (void*)end << endl;
   } else {
      erasm::cout << "Disassembly ended at:" 
		  << hex << (void*)end << endl;
   }
   return 0;
}


