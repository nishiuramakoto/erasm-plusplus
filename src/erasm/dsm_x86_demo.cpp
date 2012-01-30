#include <erasm/dsm_x86_util.hpp>
#include <erasm/faststream.hpp>
#include <iostream>

using namespace std;
using namespace erasm::x86;
using namespace erasm::x86::addr32::data32;

struct MyDsm : public SimpleDsm
{
   MyDsm(const_code_ptr start,ostream& os = erasm::cout)
      : SimpleDsm(start,os)
      {}

   using SimpleDsm::action;

   action_result_type
   action(const Ret& insn)
      {
	 os() << "Statically overriding the ret instruction matcher" << endl;
	 print_instruction(os(),insn) << endl;
	 return finish(insn);
      }
};


int main(int argc,char**argv)
{
   const_code_ptr p = (const_code_ptr) main;
   MyDsm   mydsm(p);

   const_code_ptr end = decode(p,mydsm);

   if (mydsm.failed()) {
      erasm::cout << "Disassembly failed at:"<< hex << (uint32_t)end << endl;
   } else {
      erasm::cout << "Disassembly ended at:" << hex << (uint32_t)end << endl;
   }
   return 0;
}


