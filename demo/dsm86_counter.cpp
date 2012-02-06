#include <erasm/dsm_x86_util.hpp>
#include <erasm/dsm_x86.hpp>
#include <iostream>

using namespace std;
using namespace erasm::x86;
using namespace erasm::x86::addr32::data32;

struct MyCounter : public InstructionCounter
{
   MyCounter(const_code_ptr start) 
      : InstructionCounter(start)
      {}

   // Let the base class handle the default cases 
   using InstructionCounter::action;

   action_result_type
   action(const Ret& insn)
      {
	 inc_counter();
	 return finish(insn);
      }

   template<class Op>
   action_result_type
   action(const Jmp& insn,const Op& op)
      {
	 inc_counter();
	 return finish(insn);
      }
};

int main()
{
   const_code_ptr p = (const_code_ptr) main;
   MyCounter   mydsm(p);

   const_code_ptr end = decode(p,mydsm);

   if (mydsm.failed()) {
      cerr << "Disassembly failed at:" 
	   << hex << (void*)end << endl;
      return 1;
   } 

   cout << "number of instructions = " << mydsm.get_counter() << endl;
   
   return 0;
}
