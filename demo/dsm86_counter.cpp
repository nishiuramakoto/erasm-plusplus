#include <erasm/dsm_x86_util.hpp>
#include <erasm/dsm_x86.hpp>

using namespace erasm::x86;
using namespace erasm::x86::addr32::data32;

struct MyCounter : public InstructionCounter
{
   InstructionCounter(const_code_ptr start) 
      : InstructionCounter(start)
      {}

   // Let the base class handle the default cases 
   using InstructionCounter::action;
   // 
   using InstructionCounter::counter_;

   action_result_type
   action(const Ret& insn)
      {
	 counter_++;
	 return finish(insn);
      }

   template<class Op>
   action_result_type
   action(const Jmp& insn,const Op& op)
      {
	 counter_++;
	 return finish(insn);
      }
};
