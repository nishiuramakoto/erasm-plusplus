#ifndef ERASM_DSM_X64_UTIL_HPP
#define ERASM_DSM_X64_UTIL_HPP

#include <erasm/x64_instruction_definition_common.hpp>
#include <erasm/x64_io.hpp>

namespace erasm { namespace x64 {

class DsmBase
{
public:
   DsmBase(const_code_ptr start,const_code_ptr end = 0) 
      : start_(start),end_(end), failed_(false)
      {}

   const_code_ptr get_start_address() const
      { return start_ ; }

   const_code_ptr get_end_address() const
      { return end_ ; }

   bool failed() const
      { return failed_ ; }


   template<class Insn>
   action_result_type
   action(const Insn& insn)
      { return next(insn); }

   template<class Insn,class Op1>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1)
      { return next(insn); }

   template<class Insn,class Op1,class Op2>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2)
      { return next(insn); }

   template<class Insn,class Op1,class Op2,class Op3>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2,
	  const Op3& op3)
      {  return next(insn);  }


   const_code_ptr
   error(const InstructionData& data)
      {
	 failed_ = true;
	 return data.start;
      }


protected:
   action_result_type finish(const InstructionData& data)
      { return std::make_pair(data.end, ACTION_FINISH);  }

   action_result_type next(const InstructionData& data)
      { return std::make_pair(data.end, ACTION_CONTINUE); }

   action_result_type
   check(const InstructionData& data) const
      {
	 return std::make_pair(data.end,
			       data.end < end_ ? 
			       ACTION_CONTINUE : ACTION_FINISH );
      }

   const_code_ptr start_;
   const_code_ptr end_;
   bool failed_;
};


class InstructionCounter : public DsmBase
{

public:
   typedef DsmBase base;

   InstructionCounter(const_code_ptr start,const_code_ptr end = 0) 
      : base(start,end),counter_(0)
      {}

   int get_counter() const
      { return counter_ ; }

   
   template<class Insn>
   action_result_type
   action(const Insn& insn)
      {
	 counter_ ++;
	 return next(insn);
      }

   template<class Insn,class Op1>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1)
      {
	 counter_ ++;
	 return next(insn);
      }

   template<class Insn,class Op1,class Op2>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2)
      {
	 counter_ ++;
	 return next(insn);
      }

   template<class Insn,class Op1,class Op2,class Op3>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2,
	  const Op3& op3)
      {
	 counter_ ++;
	 return next(insn);
      }
   
protected:
   int counter_;
};



template<std::ostream& CodePrinter(std::ostream& os,
				   const_code_ptr start,
				   const_code_ptr end) = print_code>
class SimpleDsm : public InstructionCounter
{
public:
   typedef InstructionCounter base;

   SimpleDsm(const_code_ptr start,std::ostream& os) 
      : base(start)
      , os_(os) 
      { }

   std::ostream& os() 
      { return os_ ; }

   const std::ostream& os() const
      { return os_ ; }

   template<class Insn>
   action_result_type
   action(const Insn& insn)
      {
	 print_code(os(),insn);
	 print_instruction(os_,insn) << std::endl;
	 return base::action(insn);
      }

   template<class Insn,class Op1>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1)
      {
	 print_code(os(),insn);
	 print_instruction(os(),insn,op1) << std::endl;
	 return base::action(insn,op1);
      }

   template<class Insn,class Op1,class Op2>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2)
      {
	 print_code(os(),insn);
	 print_instruction(os(),insn,op1,op2) << std::endl;
	 return base::action(insn,op1,op2);
      }

   template<class Insn,class Op1,class Op2,class Op3>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2,
	  const Op3& op3)
      {
	 print_code(os(),insn);
	 print_instruction(os(),insn,op1,op2,op3) << std::endl;
	 return base::action(insn,op1,op2,op3);
      }

   const_code_ptr
   error(const InstructionData& data)
      {
	 x64::print_code(os(),data.start,data.start+5);
	 os() << "decode error:"   << data.action_code << std::endl;
	 os() << "decoded length:" << data.start - start_ << std::endl;
	 return base::error(data);
      }

protected:

   std::ostream& print_code(std::ostream& os,const InstructionData& insn)
      { return CodePrinter(os,insn.start,insn.end) ; }


   std::ostream& os_;
};




}}

#endif // ERASM_DSM_X64_UTIL_HPP
