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
#ifndef MY_ERASM_DSM_X64_HPP
#define MY_ERASM_DSM_X64_HPP

#include "erasm/x64_instruction_definition.hpp"

#define ERASM_DECODER_DO_ACTION(OPCODE_LEN,INSN,OP1,ENC1,OP2,ENC2,OP3,ENC3) \
{									\
   p = opcode_start + OPCODE_LEN;					\
   ActionDispatcher<Action,						\
		    INSN,						\
		    OP1,ENC1,						\
		    OP2,ENC2,						\
		    OP3,ENC3>  dispatcher(action,params);		\
									\
   action_result_type result =  dispatcher . action();			\
									\
   p            = result.first;						\
   action_code  = result.second;					\
   continue;								\
}

// Seems not worth the trouble,so this will go away
#define ERASM_DECODER_SET_PARAMS			\
{						\
   params.start =  start;		\
   params.opcode_start = opcode_start;		\
   params.end = end;			\
   params.end = p;				\
   params.prefix = prefix;			\
   params.rex = rex;				\
   params.segment = segment;			\
   params.is_data16 = is_data16;			\
   params.is_addr32 = is_addr32;		\
   params.action_code = action_code;		\
}

#define ERASM_DECODER_PREFIX(PREFIX)		\
{						\
   opcode_start ++;				\
   prefix. PREFIX = 1;				\
   goto decode_start;				\
}


#define ERASM_PP_CONCAT(X,Y) ERASM_PP_CONCAT_(X,Y)
#define ERASM_PP_CONCAT_(X,Y) X ## Y

#define ERASM_DECODER_SEGMENT_PREFIX(U,L)	\
   {						\
      opcode_start ++;				\
      prefix. L = 1;				\
      segment = ERASM_PP_CONCAT(Segment_,U);	\
      goto decode_start;			\
   }

#define ERASM_DECODER_OPERAND_SIZE		\
{						\
   opcode_start ++;				\
   prefix.operand_size = 1;			\
   is_data16 = ! is_data16;				\
   goto decode_start;				\
}

#define ERASM_DECODER_ADDRESS_SIZE		\
{						\
   opcode_start ++;				\
   prefix.address_size = 1;			\
   is_addr32 = ! is_addr32;			\
   goto decode_start;				\
}


#define ERASM_DECODER_REX			\
   {						\
      opcode_start ++;				\
      rex.byte = p[-1];				\
      goto decode_start;			\
   }

#define ERASM_DECODER_REXW			\
{						\
   opcode_start ++;				\
   rex.byte = p[-1];				\
   goto decode_start;				\
}


namespace erasm { namespace x64 {

namespace impl {

using namespace std;

struct OpRel8
{
   typedef rel8_t operand_type;
};
struct OpRel16
{
   typedef rel16_t operand_type;
};
struct OpRel32
{
   typedef rel32_t operand_type;
};
struct OpRel64
{
   typedef rel64_t operand_type;
};

struct OpIMM8
{
   typedef imm8_t operand_type;
};

struct OpIMM16
{
   typedef imm16_t operand_type;
};
struct OpIMM32
{
   typedef imm32_t operand_type;
};
struct OpIMM64
{
   typedef imm64_t operand_type;
};

struct OpFar32 ;
struct OpFar64 ;

struct  OpR8
{
   typedef ByteReg86 operand_type;
};

struct  OpR8_Rex
{
   typedef ByteRegRex operand_type;
};

struct  OpR16
{
   typedef WordReg operand_type;
};

struct  OpR32
{
   typedef DwordReg operand_type;
};

struct  OpR64
{
   typedef QwordReg operand_type;
};

struct  OpST
{
   typedef StReg operand_type;
};

struct  OpMM
{
   typedef MmReg operand_type;
};

struct  OpXMM
{
   typedef XmmReg operand_type;
};

struct  OpCR
{
   typedef CrReg operand_type;
};

struct  OpDR
{
   typedef DrReg operand_type;
};

struct  OpSReg
{
   typedef SegReg operand_type;
};

struct  OpRDefault
{
   typedef QwordReg operand_type;
};


struct  OpRUnused ;


struct OpAL
{
   typedef RegAL operand_type;
};

struct  OpAX
{
   typedef RegAX operand_type;
};
struct OpEAX
{
   typedef RegEAX operand_type;
};
struct OpRAX
{
   typedef RegRAX operand_type;
};
struct OpDX
{
   typedef RegDX operand_type;
};
struct  OpCL
{
   typedef RegCL operand_type;
};
struct  OpST0
{
   typedef RegST0 operand_type;
};
struct  OpXMM0
{
   typedef RegXMM0 operand_type;
};
struct  OpDS
{
   typedef RegDS operand_type;
};
struct  OpES
{
   typedef RegES operand_type;
};
struct  OpSS
{
   typedef RegSS operand_type;
};
struct  OpFS
{
   typedef RegFS operand_type;
};
struct  OpGS
{
   typedef RegGS operand_type;
};
struct  OpCS
{
   typedef RegCS operand_type;
};


struct  OpRM8
{
   typedef False ptr_only;
   typedef ByteReg86 reg_operand_type;
   typedef BytePtr   ptr_operand_type;
};

struct  OpR8_Rex_M8
{
   typedef False ptr_only;
   typedef ByteRegRex reg_operand_type;
   typedef BytePtr    ptr_operand_type;
};

struct  OpRM16
{
   typedef False ptr_only;
   typedef WordReg reg_operand_type;
   typedef WordPtr ptr_operand_type;
};

struct  OpRM32
{
   typedef False ptr_only;
   typedef DwordReg reg_operand_type;
   typedef DwordPtr ptr_operand_type;
};

struct  OpRM64
{
   typedef False ptr_only;
   typedef QwordReg reg_operand_type;
   typedef QwordPtr ptr_operand_type;
};

struct  OpXMM_M16
{
   typedef False ptr_only;
   typedef XmmReg  reg_operand_type;
   typedef WordPtr ptr_operand_type;
};

struct  OpXMM_M32
{
   typedef False ptr_only;
   typedef XmmReg    reg_operand_type;
   typedef DwordPtr  ptr_operand_type;
};

struct  OpXMM_M64
{
   typedef False ptr_only;
   typedef XmmReg    reg_operand_type;
   typedef QwordPtr  ptr_operand_type;
};

struct  OpXMM_M128
{
   typedef False ptr_only;
   typedef XmmReg      reg_operand_type;
   typedef XmmWordPtr  ptr_operand_type;
};


struct  OpMM_M32
{
   typedef False ptr_only;
   typedef MmReg      reg_operand_type;
   typedef DwordPtr   ptr_operand_type;
};

struct  OpMM_M64
{
   typedef False ptr_only;
   typedef MmReg      reg_operand_type;
   typedef QwordPtr   ptr_operand_type;
};

struct  OpMM_M128
{
   typedef False ptr_only;
   typedef MmReg      reg_operand_type;
   typedef QwordPtr   ptr_operand_type;
};

struct  OpRDefault_M8
{
   typedef False ptr_only;
   typedef QwordReg   reg_operand_type;
   typedef BytePtr    ptr_operand_type;
};

struct  OpRDefault_M16
{
   typedef False ptr_only;
   typedef QwordReg  reg_operand_type;
   typedef WordPtr   ptr_operand_type;
};
struct  OpRDefault_M32
{
   typedef False ptr_only;
   typedef QwordReg  reg_operand_type;
   typedef DwordPtr  ptr_operand_type;
};
struct  OpR32_M8
{
   typedef False ptr_only;
   typedef DwordReg  reg_operand_type;
   typedef BytePtr   ptr_operand_type;
};
struct  OpR32_M16
{
   typedef False ptr_only;
   typedef DwordReg  reg_operand_type;
   typedef WordPtr   ptr_operand_type;
};

struct  OpR64_R32_M32
{
   typedef False ptr_only;
   typedef QwordReg reg_operand_type;
   typedef DwordPtr ptr_operand_type;
};
struct  OpR64_M16
{
   typedef False ptr_only;
   typedef QwordReg reg_operand_type;
   typedef WordPtr  ptr_operand_type;
};

struct  OpM
{
   typedef True ptr_only;
   typedef VoidPtr ptr_operand_type;
};

struct  OpM8
{
   typedef True ptr_only;
   typedef BytePtr ptr_operand_type;
};

struct  OpM16
{
   typedef True ptr_only;
   typedef WordPtr ptr_operand_type;
};

struct  OpM32
{
   typedef True ptr_only;
   typedef DwordPtr ptr_operand_type;
};

struct  OpM64
{
   typedef True ptr_only;
   typedef QwordPtr ptr_operand_type;
};
struct  OpM128
{
   typedef True ptr_only;
   typedef OwordPtr ptr_operand_type;
};

struct  OpM128Xmm
{
   typedef True ptr_only;
   typedef XmmWordPtr ptr_operand_type;
};

struct  OpMFar16
{
   typedef True ptr_only;
   typedef DwordPtr ptr_operand_type;
};
struct OpMFar32
{
   typedef True ptr_only;
   typedef FwordPtr ptr_operand_type;
};
struct  OpMFar64
{
   typedef True ptr_only;
   typedef TbytePtr ptr_operand_type;
};

struct  OpM16_16
{
   typedef True ptr_only;
   typedef DwordPtr ptr_operand_type;
};

struct  OpM16_32
{
   typedef True ptr_only;
   typedef FwordPtr ptr_operand_type;
};

struct  OpM16_64
{
   typedef True ptr_only;
   typedef TbytePtr ptr_operand_type;
};

struct   OpM32_32
{
   typedef True ptr_only;
   typedef QwordPtr ptr_operand_type;
};


struct  OpM32FP
{
   typedef True ptr_only;
   typedef DwordPtr ptr_operand_type;
};
struct OpM64FP
{
   typedef True ptr_only;
   typedef QwordPtr ptr_operand_type;
};
struct OpM80FP
{
   typedef True ptr_only;
   typedef TbytePtr ptr_operand_type;
};

struct  OpM16Int
{
   typedef True ptr_only;
   typedef WordPtr ptr_operand_type;
};

struct OpM32Int
{
   typedef True ptr_only;
   typedef DwordPtr ptr_operand_type;
};

struct OpM64Int
{
   typedef True ptr_only;
   typedef QwordPtr ptr_operand_type;
};

struct  OpM80Dec
{
   typedef True ptr_only;
   typedef TbytePtr ptr_operand_type;
};

struct  OpM80BCD
{
   typedef True ptr_only;
   typedef TbytePtr ptr_operand_type;
};

struct  OpM2byte
{
   typedef True ptr_only;
   typedef WordPtr ptr_operand_type;
};

struct  OpM512byte
{
   typedef True ptr_only;
   typedef VoidPtr ptr_operand_type;
};

struct  OpM94_108byte
{
   typedef True ptr_only;
   typedef VoidPtr ptr_operand_type;
};

struct  OpM14_28byte
{
   typedef True ptr_only;
   typedef VoidPtr ptr_operand_type;
};

struct  OpMOffs8
{
   typedef ByteOffset64 offset64_type;
   typedef ByteOffset32 offset32_type;
};
struct OpMOffs16
{
   typedef WordOffset64 offset64_type;
   typedef WordOffset32 offset32_type;
};
struct  OpMOffs32
{
   typedef DwordOffset64 offset64_type;
   typedef DwordOffset32 offset32_type;
};
struct   OpMOffs64
{
   typedef QwordOffset64 offset64_type;
   typedef QwordOffset32 offset32_type;
};

struct  Op1
{
   typedef One operand_type;
} ;
struct  Op0 
{
   typedef Zero operand_type;
};

struct  OpM8_ES_DI_EDI_RDI 
{
   typedef BytePtr_ES_RDI ptr_es_rdi_type;
   typedef BytePtr_ES_EDI ptr_es_edi_type;
};

struct  OpM8_DS_SI_ESI_RSI
{
   typedef BytePtr_RSI ptr_rsi_type;
   typedef BytePtr_ESI ptr_esi_type;
};

struct  OpM16_ES_DI_EDI_RDI 
{
   typedef WordPtr_ES_RDI ptr_es_rdi_type;
   typedef WordPtr_ES_EDI ptr_es_edi_type;
};

struct  OpM16_DS_SI_ESI_RSI
{ 
   typedef WordPtr_RSI ptr_rsi_type;
   typedef WordPtr_ESI ptr_esi_type;
};

struct  OpM32_ES_DI_EDI_RDI 
{
   typedef DwordPtr_ES_RDI ptr_es_rdi_type;
   typedef DwordPtr_ES_EDI ptr_es_edi_type;
};

struct  OpM32_DS_SI_ESI_RSI
{
   typedef DwordPtr_RSI ptr_rsi_type;
   typedef DwordPtr_ESI ptr_esi_type;
};

struct  OpM64_ES_DI_EDI_RDI 
{
   typedef QwordPtr_ES_RDI ptr_es_rdi_type;
   typedef QwordPtr_ES_EDI ptr_es_edi_type;
};

struct  OpM64_DS_SI_ESI_RSI
{
   typedef QwordPtr_RSI ptr_rsi_type;
   typedef QwordPtr_ESI ptr_esi_type;
};

struct  OpM8_DS_BX_EBX_RBX
{
   typedef BytePtr_EBX ptr_ebx_type;
   typedef BytePtr_RBX ptr_rbx_type;
};


struct EncModrm_rm   {};
struct EncModrm_rm_mem   {};
struct EncModrm_rm_reg   {};
struct EncModrm_reg  {};
struct EncReg    {};
struct EncIMM    {};
struct EncOffset    {};
struct EncDisplacement    {};
struct EncDS_SI_ESI_RSI    {};
struct EncES_DI_EDI_RDI    {};
struct Enc1    {};
struct EncImplicit {};


inline bool is_rex_byte(uint8_t x)
{ return 0x40 <= x && x <= 0x4f ; }

inline unsigned int modrm_mod(const_code_pointer_type p)
{ return ModrmConstHandle(p,ADDRESS_MODE_64).get_mod() ; }

inline bool modrm_mod3(const_code_pointer_type p)
{ return ModrmConstHandle(p,ADDRESS_MODE_64).get_mod() == 3; }

inline unsigned int modrm_reg_op(const_code_pointer_type p)
{ return ModrmConstHandle(p,ADDRESS_MODE_64).get_op_reg() ; }

inline unsigned int modrm_rm(const_code_pointer_type p)
{ return ModrmConstHandle(p,ADDRESS_MODE_64).get_rm() ; }

inline unsigned int imm(const_code_pointer_type p,bool addr32)
{    
   int n = ModrmConstHandle(p,ADDRESS_MODE_64).size();
   return p[n];
}


template<typename X>
inline
pair<int,X> decode_imm_at(const_code_ptr p)
{
   X v = *(const X *)p;
   return make_pair(sizeof(X),v) ;
}


template<class Action,
	 class Insn>
struct DispatcherHelper
{
   Action		& action;
   Insn			& params;
   const_code_ptr	& p;

   enum { is16bit = false  };

   DispatcherHelper(Action& action,InstructionData& params)
      : action(action) 
	// TODO:Ugly,find a better way 
	// (which must have no run-time overhead,of course)
      , params(static_cast<Insn&> (params)) 
      , p(params.end)
      {}


   template<class Reg>
   Reg decode_regadd()
      {

	 return Reg(modrm_rm(p-1), params.rex.b);
      }

   template<class Moffs>
   Moffs decode_moffset32()
      {
	 return Moffs(decode_imm<imm32_t>(),
		      get_segment());
      }

   template<class Moffs>
   Moffs decode_moffset64()
      {
	 return Moffs(decode_imm<imm64_t>(),
		      get_segment());
      }

   // This requires explicit instantiation
   template <class X>
   X decode_imm()
      {
	 std::pair<int,X> v = decode_imm_at<X>(p);
	 p +=  v.first;
	 return v.second;
      }

   // While this is not
   template <class X>
   void decode_imm(X& x)
      {
	 std::pair<int,X> v = decode_imm_at<X>(p);
	 p +=  v.first;
	 x = v.second;
      }



   action_result_type
   dispatch()
      {
	 return action.action(params);
      }

   template<class Op1>
   action_result_type
   dispatch(const Op1& op1)
      {
	 return action.action(params,op1);
      }

   template<class Op1,class Op2>
   action_result_type
   dispatch(const Op1& op1,const Op2& op2)
      {
	 return action.action(params,op1,op2);
      }

   template<class Op1,class Op2,class Op3>
   action_result_type
   dispatch(const Op1& op1,const Op2& op2,const Op3& op3)
      {
	 return action.action(params,op1,op2,op3);
      }

   bool is_addr32() const
      { return params.is_addr32 ; }

   const RexByte& get_rex() const
      { return params.rex ; }

   bool get_rex_b() const
      { return params.rex.b ; }

   bool get_rex_x() const
      { return params.rex.x ; }

   Segment get_segment() const
      { return params.segment ; }
};



template<class Action,
	 class Insn,
	 class RM,
	 class Enc = EncModrm_rm>
struct DispatcherHelperModrm
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;
   typedef typename RM::reg_operand_type modrm_reg_operand_type;
   typedef typename RM::ptr_operand_type modrm_ptr_operand_type;

   DispatcherHelperModrm(Action& action,InstructionData& params)
      : base(action,params) 
      , modrm(params.end,params.is_addr32 ? ADDRESS_MODE_32 : ADDRESS_MODE_64)
      {
	 base::p += modrm.size();
      }

   modrm_reg_operand_type get_modrm_reg_operand() const
      {
	 return  modrm_reg_operand_type(modrm.get_rm(),
					base::get_rex_b()) ;
      };

   modrm_ptr_operand_type get_modrm_ptr_operand() const
      {
	 return modrm_ptr_operand_type(modrm,
				       base::get_segment(),
				       base::get_rex());

      }


   ModrmConstHandle modrm;
};


template<class Action,
	 class Insn,
	 class RM>
struct DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_mem>
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;
   typedef typename RM::ptr_operand_type modrm_ptr_operand_type;

   DispatcherHelperModrm(Action& action,InstructionData& params)
      : base(action,params) 
      , modrm(params.end,params.is_addr32 ? ADDRESS_MODE_32 : ADDRESS_MODE_64)
      {
	 base::p += modrm.size();
      }

   modrm_ptr_operand_type get_modrm_ptr_operand() const
      {
	 assert(modrm.get_mod() != 3);
	 return modrm_ptr_operand_type(modrm,
				       base::get_segment(),
				       base::get_rex());
      }


   ModrmConstHandle modrm;
};

template<class Action,
	 class Insn,
	 class RM>
struct DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg>
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;
   typedef typename RM::operand_type modrm_reg_operand_type;

   DispatcherHelperModrm(Action& action,InstructionData& params)
      : base(action,params)
      , modrm(params.end,params.is_addr32 ? ADDRESS_MODE_32 : ADDRESS_MODE_64)
      {
	 base::p += modrm.size();
      }

   modrm_reg_operand_type get_modrm_reg_operand() const
      {
	 assert(modrm.get_mod() == 3);
	 return  modrm_reg_operand_type(modrm.get_rm(),
					base::get_rex().b) ;
      };

   ModrmConstHandle modrm;
};

template<class Action,
	 class Insn>
struct DispatcherHelperModrm<Action,Insn,OpRUnused,EncModrm_rm_reg>
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   DispatcherHelperModrm(Action& action,InstructionData& params)
      : base(action,params) 
      , modrm(params.end,params.is_addr32 ? ADDRESS_MODE_32 : ADDRESS_MODE_64)
      {
	 base::p += modrm.size();
      }

   ModrmConstHandle modrm;
};




template<class Action,
	 class Insn,
	 class OperandType1 = Nothing , class EncodingType1 = Nothing,
	 class OperandType2 = Nothing , class EncodingType2 = Nothing,
	 class OperandType3 = Nothing , class EncodingType3 = Nothing >
struct ActionDispatcher ;



template<class Action, class Insn >
struct ActionDispatcher<Action,Insn>
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 return base::dispatch();
      }
};



template<class Action,
	 class Insn, class Reg>
struct ActionDispatcher<Action,
			Insn,
			Just<Reg>,Just<EncImplicit> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Reg::operand_type op1_type;
	 op1_type op1;
	 return dispatch(op1);
      }
};

template<class Action,
	 class Insn, class Reg,class IMM>
struct ActionDispatcher<Action,
			Insn,
			Just<Reg>,Just<EncReg> ,
			Just<IMM>,Just<EncIMM> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Reg::operand_type op1_type;
	 typedef typename IMM::operand_type op2_type;

	 op1_type op1 = base::template decode_regadd<op1_type>();
	 op2_type op2 = base::template decode_imm<op2_type>();

	 return dispatch(op1,op2);
      }
};


template<class Action,
	 class Insn, class Reg,class IMM>
struct ActionDispatcher<Action,
			Insn,
			Just<IMM>,Just<EncIMM> ,
			Just<Reg>,Just<EncImplicit> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename IMM::operand_type op1_type;
	 typedef typename Reg::operand_type op2_type;
	 op1_type op1;
	 op2_type op2;
	 decode_imm(op1);
	 return dispatch(op1,op2);
      }
};



template<class Action,
	 class Insn, class Reg1,class Reg2>
struct ActionDispatcher<Action,
			Insn,
			Just<Reg1> ,Just<EncImplicit> ,
			Just<Reg2> ,Just<EncImplicit> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Reg1::operand_type op1_type;
	 typedef typename Reg2::operand_type op2_type;
	 op1_type op1;
	 op2_type op2;
	 return dispatch(op1,op2);
      }
};


template<class Action,
	 class Insn, class Reg0,class Reg>
struct ActionDispatcher<Action,
			Insn,
			Just<Reg0> , Just<EncImplicit>,
			Just<Reg>  , Just<EncReg> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Reg0::operand_type op1_type;
	 typedef typename Reg ::operand_type op2_type;
	 op1_type op1;
	 return dispatch(op1,base::template decode_regadd<op2_type>());
      }
};

template<class Action,
	 class Insn, class Reg0,class Reg>
struct ActionDispatcher<Action,
			Insn,
			Just<Reg>  , Just<EncReg> ,
			Just<Reg0> , Just<EncImplicit> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Reg ::operand_type op1_type;
	 typedef typename Reg0::operand_type op2_type;

	 op2_type op2;
	 return dispatch(base::template decode_regadd<op1_type>(),op2);
      }
};

template<class Action,
	 class Insn, class Imm
	 >
struct ActionDispatcher<Action,
			Insn,
			Just<Imm>,Just<EncIMM> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Imm::operand_type op1_type;
	 op1_type op1;
	 decode_imm(op1);
	 return dispatch(op1);
      }
};

template<class Action,
	 class Insn, class Imm,class Imm2
	 >
struct ActionDispatcher<Action,
			Insn,
			Just<Imm> ,Just<EncIMM> ,
			Just<Imm2>,Just<EncIMM> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Imm ::operand_type op1_type;
	 typedef typename Imm2::operand_type op2_type;
	 op1_type op1;
	 op2_type op2;
	 decode_imm(op1);
	 decode_imm(op2);
	 return dispatch(op1,op2);
      }
};



template<class Action,
	 class Insn, class Reg,class Imm
	 >
struct ActionDispatcher<Action,
			Insn,
			Just<Reg>,Just<EncImplicit>,
			Just<Imm>,Just<EncIMM> >
: public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Reg::operand_type op1_type;
	 typedef typename Imm::operand_type op2_type;
	 op1_type op1;
	 op2_type op2;

	 decode_imm(op2);
	 return dispatch(op1,op2);
      }
};



template<class Action,
	 class Insn,
	 class RM,
	 class IMM>
struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm> ,
			Just<IMM>    , Just<EncIMM> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename IMM::operand_type     op2_type;

	 op2_type op2;
	 decode_imm(op2);

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(base::get_modrm_reg_operand(),op2);
	 } else {
	    return dispatch(base::get_modrm_ptr_operand(),op2);
	 }
      }
};


template<class Action,
	 class Insn,
	 class RM,
	 class IMM>
struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm_reg> ,
			Just<IMM> , Just<EncIMM> >
   : public DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg>
{
   typedef DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg>
   base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename IMM::operand_type     op2_type;

	 op2_type op2;
	 decode_imm(op2);

	 return dispatch(base::get_modrm_reg_operand(),op2);
      }
};


template<class Action,
	 class Insn,
	 class RM,
	 class R>
struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm> ,
			Just<R>   , Just<EncModrm_reg> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op2_type;
	 op2_type op2(base::modrm.get_op_reg(),base::get_rex().r);

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(base::get_modrm_reg_operand(),op2);
	 } else {
	    return dispatch(base::get_modrm_ptr_operand(),op2);
	 }
      }
};

template<class Action,
	 class Insn,
	 class RM>
struct ActionDispatcher<Action,
			Insn,
			Just<RM>          , Just<EncModrm_rm> ,
			Just<OpRUnused>   , Just<EncModrm_reg> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 if (base::modrm.get_mod() == 3) {
	    return dispatch(base::get_modrm_reg_operand());
	 } else {
	    return dispatch(base::get_modrm_ptr_operand());
	 }
      }
};

template<class Action,
	 class Insn,
	 class R,
	 class RM>
struct ActionDispatcher<Action,
			Insn,
			Just<R>   , Just<EncModrm_reg> ,
			Just<RM>  , Just<EncModrm_rm> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op1_type;
	 op1_type op1(base::modrm.get_op_reg(),base::get_rex().r);

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(op1,base::get_modrm_reg_operand());
	 } else {
	    return dispatch(op1,base::get_modrm_ptr_operand());
	 }
      }
};

template<class Action,
	 class Insn,
	 class R,
	 class RM>
struct ActionDispatcher<Action,
			Insn,
			Just<R>   , Just<EncModrm_reg> ,
			Just<RM>  , Just<EncModrm_rm_mem> >
   : public DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_mem>
{
   typedef DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_mem> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op1_type;
	 op1_type op1(base::modrm.get_op_reg(),base::get_rex().r);

	 return dispatch(op1,base::get_modrm_ptr_operand());
      }
};

template<class Action,
	 class Insn,
	 class RM,
	 class R>

struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm_mem> ,
			Just<R>   , Just<EncModrm_reg> >

   : public DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_mem>
{
   typedef DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_mem> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op2_type;
	 op2_type op2(base::modrm.get_op_reg(),base::get_rex().r);

	 return dispatch(base::get_modrm_ptr_operand(),op2);
      }
};

template<class Action,
	 class Insn,
	 class R,
	 class RM>
struct ActionDispatcher<Action,
			Insn,
			Just<R>   , Just<EncModrm_reg> ,
			Just<RM>  , Just<EncModrm_rm_reg> >
   : public DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg>
{
   typedef DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg>
   base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op1_type;
	 op1_type op1(base::modrm.get_op_reg(),base::get_rex().r);

	 return dispatch(op1,base::get_modrm_reg_operand());
      }
};

template<class Action,
	 class Insn,
	 class RM,
	 class R>
struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm_reg> ,
			Just<R>   , Just<EncModrm_reg> >
   : public DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg>
{
   typedef DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op2_type;
	 op2_type op2(base::modrm.get_op_reg(),base::get_rex().r);

	 return dispatch(base::get_modrm_reg_operand(),op2);
      }
};



template<class Action,
	 class Insn,
	 class R,
	 class RM,
	 class IMM>
struct ActionDispatcher<Action,
			Insn,
			Just<R>   , Just<EncModrm_reg> ,
			Just<RM>  , Just<EncModrm_rm> ,
			Just<IMM> , Just<EncIMM> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op1_type;
	 typedef typename IMM::operand_type     op3_type;

	 op1_type op1(base::modrm.get_op_reg(),base::get_rex().r);
	 op3_type op3;

	 base::decode_imm(op3);

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(op1,base::get_modrm_reg_operand(),op3);
	 } else {
	    return dispatch(op1,base::get_modrm_ptr_operand(),op3);
	 }
      }
};

template<class Action,
	 class Insn,
	 class R,
	 class RM>
struct ActionDispatcher<Action,
			Insn,
			Just<R>   , Just<EncModrm_reg> ,
			Just<RM>  , Just<EncModrm_rm> ,
			Nothing   , Just<EncIMM> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op1_type;
	 typedef  imm8_t     op3_type;

	 op1_type op1(base::modrm.get_op_reg(),base::get_rex().r);
	 op3_type op3;

	 base::decode_imm(op3);

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(op1,base::get_modrm_reg_operand());
	 } else {
	    return dispatch(op1,base::get_modrm_ptr_operand());
	 }
      }
};

template<class Action,
	 class Insn,
	 class R,
	 class RM,
	 class IMM>
struct ActionDispatcher<Action,
			Insn,
			Just<R>   , Just<EncModrm_reg> ,
			Just<RM>  , Just<EncModrm_rm_reg> ,
			Just<IMM> , Just<EncIMM> >
   : public DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg>
{
   typedef DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op1_type;
	 typedef typename IMM::operand_type     op3_type;

	 op1_type op1(base::modrm.get_op_reg(),base::get_rex().r);
	 op3_type op3;

	 base::decode_imm(op3);

	 return dispatch(op1,base::get_modrm_reg_operand(),op3);
      }
};


template<class Action,
	 class Insn,
	 class RM,
	 class R,
	 class IMM>
struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm> ,
			Just<R>   , Just<EncModrm_reg> ,
			Just<IMM> , Just<EncIMM> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R  ::operand_type       op2_type;
	 typedef typename IMM::operand_type      op3_type;

	 op2_type op2(base::modrm.get_op_reg(),base::get_rex().r);
	 op3_type op3;

	 base::decode_imm(op3);

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(base::get_modrm_reg_operand(),op2,op3);
	 } else {
	    return dispatch(base::get_modrm_ptr_operand(),op2,op3);
	 }
      }
};

template<class Action,
	 class Insn,
	 class RM,
	 class R>

struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm> ,
			Just<R>   , Just<EncModrm_reg> ,
			Nothing   , Just<EncIMM> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R  ::operand_type       op2_type;
	 typedef imm8_t      op3_type;

	 op2_type op2(base::modrm.get_op_reg(),base::get_rex().r);
	 op3_type op3;

	 base::decode_imm(op3);

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(base::get_modrm_reg_operand(),op2);
	 } else {
	    return dispatch(base::get_modrm_ptr_operand(),op2);
	 }
      }
};


template<class Action,
	 class Insn,
	 class R,class RM,class R2>
struct ActionDispatcher<Action,
			Insn,
			Just<R>   , Just<EncModrm_reg> ,
			Just<RM>  , Just<EncModrm_rm> ,
			Just<R2>  , Just<EncImplicit> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op1_type;
	 typedef typename R2::operand_type      op3_type;

	 op1_type op1(base::modrm.get_op_reg(),base::get_rex().r);
	 op3_type op3;

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(op1,base::get_modrm_reg_operand(),op3);
	 } else {
	    return dispatch(op1,base::get_modrm_ptr_operand(),op3);
	 }
      }
};

template<class Action,
	 class Insn,
	 class R,class RM,class R2>
struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm> ,
			Just<R>   , Just<EncModrm_reg> ,
			Just<R2>  , Just<EncImplicit> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op2_type;
	 typedef typename R2::operand_type      op3_type;

	 op2_type op2(base::modrm.get_op_reg(),base::get_rex().r);
	 op3_type op3;

	 if (base::modrm.get_mod() == 3) {
	    return dispatch(base::get_modrm_reg_operand(),op2,op3);
	 } else {
	    return dispatch(base::get_modrm_ptr_operand(),op2,op3);
	 }
      }
};

template<class Action,
	 class Insn,
	 class RM,
	 class R>
struct ActionDispatcher<Action,
			Insn,
			Just<RM>  , Just<EncModrm_rm> ,
			Just<R>   , Just<EncImplicit> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op2_type;
	 op2_type op2;
	 if (base::modrm.get_mod() == 3) {
	    return dispatch(base::get_modrm_reg_operand(),op2);
	 } else {
	    return dispatch(base::get_modrm_ptr_operand(),op2);
	 }
      }
};

template<class Action,
	 class Insn,
	 class R,
	 class RM>
struct ActionDispatcher<Action,
			Insn,
			Just<R>   , Just<EncImplicit> ,
			Just<RM>  , Just<EncModrm_rm> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type       op1_type;
	 op1_type op1;
	 if (base::modrm.get_mod() == 3) {
	    return dispatch(op1,base::get_modrm_reg_operand());
	 } else {
	    return dispatch(op1,base::get_modrm_ptr_operand());
	 }
      }
};



template<class Action,
	 class Insn,
	 class R>
struct ActionDispatcher<Action,
			Insn,
			Just<R>  , Just<EncReg> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename R::operand_type op_type;
	 return dispatch(base::template decode_regadd<op_type>());
      }
};

template<class Action,
	 class Insn,
	 class X>
struct ActionDispatcher<Action,
			Insn,
			Just<X>  , Just<EncOffset> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename X::operand_type op_type;
	 return dispatch(base::template decode_imm<op_type>());
      }
};


template<class Action,
	 class Insn,
	 class RM>
struct ActionDispatcher<Action,Insn,
			Just<RM>,Just<EncModrm_rm> >
   : public DispatcherHelperModrm<Action,Insn,RM>
{
   typedef DispatcherHelperModrm<Action,Insn,RM> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 if (base::modrm.get_mod() == 3) {
	    return dispatch(base::get_modrm_reg_operand());
	 } else {
	    return dispatch(base::get_modrm_ptr_operand());
	 }
      }
};

template<class Action,
	 class Insn,
	 class RM>
struct ActionDispatcher<Action,Insn,
			Just<RM>,Just<EncModrm_rm_mem> >
   : public DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_mem>
{
   typedef DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_mem> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 return dispatch(base::get_modrm_ptr_operand());
      }
};

template<class Action,
	 class Insn,
	 class RM>
struct ActionDispatcher<Action,Insn,
			Just<RM>,Just<EncModrm_rm_reg> >
   : public DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg>
{
   typedef DispatcherHelperModrm<Action,Insn,RM,EncModrm_rm_reg> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 return dispatch(base::get_modrm_reg_operand());
      }
};

template<class Action,
	 class Insn>
struct ActionDispatcher<Action,Insn,
			Just<OpRUnused>,Just<EncModrm_rm_reg> >
   : public DispatcherHelperModrm<Action,Insn,OpRUnused,EncModrm_rm_reg>
{
   typedef DispatcherHelperModrm<Action,Insn,OpRUnused,EncModrm_rm_reg> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 return base::dispatch();
      }
};



template<class Action,
	 class Insn, class Reg,class MOff>
struct ActionDispatcher<Action,
			Insn,
			Just<Reg> ,Just<EncImplicit> ,
			Just<MOff>,Just<EncDisplacement> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}

   action_result_type action()
      {
	 typedef typename Reg::operand_type op1_type;
	 typedef typename MOff::offset32_type offset32_type;
	 typedef typename MOff::offset64_type offset64_type;

	 op1_type op1;

	 if (base::is_addr32()) {
	    offset32_type op2 = 
	       base::template decode_moffset32<offset32_type>();
	    return base::dispatch(op1,op2);
	 } else {
	    offset64_type op2 = 
	       base::template decode_moffset64<offset64_type>();
	    return base::dispatch(op1,op2);
	 }
      }
};

template<class Action,
	 class Insn, class Reg,class MOff>
struct ActionDispatcher<Action,
			Insn,
			Just<MOff>,Just<EncDisplacement> ,
			Just<Reg> ,Just<EncImplicit> >

   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}
   
   action_result_type action()
      {
	 typedef typename Reg::operand_type op2_type;
	 typedef typename MOff::offset32_type offset32_type;
	 typedef typename MOff::offset64_type offset64_type;

	 op2_type op2;

	 if (base::is_addr32()) {
	    offset32_type op1 = 
	       base::template decode_moffset32<offset32_type>();
	    return base::dispatch(op1,op2);
	 } else {
	    offset64_type op1 = 
	       base::template decode_moffset64<offset64_type>();
	    return base::dispatch(op1,op2);
	 }
      }
};

template<class Action,
	 class Insn >
struct ActionDispatcher<Action,
			Insn,
			Just<OpM8_DS_BX_EBX_RBX>  ,Just<EncDS_SI_ESI_RSI> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}
   
   action_result_type action()
      {
	 typedef BytePtr_EBX ptr_ebx_type;
	 typedef BytePtr_RBX ptr_rbx_type;

	 if (base::is_addr32()) {
	    return base::dispatch(ptr_ebx_type(base::get_segment()));
	 } else {
	    return base::dispatch(ptr_rbx_type(base::get_segment()));
	 }
      }
};


template<class Action,
	 class Insn, class DS_ESI,class ES_EDI>
struct ActionDispatcher<Action,
			Insn,
			Just<DS_ESI>  ,Just<EncDS_SI_ESI_RSI> ,
			Just<ES_EDI>  ,Just<EncES_DI_EDI_RDI> >

   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}
   
   action_result_type action()
      {
	 typedef typename DS_ESI::ptr_esi_type ptr_esi_type;
	 typedef typename DS_ESI::ptr_rsi_type ptr_rsi_type;

	 typedef typename ES_EDI::ptr_es_edi_type ptr_es_edi;
	 typedef typename ES_EDI::ptr_es_rdi_type ptr_es_rdi;

	 if (base::is_addr32()) {
	    return base::dispatch(ptr_esi_type(base::get_segment()),
				  ptr_es_edi());
	 } else {
	    return base::dispatch(ptr_rsi_type(base::get_segment()),
				  ptr_es_rdi());
	 }
      }
};

template<class Action,
	 class Insn, class DS_ESI,class ES_EDI>
struct ActionDispatcher<Action,
			Insn,
			Just<ES_EDI>  ,Just<EncES_DI_EDI_RDI> ,
			Just<DS_ESI>  ,Just<EncDS_SI_ESI_RSI> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}
   
   action_result_type action()
      {
	 typedef typename ES_EDI::ptr_es_edi_type ptr_es_edi;
	 typedef typename ES_EDI::ptr_es_rdi_type ptr_es_rdi;

	 typedef typename DS_ESI::ptr_esi_type ptr_esi_type;
	 typedef typename DS_ESI::ptr_rsi_type ptr_rsi_type;



	 if (base::is_addr32()) {
	    return base::dispatch(ptr_es_edi(),
				  ptr_esi_type(base::get_segment()));
				  
	 } else {
	    return base::dispatch(ptr_es_rdi(),
				  ptr_rsi_type(base::get_segment()));
				  
	 }
      }
};

template<class Action,
	 class Insn, class M,class Reg>
struct ActionDispatcher<Action,
			Insn,
			Just<M>   ,Just<EncES_DI_EDI_RDI> ,
			Just<Reg> ,Just<EncImplicit> >

   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}
   
   action_result_type action()
      {
	 typedef typename Reg::operand_type op2_type;
	 typedef typename M::ptr_es_edi_type ptr32_type;
	 typedef typename M::ptr_es_rdi_type ptr64_type;

	 op2_type op2;

	 if (base::is_addr32()) {
	    return base::dispatch(ptr32_type(),op2);
	 } else {
	    return base::dispatch(ptr64_type(),op2);
	 }
      }
};

template<class Action,
	 class Insn, class M,class Reg>
struct ActionDispatcher<Action,
			Insn,
			Just<Reg> ,Just<EncImplicit> ,
			Just<M>   ,Just<EncES_DI_EDI_RDI> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}
   
   action_result_type action()
      {
	 typedef typename Reg::operand_type op1_type;
	 typedef typename M::ptr_es_edi_type ptr32_type;
	 typedef typename M::ptr_es_rdi_type ptr64_type;

	 op1_type op1;

	 if (base::is_addr32()) {
	    return base::dispatch(op1,ptr32_type());
	 } else {
	    return base::dispatch(op1,ptr64_type());
	 }
      }
};


template<class Action,
	 class Insn, class M,class Reg>
struct ActionDispatcher<Action,
			Insn,
			Just<Reg> ,Just<EncImplicit> ,
			Just<M>   ,Just<EncDS_SI_ESI_RSI> >
   : public DispatcherHelper<Action,Insn>
{
   typedef DispatcherHelper<Action,Insn> base;

   ActionDispatcher(Action& action,InstructionData& params)
      : base(action,params)
      {}
   
   action_result_type action()
      {
	 typedef typename Reg::operand_type op1_type;
	 typedef typename M::ptr_esi_type ptr32_type;
	 typedef typename M::ptr_rsi_type ptr64_type;

	 op1_type op1;

	 if (base::is_addr32()) {
	    return base::dispatch(op1,ptr32_type(base::get_segment()));
	 } else {
	    return base::dispatch(op1,ptr64_type(base::get_segment()));
	 }
      }
};


} // namespace impl


template<class Action,
	 bool Default_addressing_mode_is_64bit,
	 bool Default_operand_size_is_32bit
	 >
inline
const_code_pointer_type
decode(const_code_pointer_type decode_start,Action & action)
{
   using namespace impl;
   InstructionData params;

   // TODO: determine the performance implications for this construct
   const_code_ptr   & start    = params.start;
   const_code_ptr   & opcode_start  = params.opcode_start;
   const_code_ptr   & p             = params.end;

   prefix_bitset    & prefix    = params.prefix;
   RexByte          & rex       = params.rex;
   Segment          & segment   = params.segment;
   bool             & is_data16   = params.is_data16;
   bool             &is_addr32 = params.is_addr32;
   action_code_type  & action_code = params.action_code;

   action_code = ACTION_CONTINUE;
   p   = decode_start;

   while (action_code == ACTION_CONTINUE) {

      start = p;
      prefix.flags = 0;
      rex.byte = 0;
      segment = DefaultSegment;
      opcode_start = p;

      is_data16   = ! Default_operand_size_is_32bit;
      is_addr32 = ! Default_addressing_mode_is_64bit;


     decode_start:
#include "erasm/dsm_x64_auto_stm.hpp"

     decode_error:
    break;
   }

   if (action_code == ACTION_FINISH) {
      return p;
   } 

   return action.error(params);
}

namespace addr64 { namespace data32 {
template<class Action>
inline
const_code_pointer_type
decode(const_code_pointer_type decode_start,Action & action)
{ 
   return ::erasm::x64::decode<Action,true,true>
      (decode_start,action) ; 
}
}}

}}

#endif // MY_ERASM_DSM_X64_HPP
