// manually editted test code
p += adc (p,spl,byte_ptr [r13d + r15d * _2 + (int8_t) INT8_C (43)]);
p += adc (p,edx,dword_ptr [r13 + r15 * _2 + (int8_t) INT8_C (43)]);
p += adc(p,byte_ptr.gs[rsi],(imm8_t)0x1);
p += adc(p,byte_ptr.ss[rdi],(imm8_t)0x1);
p += adc(p,byte_ptr.gs[esi],(imm8_t)0x1);
p += adc(p,byte_ptr.ss[edi],(imm8_t)0x1);
p += adc(p,byte_ptr[rip+0x1],(imm8_t)0x1);
p += adc(p,byte_ptr[0x1U],(imm8_t)0x1);
p += adc(p,byte_ptr[rax + r12*_4+0x1],(imm8_t)0x1);
p += adc(p,byte_ptr[rax + r13*_4+0x1],(imm8_t)0x1);
p += adc(p,byte_ptr[rbp + r13*_4+0x1],(imm8_t)0x1);
p += adc(p,byte_ptr[rax+0x1],(imm8_t)0x1);
p += adc(p,byte_ptr[rax+0xcccc],(imm8_t)0x1);
p += adc(p,byte_ptr[rax+(imm32_t)0x2],(imm8_t)0x1);
p += adc(p,al,(imm8_t)0x1);
p += adc(p,ax,(imm16_t)0x1);
p += adc(p,eax,(imm32_t)0x2);
p += adc(p,rax,(imm32_t)0x2);
p += adc(p,cl,(imm8_t)0x1);
p += adc(p,ch,(imm8_t)0x1);
p += adc(p,r8l,(imm8_t)0x1);
p += adc(p,dil,(imm8_t)0x1);
p += adc(p,byte_ptr[rax+r10*_2+0x8],(imm8_t)0x1);
p += adc(p,byte_ptr[rbx],(imm8_t)0x1);
p += adc(p,byte_ptr[ebx],(imm8_t)0x1);
p += adc(p,r8w,(imm16_t)0x1);
p += adc(p,word_ptr[rax+r10*_8+0x8],(imm16_t)0x1);
p += adc(p,word_ptr[eax],(imm16_t)0x1);
p += adc(p,ebx,(imm32_t)0x2);
p += adc(p,dword_ptr[rax+r9*_2],(imm32_t)0x2);
p += adc(p,dword_ptr.es[ecx + edx * _4],(imm32_t)0x2);
p += adc(p,dword_ptr.gs[ecx + edx * _4 ],(imm32_t)0x2);
p += adc(p,dword_ptr.cs[ecx + edx * _4 ],(imm32_t)0x2);
p += adc(p,r10,(imm32_t)0x2);
p += adc(p,qword_ptr[rax],(imm32_t)0x2);
p += adc(p,dx,(imm8_t)0x1);
p += adc(p,word_ptr[rdx],(imm8_t)0x1);
p += adc(p,ebx,(imm8_t)0x1);
p += adc(p,dword_ptr[rax],(imm8_t)0x1);
p += adc(p,r9,(imm8_t)0x1);
p += adc(p,qword_ptr[r9],(imm8_t)0x1);
p += adc(p,cl,dl);
p += adc(p,ch,dh);
p += adc(p,dil,sil);
p += adc(p,r8l,r9l);
p += adc(p,cl,dh);
p += adc(p,cl,r8l);
p += adc(p,byte_ptr[rax],cl);
p += adc(p,bx,dx);
p += adc(p,word_ptr[rax],ax);
p += adc(p,ebx,edx);
p += adc(p,dword_ptr[rax],eax);
p += adc(p,r8,r9);
p += adc(p,qword_ptr[rax],rax);
p += adc(p,bl,cl);
p += adc(p,bl,byte_ptr[rax]);
p += adc(p,bx,ax);
p += adc(p,bx,word_ptr[rax]);
p += adc(p,ebx,eax);
p += adc(p,ebx,dword_ptr[rax]);
p += adc(p,rbx,rcx);
p += adc(p,rbx,qword_ptr[rax]);
p += addpd(p,xmm1,xmm2);
p += addpd(p,xmm1,xmm8);
p += addpd(p,xmm8,xmm1);
p += addpd(p,xmm8,xmm9);
p += addpd(p,xmm1,xmmword_ptr[rax+r8*_2]);
p += addss(p,xmm1,xmm2);
p += addss(p,xmm1,xmm8);
p += addss(p,xmm8,xmm9);
p += addss(p,xmm1,dword_ptr[eax]);
p += addss(p,xmm15,dword_ptr[eax]);
p += addss(p,xmm15,dword_ptr.cs[eax]);
p += aeskeygenassist(p,xmm8,xmmword_ptr[eax + eax*_2 + 0x7fff],(imm8_t)0x1);
p += blendvpd(p,xmm8,xmm1,xmm0);
p += bswap(p,eax);
p += bswap(p,r8d);
p += bswap(p,rax);
p += bswap(p,r8);
p += call(p,0x1);
p += call(p,qword_ptr[rax]);
p += call(p,far32_ptr[rax]);
p += call(p,far32_ptr[eax]);
p += call(p,far32_ptr.gs[eax+0x1234]);
p += call(p,far32_ptr.gs[ebp+0x1234]);
p += call(p,far64_ptr.gs[ebp+0x1234]);
p += clflush(p,byte_ptr [ rax + rbx * _8 + 0x1234 ]);
p += cmps(p,byte_ptr.ds[rsi] , byte_ptr.es[rdi]);
p += cmps(p,word_ptr.ds[rsi] ,word_ptr.es[rdi] );
p += cmps(p,dword_ptr.ds[rsi],dword_ptr.es[rdi] );
p += cmps(p,qword_ptr.ds[rsi],qword_ptr.es[rdi] );
p += cmpxchg8b(p,qword_ptr[rax]);
p += cmpxchg16b(p,oword_ptr[rax]);
p += crc32(p,r8d,al);
p += crc32(p,r8d,spl);
p += crc32(p,r8,spl);
p += cvtpd2pi(p,mm1,xmm8);
p += cvtpd2pi(p,mm1,xmmword_ptr[rax]);
p += extractps(p,rax,xmm1,0x12);
p += extractps(p,rax,xmm8,0x12);
p += extractps(p,dword_ptr[rax],xmm8,0x12);
p += fadd(p,dword_ptr[0x0U]);
p += fadd(p,qword_ptr[0x0U]);
p += fadd(p,st0,st1);
p += fadd(p,st1,st0);
p += fadd(p,st0,st0);
p += fiadd(p,dword_ptr[rax]); 
p += fbld(p,tbyte_ptr[rax]);
p += fbstp(p,tbyte_ptr [rax]);
p += fldenv(p,void_ptr[rax]);
p += int3(p);
p += int_(p,0x3);
p += int_(p,0x1);
p += invlpg(p,void_ptr[rax]);
p += ja(p,(rel8_t)0x1);
p += ja(p,(rel32_t)-1);
p += jmp(p,far16_ptr[r8]);
p += jmp(p,far32_ptr[r8]);
p += jmp(p,far64_ptr[rax]);

p += mov(p,rax,gs);
p += mov(p,rax,gs);

p += mov(p,al,byte_offset.cs[(uint32_t)0]);
p += mov(p,al,byte_offset.cs[(uint64_t)0]);

p += mov(p,al,byte_offset.cs[UINT64_C(0x123456789abcde)]);
p += mov(p,al,byte_offset[UINT64_C(0x123456789abcde)]);
p += mov(p,al,byte_offset.cs[UINT64_C(0x123456789abcde)]);
p += mov(p,ax,word_offset.ss[UINT64_C(0x123456789abcde)]);
p += mov(p,eax,dword_offset.ds[UINT64_C(0x123456789abcde)]);
p += mov(p,rax,qword_offset.es[UINT64_C(0x123456789abcde)]);

p += mov(p,al,byte_offset.fs[0x12345678U]);
p += mov(p,ax,word_offset.gs[0x12345678U]);
p += mov(p,eax,dword_offset.ds[0x12345678U]);
p += mov(p,rax,qword_offset.es[0x12345678U]);

p += mov(p,al,0x1);
p += mov(p,ah,0x1);
p += mov(p,dil,0x1);
p += mov(p,r8b,0x1);
p += mov(p,ax,0x1);
p += mov(p,r8w,0x1);
p += mov(p,eax,0x1);
p += mov(p,r8d,0x1);
p += mov(p,rax,0x1);
p += mov(p,r8 ,0x1);
p += mov(p,rax,cr2);
p += mov(p,rax,cr8);
p += mov(p,cr1,rax);
p += mov(p,cr1,r8);
p += mov(p,cr8,rcx);
p += mov(p,cr8,r9);
p += mov(p,rax,db2);
p += mov(p,db1,rax);

p += movmskpd(p,rax,xmm1);
p += movmskpd(p,r15,xmm1);
p += movmskpd(p,rax,xmm13);
p += movmskpd(p,r15,xmm13);

p += movzx(p,si,dl);
p += movzx(p,si,dh);

p += movzx(p,esi,dl);
p += movzx(p,esi,dh);

p += rcl(p,al,0x1);
p += rcl(p,al,0x2);
p += rcl(p,al,cl);

p += ins(p,byte_ptr.es[rdi],dx);
p += ins(p,byte_ptr.es[edi],dx);
p += ins(p,word_ptr.es[rdi],dx);
p += ins(p,word_ptr.es[edi],dx);
p += ins(p,dword_ptr.es[rdi],dx);
p += ins(p,dword_ptr.es[edi],dx);
p += rep_ins(p,byte_ptr.es[rdi],dx);
p += rep_ins(p,byte_ptr.es[rdi],dx);
p += rep_ins(p,byte_ptr.es[edi],dx);
p += rep_ins(p,word_ptr.es[rdi],dx);
p += rep_ins(p,word_ptr.es[edi],dx);
p += rep_ins(p,dword_ptr.es[rdi],dx);
p += rep_ins(p,dword_ptr.es[edi],dx);

p += outs(p,dx,byte_ptr.gs[rsi]);
p += outs(p,dx,byte_ptr.gs[esi]);
p += outs(p,dx,word_ptr.ds[rsi]);
p += outs(p,dx,word_ptr.ds[esi]);
p += outs(p,dx,dword_ptr.es[rsi]);
p += outs(p,dx,dword_ptr.es[esi]);
p += rep_outs(p,dx,byte_ptr[rsi]);
p += rep_outs(p,dx,byte_ptr.es[rsi]);
p += rep_outs(p,dx,byte_ptr.ds[esi]);
p += rep_outs(p,dx,word_ptr[rsi]);
p += rep_outs(p,dx,word_ptr.ds[esi]);
p += rep_outs(p,dx,dword_ptr[rsi]);
p += rep_outs(p,dx,dword_ptr.ds[esi]);

p += movs(p,byte_ptr.es[rdi],byte_ptr[rsi]);
p += movs(p,byte_ptr.es[edi],byte_ptr.ds[esi]);
p += movs(p,byte_ptr.es[edi],byte_ptr.gs[esi]);
p += movs(p,byte_ptr.es[edi],byte_ptr.cs[esi]);

p += movs(p,word_ptr.es[rdi],word_ptr[rsi]);
p += movs(p,word_ptr.es[edi],word_ptr.ds[esi]);
p += movs(p,dword_ptr.es[rdi],dword_ptr[rsi]);
p += movs(p,dword_ptr.es[edi],dword_ptr.ds[esi]);

p += movs(p,qword_ptr.es[rdi],qword_ptr[rsi]);
p += movs(p,qword_ptr.es[edi],qword_ptr.ds[esi]);

p += rep_movs(p,byte_ptr.es[rdi],byte_ptr[rsi]);
p += rep_movs(p,byte_ptr.es[edi],byte_ptr.ds[esi]);
p += rep_movs(p,byte_ptr.es[edi],byte_ptr.ss[esi]);

p += rexw_rep_movs(p,byte_ptr.es[rdi],byte_ptr[rsi]);
p += rexw_rep_movs(p,byte_ptr.es[edi],byte_ptr.ds[esi]);

p += rep_movs(p,word_ptr.es[rdi],word_ptr[rsi]);
p += rep_movs(p,word_ptr.es[rdi],word_ptr.ds[rsi]);
p += rep_movs(p,word_ptr.es[edi],word_ptr.ds[esi]);

//p += rexw_rep_movs(p,word_ptr.es[rdi],word_ptr[rsi]);

p += rep_movs(p,dword_ptr.es[rdi],dword_ptr[rsi]);
p += rep_movs(p,dword_ptr.es[edi],dword_ptr.ds[esi]);

//p += rexw_rep_movs(p,qword_ptr.es[rdi],qword_ptr[rsi]);
//p += rexw_rep_movs(p,qword_ptr.es[edi],qword_ptr.ds[esi]);

p += rep_movs(p,qword_ptr.es[rdi],qword_ptr[rsi]);
p += rep_movs(p,qword_ptr.es[edi],qword_ptr.ds[esi]);


p += rcl(p,dl,1);
p += rcl(p,dl,cl);
p += rcl(p,dil,1);
p += rcl(p,dil,cl);
p += ror(p,rcx,2);
p += ror(p,qword_ptr [rcx],2);
p += pop(p,dx);
p += pop(p,rdx);
p += pop(p,word_ptr[rax]);
p += pop(p,word_ptr[eax]);
p += pop(p,qword_ptr[rax]);
p += pop(p,qword_ptr[eax]);
p += push(p,fs);
//p += prefix_operand(p); p += push(p,fs);

p += lods(p,al,byte_ptr.ds[esi]);
p += lods(p,byte_ptr.ds[esi]);
p += scas(p,al,byte_ptr.es[edi]);
p += scas(p,byte_ptr.es[edi]);
p += stos(p,byte_ptr.es[edi],al);
p += stos(p,byte_ptr.es[edi]);

p += rexw_rep_ins(p,byte_ptr.es[edi],dx);
p += rexw_rep_ins(p,dword_ptr.es[edi],dx);
p += rexw_rep_movs(p,byte_ptr.es[rdi],byte_ptr[rsi]);
p += rexw_rep_outs(p,dx,byte_ptr.ds[rsi]);
p += rexw_rep_outs(p,dx,dword_ptr.ds[rsi]);
p += rexw_rep_lods(p,al,byte_ptr.ds[esi]);
p += rexw_rep_stos(p,byte_ptr.es[edi]);
p += rexw_repe_cmps(p,byte_ptr.ds[esi],byte_ptr.es[edi]);
p += rexw_repe_scas(p,byte_ptr.es[edi]);
p += rexw_repne_cmps(p,byte_ptr.ds[esi],byte_ptr.es[edi]);
p += rexw_repne_scas(p,byte_ptr.es[edi]);

p += wait_(p);
p += fwait(p);
p += int3(p);// work around for a dsm bug
p += fxam(p); 

p += pclmullqlqdq(p,xmm0,xmm1);
p += pclmulhqlqdq(p,xmm0,xmm1);
p += pclmullqhdq(p,xmm0,xmm1);
p += pclmulhqhdq(p,xmm0,xmm1);

p += movsx(p,rbx,r14l);

p += xchg (p,ax,r8w);
