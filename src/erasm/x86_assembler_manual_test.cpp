p += cmps(p,byte_ptr.es[esi],byte_ptr.es[edi]);
p += cmps(p,byte_ptr.ds[esi],byte_ptr.es[edi]);
p += cmps(p,byte_ptr.ds[ si],byte_ptr.es[ di]);
p += cmps(p,word_ptr.ds[esi],word_ptr.es[edi]);
p += cmps(p,word_ptr.ds[ si],word_ptr.es[ di]);
p += cmps(p,dword_ptr.ds[esi],dword_ptr.es[edi]);
p += cmps(p,dword_ptr.ds[ si],dword_ptr.es[ di]);


p += aaa(p);
p += aad(p);
p += aad(p,(imm8_t)0x12);
p += adc(p,al,(imm8_t)0x12);
p += adc(p,ax,(imm16_t)0x1234);
p += adc(p,eax,(imm32_t)0x1234);
p += adc(p,cl,(imm8_t)0x12);
p += adc(p,byte_ptr[0x123456U],(imm8_t)0x12);
p += adc(p,byte_ptr[eax],(imm8_t)0x12);
p += adc(p,byte_ptr[ebp],(imm8_t)0x12);
p += adc(p,byte_ptr[ebp+1],(imm8_t)0x12);
p += adc(p,byte_ptr[ebp+esp + 1],(imm8_t)0x12);
p += adc(p,byte_ptr[esp],(imm8_t)0x12);
p += adc(p,byte_ptr[eax + ebp*_4+1],(imm8_t)0x12);
p += adc(p,byte_ptr[eax +esp],(imm8_t)0x12);
p += adc(p,byte_ptr[esp + eax],(imm8_t)0x12);

p += adc(p,byte_ptr[bx+si],(imm8_t)0x12);
p += adc(p,byte_ptr[bx+di],(imm8_t)0x12);
p += adc(p,byte_ptr[bp+si],(imm8_t)0x12);
p += adc(p,byte_ptr[bp+di],(imm8_t)0x12);
p += adc(p,byte_ptr[si],(imm8_t)0x12);
p += adc(p,byte_ptr[di],(imm8_t)0x12);
p += adc(p,byte_ptr[(offset16_t)0x1234],(imm8_t)0x12);
p += adc(p,byte_ptr[bx],(imm8_t)0x12);

p += adc(p,byte_ptr[bx+si + (rel8_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bx+di + (rel8_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bp+si + (rel8_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bp+di + (rel8_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[si    + (rel8_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[di    + (rel8_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bp    + (rel8_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bx    + (rel8_t)0x1],(imm8_t)0x12);

p += adc(p,byte_ptr[bx+si + (rel16_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bx+di + (rel16_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bp+si + (rel16_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bp+di + (rel16_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[si    + (rel16_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[di    + (rel16_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bp    + (rel16_t)0x1],(imm8_t)0x12);
p += adc(p,byte_ptr[bx    + (rel16_t)0x1],(imm8_t)0x12);


p += adc(p,cx,(imm8_t)0x12);
p += adc(p,word_ptr[ebx],(imm8_t)0x12);
p += adc(p,word_ptr[bx],(imm8_t)0x12);
p += adc(p,dword_ptr[eax],1u);
p += adc(p,cl,bh);
p += adc(p,cx,bx);
p += adc(p,ecx,edx);
p += addpd(p,xmm1,xmm7);
p += addpd(p,xmm1,xmmword_ptr[eax]);
p += addpd(p,xmm1,xmmword_ptr[bp]);
p += bound(p,ax,dword_ptr[eax]);
p += bound(p,eax,qword_ptr[eax]);
p += call(p,(rel16_t)0x1234);
p += call(p,(rel32_t)0x12345678);
p += call(p,ax);
p += call(p,word_ptr[eax]);
p += call(p,eax);
p += call(p,dword_ptr[eax]);
p += call(p,far16((imm8_t)0x12,0x3456));
p += call(p,far32((imm8_t)0x12,0x3456789a));
p += call(p,far16_ptr[eax]);
p += call(p,far32_ptr[eax]);
p += inc(p,bx);
p += inc(p,edx);
p += lds(p,ax,far16_ptr[eax]);
p += lds(p,eax,far32_ptr[eax]);
p += pop(p,ds);

p += pclmullqlqdq(p,xmm0,xmm1);
p += pclmulhqlqdq(p,xmm0,xmm1);
p += pclmullqhdq(p,xmm0,xmm1);
p += pclmulhqhdq(p,xmm0,xmm1);
