// This file lists assembly expressions that should be rejected by compiler
adc(p,ch,x64::sil); // rex prefixed expressions cannot access ah,bh,ch,dh
adc(p,ch,byte_ptr[esp*_2]); // esp cannot be used as an index register
adc(p,ch,byte_ptr[eax + esp*_2]);  // esp cannot be used as an index register
adc(p,ch,byte_ptr[x64::r13w ]); // extended registers not available in x86 mode 
adc(p,ch,byte_ptr[x64::r13w + si ]); // extended registers not available in x86 mode 
adc(p,ch,byte_ptr[x64::r8d ]); // extended registers not available in x86 mode 
adc(p,ch,byte_ptr[x64::r8d + ebx ]); // extended registers not available in x86 mode 
adc(p,ch,byte_ptr[ax]); // ax cannot be used in 16bit addressing mode
adc(p,x64::r8d,eax);
addpd(p,x64::xmm8,xmm1);
cmps(p,byte_ptr.ds[esi],byte_ptr.ds[edi]);
cmps(p,byte_ptr.ds[ bx],byte_ptr.es[ di]);
cmps(p,word_ptr.ds[esi],word_ptr.gs[edi]);
cmps(p,word_ptr.ds[esi],word_ptr.es[di]);
cmps(p,dword_ptr.ds[esi],dword_ptr.es[eax]);


