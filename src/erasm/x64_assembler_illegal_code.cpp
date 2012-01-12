// This file lists assembly expressions that should be rejected by the compiler
adc(p,ch,byte_ptr[_6 * rax]);  // invalid addressing
cmps (p,byte_ptr . fs [si],byte_ptr . es [di]); // invalid addressing mode for x64
crc32(p,r8d,ah); // ah-dh cannot be used with rex prefix
crc32(p,r8,ah); // ah-dh cannot be used with rex prefix
crc32(p,rax,ah); // a qword reg is automatically a rex register
movsx(p,r8w,ah);
movsx(p,r8d,ah);
movsx(p,r8 ,ah);
add(p,dh,byte_ptr . ss [r9]); // ah-dh cannot be used with a rex byte
add(p,dh,byte_ptr [(signed int)-1]); // when disp32 is used alone,it must be a unsigned value
adc(p,ch,sil); // rex prefixed expressions cannot access ah,bh,ch,dh
adc(p,ch,byte_ptr[esp*_2]); // esp cannot be used as an index register
adc(p,ch,byte_ptr[eax + esp*_2]);  // esp cannot be used as an index register
adc(p,ch,byte_ptr[rsp*_2]); // esp cannot be used as an index register
adc(p,ch,byte_ptr[rsp*_4]);// esp cannot be used as an index register
adc(p,ch,byte_ptr[rsp*_8]); // esp cannot be used as an index register
adc(p,ch,byte_ptr[rax + rsp*_2]); // esp cannot be used as an index register
cvtpd2pi(p,mm1,xmmword_ptr[rax+ rsp*_2]); //  // esp cannot be used as an index register
adc(p,ch,byte_ptr[rax + ecx]); // 64- and 32- bit registers cannot coexist in an effective address computation
ins(p,byte_ptr.ds[edi],dx); // ES segment cannot be overridden for string instructions
cmps(p,word_ptr.ds[esi] ,word_ptr.ds[edi] ); // es segment cannot be overridden
cmps(p,word_ptr.ds[rsi] ,word_ptr.cs[rdi] ); // es segment cannot be overridden
cmps(p,byte_ptr.ds[rsi+1],byte_ptr.es[rdi] );  //  *_ptr.ds[rsi],*_ptr.es[rdi] are just the place-holders and not encoded.
cmps(p,dword_ptr.ds[rax],dword_ptr.es[rdi] ); //  *_ptr.ds[rsi],*_ptr.es[rdi] are just the place-holders and not encoded.
cmps(p,qword_ptr.es[edi],qword_ptr.ds[rsi] ); //  cannot mix differenct address modes
