#include <erasm/faststream.hpp>
#include <boost/timer.hpp>
#include <iostream>
#include <iomanip>

#define NDEBUG
#include <erasm/x86_io.hpp>

// 801012              0815a8ed  3 adc BYTE PTR[eax],0x12;|

char opcode[] = {0x80,0x10,0x12};
int  len = sizeof(opcode);
char*  addr = (char*) 0x0815a8ed;


using namespace std;
using namespace erasm::x86;

int test_fwriteA()
{
   return printf("%s %s[%s],%x\n","adc","BYTE PTR","eax",0x12);
}

int test_printfC()
{
   static char buf[200];
   int counter=0;
   int n = 0;

   for (int i=0 ;i < len;i++) {
      n+= sprintf(&buf[n],"%02x",opcode[i]);
   }

   return printf("%s %p %d %s %s[%s],%x\n",buf,addr,len,"adc","BYTE PTR","eax",0x12);
}


int test_fwriteC()
{
   static char buf[200];
   int counter=0;
   for (int i=0;i<len;i++) {
      unsigned char x = opcode[i];
      buf[counter]   = (char) ('0' + x/16) ;
      buf[counter+1] = (char) ('0' + x%16) ;
      counter += 2;
   }
   
   for (;counter<20;counter++) {
      buf[counter] = ' ';
   }
   buf[counter]=0;

   return printf("%s %p %d %s %s[%s],%x\n",buf,addr,len,"adc","BYTE PTR","eax",0x12);
}

int testC2()
{
   print_code(erasm::cout,(const_code_pointer_type)opcode,(const_code_pointer_type)(opcode+len));
   return printf("%s %s[%s],%x\n","adc","BYTE PTR","eax",0x12);
}

void test_myoutA()
{
   const_code_pointer_type p = (const_code_pointer_type) opcode;
   erasm::cout 
      << "adc" << " " << "BYTE PTR" << "[" << "eax" << "]" << " , " 
      <<  hex  << 0x12 << endl;
}

void test_myoutC()
{
   const_code_pointer_type p = (const_code_pointer_type) opcode;
   print_code(erasm::cout,p,p+len)
      << "adc" << " " << "BYTE PTR" << "[" << "eax" << "]" << " , " 
      <<  hex  << 0x12 << endl;
}


void test_nullout()
{
   const_code_pointer_type p = (const_code_pointer_type) opcode;
   print_code(erasm::cout_counter,p,p+len)
      << "adc" << " " << "BYTE PTR" << "[" << "eax" << "]" << " , " 
      <<  hex  << 0x12 << endl;
}


using namespace std;

int main() 
{
   {
      cerr << "printf,format A" << endl;
      boost::timer t0;
      for (int i=0;i<2150000;i++) {
	 test_fwriteA();
      }
      float elapsed = t0.elapsed();
      cerr << "elapsed time = " << elapsed << " sec" << endl;      
   }

   {
      cerr << "printf,format C" << endl;
      boost::timer t0;
      for (int i=0;i<2150000;i++) {
	 test_printfC();
      }
      float elapsed = t0.elapsed();
      cerr << "elapsed time = " << elapsed << " sec" << endl;      
   }

   {
      cerr << "printf,format C" << endl;
      boost::timer t0;
      for (int i=0;i<2150000;i++) {
	 test_fwriteC();
      }
      float elapsed = t0.elapsed();
      cerr << "elapsed time = " << elapsed << " sec" << endl;      
   }

   {
      cerr << "erasm::cout,format C" << endl;
      boost::timer t0;
      for (int i=0;i<2150000;i++) {
	 test_myoutC();
      }
      float elapsed = t0.elapsed();
      cerr << "elapsed time = " << elapsed << " sec" << endl;      
   }

   {
      cerr << "format C2" << endl;
      boost::timer t0;
      for (int i=0;i<2150000;i++) {
	 testC2();
      }
      float elapsed = t0.elapsed();
      cerr << "elapsed time = " << elapsed << " sec" << endl;      
   }

   {
      cerr << "erasm::cout,Format A" << endl;
      boost::timer t0;
      for (int i=0;i<2150000;i++) {
	 test_myoutA();
      }
      float elapsed = t0.elapsed();
      cerr << "elapsed time = " << elapsed << " sec" << endl;      
   }

   {
      cerr << "erasm::cout_counter" << endl;
      boost::timer t0;
      for (int i=0;i<2150000;i++) {
	 test_nullout();
      }
      float elapsed = t0.elapsed();
      cerr << "elapsed time = " << elapsed << " sec" << endl;      
   }

   return 0;
}
