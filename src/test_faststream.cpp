#include <erasm/faststream.hpp>
#include <boost/timer.hpp>
#include <iostream>

using namespace std;

const char* str[] = {"aaaaabbbbaaaaaaaa", "aaaaaa" ,"ddddddddd" , "eflkdkl","def"};
int main()
{
   boost::timer t0;
   for (int i = 0;i < 2150000 ; i++ ) {
      for (int j=0;j<5;j++) {
	 cout << str[j] << " ";	 
      }
      cout << endl;
   }
   float elapsed = t0.elapsed();
   erasm::cerr << "elapsed time = " << elapsed << " sec" << endl;
   return 0;
}


