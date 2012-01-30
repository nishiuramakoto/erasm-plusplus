#include <erasm/faststream.hpp>

namespace erasm {
nullstream cout_counter;
ofaststream<500>  cout(stdout);
ofaststream<500>  cerr(stderr);
}
