#ifndef INCLUDED_SOCKETCLIENT
#define INCLUDED_SOCKETCLIENT

#include <string>
#define SERVER_ADDR "54.174.136.215"
#define SERVER_PORT 3000
#define BUFLEN 1000

#ifdef _WIN32
char * sendtoServer_Windows(std::string str);
#endif
#endif // INCLUDED_SOCKETCLIENT