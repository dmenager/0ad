#include "precompiled.h"
#include "SocketClient.h"

#ifdef _WIN32
#include <WS2tcpip.h>

#pragma comment (lib, "wsock32.lib")

char * sendtoServer_Windows(std::string str) {
	// Winsock data
	WSADATA wsa_data;
	int res;
	char receive_buffer[BUFLEN] = "";

	// Initializing Winsock
	res = WSAStartup(MAKEWORD(2,2), &wsa_data);
	if (res != 0) {
		//debug_warn(L"Error initializing Winsock");
		WSACleanup();
		return receive_buffer;
	}

	// Creating Socket
	SOCKET sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if(sock == INVALID_SOCKET) {
		//debug_warn(L"Error in creating socket\n");
		WSACleanup();
		return receive_buffer;
	}

	// Resolve the hostname to an IP address, stored in the hostent structure
	char *hostname = SERVER_ADDR;
	struct hostent *server = gethostbyname(hostname);
	if (server == NULL) {
		//debug_warn(L"Error in resolving hostname to an IP address\n");
		WSACleanup();
		return receive_buffer;
	}

	SOCKADDR_IN server_info;
	server_info.sin_port = htons(SERVER_PORT);
	server_info.sin_family = AF_INET;
	server_info.sin_addr.s_addr = *((unsigned long *)server->h_addr);
	
	// Connect to server
	res = connect(sock, (SOCKADDR*)(&server_info), sizeof(server_info));
	if (res != 0) {
		//debug_warn(L"Error connecting to server\n");
		WSACleanup();
		return receive_buffer;
	}

	// Send data to server
	long strSize = str.length();

	res = send(sock, str.c_str(), strSize, 0);
	if (res == SOCKET_ERROR) {
		//debug_warn(L"Error in sending data");
		WSACleanup();
		return receive_buffer;
	}

	res = recv(sock, receive_buffer, BUFLEN, 0);

	return receive_buffer;
}


#endif