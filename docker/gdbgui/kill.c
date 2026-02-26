#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>

#define STR_IMPL(x) #x
#define STR(x) STR_IMPL(x)

#define PROJECT_NAME STR( OTOBO_PROJECT_NAME )

static char HTTP_REQUEST_KILL[] = "POST /containers/" PROJECT_NAME "-web-1/kill?signal=SIGINT HTTP/1.0\r\n\r\n";

static char HTTP_REQUEST_RESTART_WEB[] = "POST /containers/" PROJECT_NAME "-web-1/restart?t=0 HTTP/1.0\r\n\r\n";
static char HTTP_REQUEST_RESTART_NGINX[] = "POST /containers/" PROJECT_NAME "-nginx-1/restart?t=0 HTTP/1.0\r\n\r\n";

static char DOCKER_SOCK[]  = "/tmp/docker.sock"; 

int send_request(char* req)
{
	int fd = socket(AF_UNIX, SOCK_STREAM, 0);

	struct sockaddr_un server_addr;
	server_addr.sun_family = AF_UNIX;
	strcpy(server_addr.sun_path, DOCKER_SOCK);

	int result = connect(fd, (struct sockaddr *)&server_addr, sizeof(server_addr));
	if(result < 0 ){

		printf("e: %i\n",result);
		return result;
	} 

	// printf("%s\n",req);
	write(fd,req,strlen(req));
	close(fd);
}

int main(int argc, char** argv) 
{
	if(argc > 1) 
	{
		if( strcmp("restart",argv[1]) == 0 )
		{
			send_request(HTTP_REQUEST_RESTART_WEB);
			send_request(HTTP_REQUEST_RESTART_NGINX);
			return 0;
		} 
	}

	send_request(HTTP_REQUEST_KILL);
	return 0;
}
