#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>


static char HTTP_REQUEST_KILL[] = "POST /containers/otobo-web-1/kill?signal=SIGINT HTTP/1.0\r\n\r\n";

static char HTTP_REQUEST_RESTART[] = "POST /containers/otobo-web-1/restart HTTP/1.0\r\n\r\n";

static char DOCKER_SOCK[]  = "/tmp/docker.sock"; 

int main(int argc, char** argv) 
{
	char* HTTP_REQUEST = HTTP_REQUEST_KILL;

	if(argc > 1) 
	{
		if( strcmp("restart",argv[1]) == 0 )
		{
			HTTP_REQUEST = HTTP_REQUEST_RESTART;
		} 
	}

	int fd = socket(AF_UNIX, SOCK_STREAM, 0);

	struct sockaddr_un server_addr;
	server_addr.sun_family = AF_UNIX;
	strcpy(server_addr.sun_path, DOCKER_SOCK);

	int result = connect(fd, (struct sockaddr *)&server_addr, sizeof(server_addr));
	if(result < 0 ) return result;

	write(fd,HTTP_REQUEST,strlen(HTTP_REQUEST));
	close(fd);

	return 0;
}
