#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>

int main() 
{
	int fd = socket(AF_UNIX, SOCK_STREAM, 0);

	struct sockaddr_un server_addr;

	server_addr.sun_family = AF_UNIX;
	strcpy(server_addr.sun_path, "/tmp/docker.sock");

	int result = connect(fd, (struct sockaddr *)&server_addr, sizeof(server_addr));

	if(result < 0 ) return result;

	char buf[] = "POST /containers/otobo-web-1/kill?signal=SIGINT HTTP/1.0\r\n\r\n";

	write(fd,buf,strlen(buf));

//	sleep(1);
	close(fd);
    return 0;
}
