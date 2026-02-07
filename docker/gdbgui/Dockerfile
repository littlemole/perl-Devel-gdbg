FROM ubuntu:24.04

ENV TZ="UTC"

# update and install packages
RUN DEBIAN_FRONTEND=noninteractive apt update 
RUN DEBIAN_FRONTEND=noninteractive apt upgrade -y

# system deps
RUN DEBIAN_FRONTEND=noninteractive apt install -y \
	xvfb x11vnc novnc websockify xfce4 dbus-x11 \
	xterm netcat-openbsd curl

# gtk deps	
RUN DEBIAN_FRONTEND=noninteractive apt install -y \
	libgtk-3-dev gobject-introspection \
	gir1.2-gtk-3.0 libgtksourceview-4-dev

# perl deps
RUN DEBIAN_FRONTEND=noninteractive apt install -y \
	libfile-slurp-perl libjson-perl \
	libparams-util-perl	libgtk3-perl 
	 
# xfce dark mode
COPY xsettings.xml /opt

# this will out PID 1
COPY run.sh /opt
RUN chmod a+rx /opt/*.sh

# create the suid executable that will
# send a SIGINT command over the mounted
# /tmp/docker.sock local UNIX socket to
# the otobo-web-1 docker container.
COPY kill.c /opt/
RUN gcc /opt/kill.c -o /opt/kill
RUN chown root:root /opt/kill
RUN chmod a+rxs /opt/kill

# add Perl Debugger code
RUN mkdir -p /usr/share/perl5/Devel/

COPY lib/Devel/gdbg.ui /opt
COPY lib/Devel/gdbgui.pl /opt
COPY lib/Devel/dipc.pm /usr/share/perl5/Devel/

# drop root privileges
# uid 1000 by default since 24.04
USER ubuntu 

# run it
CMD ["/opt/run.sh"]
