FROM ubuntu:24.04

ENV TZ="UTC"

RUN DEBIAN_FRONTEND=noninteractive apt update 
RUN DEBIAN_FRONTEND=noninteractive apt install -y xvfb x11vnc novnc websockify xfce4 dbus-x11 xterm libgtk3-perl gobject-introspection gir1.2-gtk-3.0 libgtk-3-dev curl libfile-slurp-perl 
RUN DEBIAN_FRONTEND=noninteractive apt install -y libgtksourceview-4-dev netcat-openbsd libjson-perl libparams-util-perl	 

COPY run.sh /opt
RUN chmod a+rx /opt/*.sh

COPY kill.c /opt/
RUN gcc /opt/kill.c -o /opt/sigkill
RUN chown root:root /opt/sigkill
RUN chmod a+rxs /opt/sigkill

RUN mkdir -p /usr/share/perl5/Devel/
#RUN curl 'https://raw.githubusercontent.com/littlemole/perl-Devel-gdbg/refs/heads/main/lib/Devel/gdbg.ui' > /opt/gdbg.ui
#RUN curl 'https://raw.githubusercontent.com/littlemole/perl-Devel-gdbg/refs/heads/main/lib/Devel/gdbgui.pl' > /opt/gdbgui.pl
#RUN curl 'https://raw.githubusercontent.com/littlemole/perl-Devel-gdbg/refs/heads/main/lib/Devel/dipc.pm' > /usr/share/perl5/Devel/dipc.pm

COPY lib/Devel/gdbg.ui /opt
COPY lib/Devel/gdbgui.pl /opt
COPY lib/Devel/dipc.pm /usr/share/perl5/Devel/

# uid 1000 by defualt since 24.04
USER ubuntu 

CMD "/opt/run.sh"
