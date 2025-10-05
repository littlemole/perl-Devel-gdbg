# simple

debug a simple perl script

```Bash
PERL5LIB="$PERL5LIB:/home/mike/workspace/old/td/lib/" perl -dd:gdbg test.pl
```

# debug otobo

run the ui;
```Bash
GDBG_FIFO_DIR="/opt/otobo/" GDBG_KILL_CMD='docker exec otobo-web-1 kill -s SIGINT' perl -I /home/mike/workspace/old/td/lib gdbgui.pl
```

modify entrypoint.sh

entrypoint.sh:

```Bash
# Start the webserver
function exec_web() {

    # For development omit the --env option, thus setting PLACK_ENV to its default value 'development'.
    # This enables additional middlewares that are useful during development.
    # For development also enable the -R option. This watches for changes in the modules and the config files.
    # otobo.psgi is watched implicitly.
#       exec plackup --server Gazelle -R Kernel --port 5000 bin/psgi-bin/otobo.psgi

    # For debugging reload the complete application for each request by passing -L Shotgun
       export GDBG_NO_FORK=1
       export GDBG_FIFO_DIR="/opt/otobo/"
       export GDBG_KILL_CMD='docker exec otobo-web-1 kill -s SIGINT '
#       exec perl -I /opt/otobo/Kernel/cpan-lib -d:gdbg /opt/otobo_install/local/bin/plackup --loader Shotgun --port 5000 bin/psgi-bin/otobo.psgi
       exec perl -I /opt/otobo/Kernel/cpan-lib -d:gdbg /opt/otobo_install/local/bin/plackup -s Standalone --port 5000 bin/psgi-bin/otobo.psgi
```

note the GDBG_FIFO_DIR variable
assumption is that the opt_otobo volume is mapped (or smylinked) to /opt/otob on the docker host,
so both inside and outside container /opt/otobo points to the same filesystem. this is to make the fifo ipc work.

