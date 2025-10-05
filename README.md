# simple

debug a simple perl script

```Bash
PERL5LIB="$PERL5LIB:/home/mike/workspace/old/td/lib/" perl -dd:gdbg test.pl
```

# debug otobo

pre-requisite: you have some kind of local otobo dev environment running
otobo-docker, but with the /opt/otobo mapped to a local directory.

- make sure the local volume is either under /opt/otobo on the docker host,
or /opt/otobo is symlinked to it.

- copy the contents of lib/Devel to /opt/otobo/Kernel/cpan-lib/Devel (must be
avail inside the container).

- modify entrypoint.sh (either with a local docker img build, or by mapping
entrypoint.sh to a local volume by patching docker compose files)


### entrypoint.sh:

this will attach the debugger, and run otobo with plackup Standalone
(instead of Shotgun or Gazelle).

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

now run `docker-compose up` to start otobo

afterwards, start the debugger ui:

run the ui;
```Bash
GDBG_FIFO_DIR="/opt/otobo/" GDBG_KILL_CMD='docker exec otobo-web-1 kill -s SIGINT' perl -I </path/to/local/copy/of/debugger/src/lib/> gdbgui.pl
```
