# simple

debug a simple perl script

```Bash
PERL5LIB="$PERL5LIB:<Path_to_this_repo_on_yor_disk>/lib/" perl -dd:gdbg test.pl
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
       exec perl -I /opt/otobo/Kernel/cpan-lib -d:gdbg /opt/otobo_install/local/bin/plackup -s Standalone --port 5000 bin/psgi-bin/otobo.psgi
```

now run `docker-compose up` to start otobo

afterwards, start the debugger ui:

run the ui;
```Bash
GDBG_FIFO_DIR="/opt/otobo/" GDBG_KILL_CMD='docker exec otobo-web-1 kill -s SIGINT {{PID}}' perl -I </path/to/local/copy/of/debugger/src/lib/> gdbgui.pl
```

# using docker

edit the .env file in `docker/gdbgui` and set the **OPT_OTOBO** variable to 
point to your local /opt/otobo volume that is mounted into the container.

then cd into the docker compose folder and run makefile

```Bash
cd docker/gdbgui
make up
```
Then use a VNC client to connect to the GUI app inside docker running on Xvfb.
Default Location is **localhost:9999**

When you do not have a VNC client, use noVNC in your browser:

```
http://localhost:9999/vnc.html
```

To stop the debugger container close the
debugger app in the GUI, or use makefile

```Bash
make down
```

## Restarting Debugging

Stop the dgbg container, stop and re-start the otobo web container, then re-start the debugger container.

*sorry for the inconvinience for the time being*