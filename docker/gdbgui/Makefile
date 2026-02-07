
all: build   # default -> build 

prepare:
	cp -r ../../lib .	

clean : prepare
	docker compose build --no-cache

build: prepare
	docker compose build

up: build
	docker compose up -d
	
down:
	docker compose down

debug: up
	GDBG_NO_FORK=1 GDBG_FIFO_DIR="/opt/otobo/" GDBG_KILL_CMD='docker kill otobo-web-1 -s SIGINT' perl -I /home/mike/workspace/perl/perl-Devel-gdbg/lib/   gdbgui.pl
