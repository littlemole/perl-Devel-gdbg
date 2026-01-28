
all: build

prepare:
	cp -r ../../lib .	

build: prepare
	docker compose build

up: build
	docker compose up -d
	
down:
	docker compose down

	