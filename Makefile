PROJECT := $(shell basename $(CURDIR))
BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
GHC_VER := 8.8
RESOLVER := lts-16.13
BASE_IMAGE := $(PROJECT)-test-base:$(RESOLVER)
export COMPOSE_PROJECT_NAME := $(PROJECT)-$(BRANCH)

.PHONY: env
env:
	@echo "PROJECT=$(PROJECT)"
	@echo "BRANCH=$(BRANCH)"
	@echo "GHC_VER=$(GHC_VER)"
	@echo "RESOLVER=$(RESOLVER)"
	@echo "BASE_IMAGE=$(BASE_IMAGE)"
	@echo "COMPOSE_PROJECT_NAME=$(COMPOSE_PROJECT_NAME)"

.PHONY: base
base:
	docker build -t $(BASE_IMAGE) --build-arg GHC_VER=$(GHC_VER) --build-arg RESOLVER=$(RESOLVER) ./docker/test-base

.PHONY: base-bash
base-bash:
	docker run -it --rm $(BASE_IMAGE) /bin/bash

.PHONY: build
build:
	docker-compose build --build-arg BASE_IMAGE=$(BASE_IMAGE)

.PHONY: test
test: build
	docker-compose run --rm dev stack test

.PHONY: bash
bash:
	docker-compose run --rm dev /bin/bash

.PHONY: ps
ps:
	docker-compose ps

.PHONY: down
down:
	docker-compose down

.PHONY: up-db
up-db:
	docker-compose up -d db

.PHONY: stop-db
stop-db:
	docker-compose stop db
