.PHONY: doc rm-pg-volume db-shell build-backend nix-shell build-backend-docker-image
DEFAULT_GOAL: help

export LOCAL_USER_ID ?= $(shell id -u $$USER)

# Gitlab docker image repository supports nesting names
# but docker hub does not
IMAGE_NAME=denibertovic/nuts-and-bolts

progress=auto

## Build backend with nix
build-backend:
	@nix-build --attr backend release.nix $(builders)

## Build docker image for backend with nix
build-backend-docker-image:
	@nix-build --attr backend-docker-image release.nix
	@docker load < ./result

## Enter backend nix-shell
nix-shell:
	@nix-shell

## Run local dev env
dev:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose up

## Nukes dev env entirely (NOTICE: removed data volume as well)
dev-clean:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose down -v

## Remove dev env containers
dev-rm:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose rm ${OPTS}

## Generate API docs TODO FIX this
# doc:
# 	@stack runghc src/Api/Docs.hs > docs/api.md

## Spawn bash shell in api container
shell:
	@docker exec -e COLUMNS="`tput cols`" -e LINES="`tput lines`" -u ${LOCAL_USER_ID} -it $$(docker-compose ps -q api) /bin/bash -c "export PATH=/home/user/.local/bin:\$${PATH/\/root\/.local\/bin:/} && \
		export HOME=\$$PWD && \
		reset -w && \
		/bin/bash"

## Spawn bash shell in api container (as ROOT)
shell-root:
	@docker exec -it $$(docker-compose ps -q api) /bin/bash -c "export PATH=/home/user/.local/bin:\$$PATH && \
		export HOME=\$$PWD && \
		/bin/bash"

## Shell into the postgres database container
db-shell:
	@docker exec -e COLUMNS="`tput cols`" -e LINES="`tput lines`" -it $$(docker-compose ps -q postgres) /bin/bash -c "reset -w && /bin/bash"

## PSQL into the postgres database container
psql:
	@docker exec -e COLUMNS="`tput cols`" -e LINES="`tput lines`" -it $$(docker-compose ps -q postgres) /bin/bash -c "psql -Upostgres"

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-0-9_]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

