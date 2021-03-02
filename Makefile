.PHONY: all
all: extract

ifndef version
override version = $(shell curl -s -L http://beta.quicklisp.org/dist/quicklisp.txt | grep '^version: ' | sed -e 's/version: //')
endif

.PHONY: version
version:
	@echo "${version}"

image_name = "ghcr.io/quickdocs/quicklisp-dist-all:${version}"
.PHONY: docker_image
docker_image:
	docker image inspect ${image_name} >/dev/null 2>&1 || \
		docker pull ${image_name} >/dev/null 2>&1 || \
		(docker build -t ${image_name} --build-arg DIST_VERSION=${version} . \
		   && ([ "${PUSH_IMAGE}" ] && docker push ${image_name}) || true)

.PHONY: extract
extract: output/quicklisp/$(version)
output/quicklisp/$(version): docker_image
	docker run --rm -i -v ${PWD}:/app ${image_name}

.PHONY: upload
upload:
	./upload.sh

.PHONY: clean
clean:
	rm -rf output
	rm quicklisp-*.tar.gz
