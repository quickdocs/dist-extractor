.PHONY: all
all: extract

ifndef version
override version = $(shell curl -s -L http://beta.quicklisp.org/dist/quicklisp.txt | grep '^version: ' | sed -e 's/version: //')
endif

.PHONY: version
version:
	@echo "${version}"

image_name = "ghcr.io/quickdocs/quicklisp-dist-all"
.PHONY: docker_image
docker_image:
	./build_image.sh ${image_name} ${version}

.PHONY: extract
extract: output/quicklisp/$(version)
output/quicklisp/$(version): docker_image
	docker run --rm -i -v ${PWD}:/app "${image_name}:${version}" /app/extract.sh

.PHONY: upload
upload:
	./upload.sh

.PHONY: clean
clean:
	rm -rf output
	rm quicklisp-*.tar.gz
