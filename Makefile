.PHONY: all
all: build_image extract

ifndef version
override version = $(shell curl -s -L http://beta.quicklisp.org/dist/quicklisp.txt | grep '^version: ' | sed -e 's/version: //')
endif

.PHONY: version
version:
	@echo "${version}"

image_name = ghcr.io/quickdocs/quicklisp-dist-all
.PHONY: docker_image
docker_image:
	./build_image.sh ${image_name} ${version}

.PHONY: extract
extract: output/quicklisp/$(version)
output/quicklisp/$(version):
	docker run --rm -i -e BUCKET_BASE_URL=${BUCKET_BASE_URL} -v ${PWD}:/app "${image_name}:${version}" /app/extract.sh

.PHONY: upload
upload:
	./upload.sh

.PHONY: clean
clean:
	rm -rf output .output
