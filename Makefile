.PHONY: all
all: docker_image extract

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

.PHONY: quicklisp-projects-version
quicklisp-projects-version: quicklisp-projects
	cd quicklisp-projects \
		&& git checkout "quicklisp-${version}" 2>/dev/null \
		|| git checkout `git rev-list -n 1 --first-parent --before="${version}T23:59:59Z" master`
quicklisp-projects:
	git clone https://github.com/quicklisp/quicklisp-projects

.PHONY: extract
extract: output/quicklisp/$(version)
output/quicklisp/$(version): quicklisp-projects-version
	docker run --rm -i -e BUCKET_BASE_URL=${BUCKET_BASE_URL} -v ${PWD}:/app "${image_name}:${version}" /app/extract.sh

generate-index:
	./generate-index.sh quicklisp ${version}

.PHONY: upload
upload:
	./upload.sh

.PHONY: clean
clean:
	rm -rf output .output
