.PHONY: all
all: docker_image extract

BUCKET_BASE_URL=https://storage.googleapis.com/quickdocs-dist

ifndef quicklisp_version
override quicklisp_version = $(shell curl -s -L http://beta.quicklisp.org/dist/quicklisp.txt | grep '^version: ' | sed -e 's/version: //')
endif

ifndef version
override version = $(shell curl -sL https://storage.googleapis.com/quickdocs-dist/quicklisp/info.json | jq -r '.latest_version')
endif

.PHONY: version
version:
	@echo "${version}"
.PHONY: quicklisp_version
quicklisp_version:
	@echo "${quicklisp_version}"

image_name = ghcr.io/quickdocs/quicklisp-dist-all
.PHONY: docker_image
docker_image:
	./build_image.sh ${image_name} ${quicklisp_version}

.PHONY: quicklisp-projects-version
quicklisp-projects-version: quicklisp-projects
	cd quicklisp-projects \
		&& git checkout "quicklisp-${quicklisp_version}" 2>/dev/null \
		|| git checkout `git rev-list -n 1 --first-parent --before="${quicklisp_version}T23:59:59-0400" master`
quicklisp-projects:
	git clone https://github.com/quicklisp/quicklisp-projects

.PHONY: extract
extract: output/quicklisp/$(quicklisp_version)
output/quicklisp/$(quicklisp_version): quicklisp-projects-version
	docker run --rm -i -e BUCKET_BASE_URL=${BUCKET_BASE_URL} -e EXTRACT_ALL=${EXTRACT_ALL} -v ${PWD}:/app "${image_name}:${quicklisp_version}" /app/extract.sh

.PHONY: generate-index
generate-index:
	BUCKET_BASE_URL=${BUCKET_BASE_URL} ./generate-index.sh quicklisp ${quicklisp_version}

.PHONY: upload
upload:
	./upload.sh

.PHONY: clean
clean:
	rm -rf output .output

# GitHub API
.PHONY: github_deployment github_deployment_status
github_deployment:
	curl -s -X POST \
		-H "Authorization: token ${GITHUB_TOKEN}" \
		-H 'Accept: application/vnd.github.v3+json' \
		https://api.github.com/repos/${GITHUB_REPOSITORY}/deployments \
		-d "{\"ref\":\"master\",\"required_contexts\":[],\"payload\":{\"version\":\"${quicklisp_version}\"}}"

github_deployment_status:
	curl -s -X POST \
		-H "Authorization: token ${GITHUB_TOKEN}" \
		-H 'Content-Type: application/json' \
		-H 'Accept: application/vnd.github.v3+json, application/vnd.github.flash-preview+json, application/vnd.github.ant-man-preview+json' \
		https://api.github.com/repos/${GITHUB_REPOSITORY}/deployments/${deployment_id}/statuses \
		-d "{\"state\":\"${state}\", \"description\":\"${description}\", \"log_url\":\"${log_url}\", \"environment_url\":\"${environment_url}\"}"
