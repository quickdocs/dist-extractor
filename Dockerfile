FROM fukamachi/sbcl
ARG DIST_VERSION
ARG BUILD_DATE
ARG VCS_REF
LABEL org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.vcs-url="https://github.com/quickdocs/dist-extractor" \
      org.label-schema.version=$DIST_VERSION \
      org.label-schema.schema-version="1.0"

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends \
    jq \
    file \
    # For cl-fuse
    libfuse-dev \
    # For IOLib
    libfixposix-dev && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /app

RUN set -x; \
  sbcl \
    --eval '(ql:uninstall-dist "quicklisp")' \
    --eval "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/${DIST_VERSION}/distinfo.txt\" :prompt nil)" \
    --quit && \
  sbcl --eval '(mapc (lambda (release) (ignore-errors (handler-bind ((error (function uiop:print-condition-backtrace))) (ql-dist:ensure-installed release)))) (ql-dist:provided-releases t))' --quit

ENTRYPOINT ["/bin/bash"]
