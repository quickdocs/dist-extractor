ARG SBCL_VERSION=2.1.1
FROM clfoundation/sbcl:${SBCL_VERSION}
ARG DIST_VERSION

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends jq && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /app

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp

RUN set -x; \
  sbcl --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:uninstall-dist "quicklisp")' \
    --eval "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/${DIST_VERSION}/distinfo.txt\" :prompt nil)" \
    --quit && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  sbcl --eval '(mapc (function ql-dist:ensure-installed) (ql-dist:provided-releases t))' --quit && \
  rm /root/quicklisp.lisp

ENTRYPOINT ["/app/extract.sh"]
