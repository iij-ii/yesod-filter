ARG GHC_VER
FROM haskell:${GHC_VER}

WORKDIR /build
ENV PATH=/root/.local/bin:$PATH LC_ALL=C.UTF-8
ARG RESOLVER
RUN stack --resolver ${RESOLVER} new dummy-project yesodweb/simple \
 && cd dummy-project \
 && stack setup \
 && stack build --only-dependencies \
 && stack test --only-dependencies \
 && cd .. \
 && rm -rf dummy-project
