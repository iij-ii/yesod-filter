ARG BASE_IMAGE
FROM ${BASE_IMAGE}

WORKDIR /build
COPY stack.yaml yesod-filter.cabal LICENSE README.md Setup.hs ./
RUN apt-get update \
 && apt-get install -q -y --no-install-recommends default-libmysqlclient-dev libpcre3-dev \
 && rm -rf /var/lib/apt/lists/* \
 && stack setup \
 && stack build --only-dependencies \
 && stack test --only-dependencies
COPY src ./src
COPY test ./test
