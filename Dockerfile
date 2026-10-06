FROM alpine:3.12
MAINTAINER Jasper Van der Jeugt <m@jaspervdj.be>

RUN apk add --no-cache curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz gzip tar perl git bash sudo binutils-gold
RUN apk add --no-cache zlib zlib-dev zlib-static gmp gmp-dev ncurses-static
RUN curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/0.1.50.2/x86_64-linux-ghcup-0.1.50.2 >/usr/bin/ghcup
RUN chmod +x /usr/bin/ghcup
RUN ghcup install ghc 8.10.7
RUN ghcup set ghc 8.10.7
RUN ghcup install stack 2.15.7
ENV PATH="/root/.ghcup/bin:${PATH}"
RUN stack update

COPY stack.yaml /work/
COPY stack.yaml.lock /work/
COPY zureg.cabal /work/
WORKDIR /work
RUN stack build --system-ghc --only-dependencies \
        --ghc-options='-split-sections -optl-static'

COPY . /work/
RUN mkdir -p /zureg/bin
RUN stack install --system-ghc \
        --ghc-options='-split-sections -optl-static' \
        --local-bin-path=/zureg/bin --copy-bins
