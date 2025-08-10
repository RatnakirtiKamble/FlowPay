# Alternative approach using Ubuntu
FROM ubuntu:22.04 as builder

WORKDIR /app

# Install system dependencies including Haskell
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
    curl \
    build-essential \
    libpq-dev \
    libncurses-dev \
    zlib1g-dev \
    libgmp-dev \
    pkg-config \
    libtinfo-dev \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup and Stack
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:${PATH}"
RUN ghcup install ghc 9.4.8 && \
    ghcup install stack && \
    ghcup set ghc 9.4.8

COPY stack.yaml package.yaml ./
RUN stack build --only-dependencies

COPY . .
RUN stack build --copy-bins --local-bin-path /app/bin

# Runtime stage
FROM ubuntu:22.04
WORKDIR /app

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
    libpq5 \
    libtinfo6 \
    libgmp10 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/bin/payment-engine-exe .

EXPOSE 8080
CMD ["./payment-engine-exe"]