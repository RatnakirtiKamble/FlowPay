FROM haskell:9.4.8 as builder
WORKDIR /app

# Fix 1: Add error handling and verbose output
RUN set -ex && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    libpq-dev \
    libncurses-dev \
    zlib1g-dev \
    libgmp-dev \
    pkg-config \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY stack.yaml package.yaml ./
RUN stack build --only-dependencies

COPY . .
RUN stack build --copy-bins --local-bin-path /app/bin

FROM debian:bookworm-slim
WORKDIR /app

# Fix 2: Apply same pattern to runtime stage
RUN set -ex && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    libpq5 \
    libtinfo6 \
    libgmp10 \
    ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/bin/payment-engine-exe .

EXPOSE 8080
CMD ["./payment-engine-exe"]