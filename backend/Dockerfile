# --- Stage 1: The Builder ---
# Use a more stable, slim base image to avoid package manager issues.
FROM haskell:9.4.8-slim-bullseye as builder

# Set the working directory inside the container
WORKDIR /app

# Install necessary system-level dependencies for your Haskell packages.
# This is a more robust command to prevent failures in non-interactive environments.
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    libpq-dev \
    libncurses-dev \
    zlib1g-dev \
    libgmp-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Copy the Stack configuration files first.
# This allows Docker to cache the dependency installation layer.
COPY stack.yaml package.yaml ./

# Install all Haskell dependencies. This is the longest step and will be
# cached as long as your stack.yaml/package.yaml don't change.
RUN stack build --only-dependencies

# Now copy the rest of your backend source code
COPY . .

# Build the executable and copy it to a known location.
# This creates a static binary that we can use in the next stage.
RUN stack build --copy-bins --local-bin-path /app/bin

# --- Stage 2: The Final Image ---
# Use a minimal base image to keep the final container small and secure.
# Debian 'slim' is a good choice.
FROM debian:bookworm-slim

# Set the working directory
WORKDIR /app

# Install only the runtime dependencies needed for the executable.
# 'libpq5' is the runtime library for postgresql, 'libtinfo6' is for ncurses.
RUN apt-get update && apt-get install -y \
    libpq5 \
    libtinfo6 \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Copy the compiled executable from the 'builder' stage
COPY --from=builder /app/bin/payment-engine-exe .

# The PORT environment variable will be provided by Railway.
# This EXPOSE line is for documentation purposes.
EXPOSE 8080

# The command to run your application.
CMD ["./payment-engine-exe"]
