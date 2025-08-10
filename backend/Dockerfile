FROM debian:bookworm-slim


WORKDIR /app

RUN apt-get update && apt-get install -y \
    libpq5 \
    libtinfo6 \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

COPY bin/payment-engine-exe .

EXPOSE 8080

CMD ["./payment-engine-exe"]