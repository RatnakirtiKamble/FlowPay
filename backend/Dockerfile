# Use an official Haskell image with GHC and Cabal installed
FROM haskell:8.10

WORKDIR /app

# Copy the entire project into the container
COPY . .

# Update Cabal package list and build the executable
RUN cabal update
RUN cabal build

# Expose the port your app listens on
EXPOSE 8080

# Run your app executable (replace `your-executable-name` with your actual binary name)
CMD ["cabal", "run", "--", "+RTS", "-N", "-RTS"]
