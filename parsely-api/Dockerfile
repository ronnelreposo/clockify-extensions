# Use the official Haskell image for the builder stage
FROM haskell:9.4.7 as builder

# Set the working directory in the builder container
WORKDIR /app

# Copy necessary files for setup and build (e.g., stack.yaml, .cabal file) 
# Adjust this based on your project structure.
COPY stack.yaml ./
COPY *.cabal ./

# Setup GHC
RUN stack setup

# Copy the remaining source files to the builder container
COPY . .

# Build the project and extract the built binary
RUN stack build && cp $(stack path --local-install-root)/bin/api-exe /app/api-exe

# Use a minimal base image for the final image
FROM debian:buster-slim

# Create a directory for the application
WORKDIR /app

# Set the environment variable for the port
ENV API_PORT 8080

# Expose the port for documentation purposes
EXPOSE 8080

# Copy the built binary from the builder stage
COPY --from=builder /app/api-exe /app/api-exe

# Install any runtime dependencies (e.g., libgmp) and clean up
RUN apt-get update && apt-get install -y libgmp10 && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set the CMD to run your application
CMD ["/app/api-exe"]
