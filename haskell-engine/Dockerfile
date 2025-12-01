# Stage 1: Build
FROM haskell:9.6.3 AS build
WORKDIR /app

# Install stack
RUN apt-get update && apt-get install -y curl && \
    curl -sSL https://get.haskellstack.org/ | sh

# Copy project files
COPY . .

# Build static binary
RUN stack setup && stack install --local-bin-path /app/bin

# Stage 2: Runtime image (small!)
FROM debian:bookworm-slim
WORKDIR /app
COPY --from=build /app/bin/haskell-engine /app/haskell-engine

EXPOSE 8081
CMD ["./haskell-engine"]
