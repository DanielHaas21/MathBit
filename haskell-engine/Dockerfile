FROM haskell:9.6.3 as build
WORKDIR /app

# Copy project files
COPY . .

# Install dependencies and build
RUN stack setup && stack build --copy-bins

# Run container
EXPOSE 8081
CMD ["stack", "exec", "math-service"]