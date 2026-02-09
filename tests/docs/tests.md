# MathBit - `tests`

This package contains the test suites for MathBit, including unit tests, integration tests, and end-to-end tests.
These tests test the following functionalities:

- **Unit Tests**: Focus on testing the Haskell engine's core logic, including the basic operations, problem-solving algorithms, error handling, and edge cases. These tests ensure that individual functions and modules work correctly in isolation.

- **Integration Tests**: Test mainly CRUD round-trips between the frontend and backend, ensuring that the API endpoints work correctly and that the frontend can communicate with the backend together correctly.

- **End-to-End Tests**: Simulate real user interactions with the application, testing the entire system from the frontend to the backend, ensuring that all components work together seamlessly. These tests cover user flows such as creating a new problem, and solving a problem, ensuring that the application behaves as expected in real-world scenarios.

To do e2e specifically, run `pnpm exec playwright install` to install the necessary browsers for testing.

**_Warning: To run any tests make sure to have all the services running, including the Haskell engine and the web application. You can start these services using the provided Docker Compose configuration or by running them individually as needed._**
