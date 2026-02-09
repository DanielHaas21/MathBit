import { MathEngineErrorResponse, MathEngineSolveResponse } from 'web-api-client';
import * as dotenv from 'dotenv';
import { describe, expect, it } from 'vitest';
dotenv.config();

// since were not in tsoa we have to create a custom error class
// normally wed use badRequest('message', statusCode) but since we want to test the actual response from the haskell engine we need to throw a custom error with the response data and status code
class HaskellEngineHttpError extends Error {
  public readonly status: number;
  public readonly responseBody: unknown;

  constructor(status: number, message: string, responseBody?: unknown) {
    super(message);
    this.name = 'HaskellEngineHttpError';
    this.status = status;
    this.responseBody = responseBody;
  }
}

// before testing make sure to have the haskell engine running on the specified url in .env file
// Typed as unkown since MathJSON is a complex array structure of n-dimensional arrays and objects,
// Also in the real api the type Expression cant be used since it only works in ESM and the backend in is NodeNext which doesnt allow runtime imports anyway
async function mockFetch(rawExpression: unknown): Promise<MathEngineSolveResponse> {
  const requestBody = JSON.stringify({ rawExpression: rawExpression });
  const response: MathEngineSolveResponse = await fetch(process.env.HASKELL_ENGINE_URL + '/solve', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: requestBody,
  }).then(async (res) => {
    if (res.status === 400) {
      const data: MathEngineErrorResponse = await res.json().catch(() => undefined);

      const codeFromBody = data?.errHTTPCode;
      const status = codeFromBody ?? res.status;
      throw new HaskellEngineHttpError(
        status,
        `Haskell engine rejected the request: ${status}`,
        data
      );
    }

    if (!res.ok) {
      throw new HaskellEngineHttpError(
        res.status,
        `Haskell engine responded with status ${res.status}`
      );
    }
    return res.json();
  });
  return response;
}

// Basic arithmetic test

describe('Haskell Engine', () => {

  it('test if the response has the correct structure', async () => {
    const response = await mockFetch(['Add', 85, 58]);
    expect(response).toHaveProperty('finalExpression');
    expect(response.steps).toBeInstanceOf(Array);
  });

  it('should solve a simple expression', async () => {
    const response = await mockFetch(['Add', 85, 58]);
    expect(response.finalExpression).toBe('143');
  });

  it('should solve a symbolic expression', async () => {
    const response = await mockFetch([
      'Add',
      ['Multiply', -33, 'x'],
      ['Multiply', 5, 'x'],
      ['Multiply', 9, 'x'],
    ]);
    expect(response.finalExpression).toBe('- 19x');
  });

  it('should solve a multiplication expression', async () => {
    const response = await mockFetch([
      'Multiply',
      5,
      6,
      'x',
      'x',
      ['Divide', ['Multiply', 5, 'x'], 'x'],
    ]);
    expect(response.finalExpression).toBe('150x^{2}');
  });

  it('should solve a combination of all operations', async () => {
    const response = await mockFetch([
      'Add',
      ['Multiply', 10, 'x', 'x'],
      ['Multiply', -2, 'x'],
      ['Multiply', 6, 'x'],
      ['Multiply', 8, ['Divide', ['Add', ['Multiply', 5, 'x'], ['Negate', 'x']], 'x']],
    ]);
    expect(response.finalExpression).toBe('4x + 32 + 10x^{2}');
  });

  it('should solve a distributive property expression', async () => {
    const response = await mockFetch([
      'Multiply',
      5,
      'x',
      ['Add', ['Multiply', 5, 'x'], ['Negate', ['Divide', ['Multiply', 2, 'x'], 'x']], 9],
    ]);
    expect(response.finalExpression).toBe('35x + 25x^{2}');
  });

  it('should solve a power/sqrt expression', async () => {
    const response = await mockFetch([
      'Multiply',
      2,
      [
        'Power',
        'x',
        [
          'Add',
          ['Multiply', -8, 'x'],
          ['Multiply', 2, 'x'],
          ['Sqrt', ['Divide', ['Multiply', 4, 'x'], 'x']],
        ],
      ],
      ['Root', 'x', 3],
      ['Root', 'x', 3],
    ]);
    expect(response.finalExpression).toBe('2x^{\\frac{2}{3}} \\times  x^{- 6x + 2}');
  });

  it('should reject unknown symbols', async () => {
    await expect(mockFetch(['Decrement', 'x'])).rejects.toMatchObject({
      name: 'HaskellEngineHttpError',
      status: 400,
    });
    await expect(mockFetch(['Decrement', 'x'])).rejects.toThrow(
      'Haskell engine rejected the request: 400'
    );
  });

  // Note that this case will never happen since the frontend validates MathJSON expressions before sending to the backend to reduce traffic
  it('should reject error symbols', async () => {
    await expect(
      mockFetch(['Divide', ['Error', 'missing'], ['Error', 'missing']])
    ).rejects.toMatchObject({
      name: 'HaskellEngineHttpError',
      status: 400,
    });
    await expect(mockFetch(['Divide', ['Error', 'missing'], ['Error', 'missing']])).rejects.toThrow(
      'Haskell engine rejected the request: 400'
    );
  });
});
