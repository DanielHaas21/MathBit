import { ComputeEngine } from '@cortex-js/compute-engine';

export interface EvalRequest {
  latex: string;
  xRange: [number, number];
  nSamples: number;
  clampX?: number;
  clampY?: number;
}

export interface EvalResponse {
  points: Array<[number, number]>;
}

// Generate linearly spaced samples
function linspace(min: number, max: number, n: number): number[] {
  if (n <= 1) return [min];
  const step = (max - min) / (n - 1);
  const arr: number[] = new Array(n);
  for (let i = 0; i < n; i++) arr[i] = min + i * step;
  return arr;
}

// Worker message handler
self.onmessage = async (e: MessageEvent<EvalRequest>) => {
  const { latex, xRange, nSamples, clampX = 1e6, clampY = 1e6 } = e.data;

  try {
    const ce = new ComputeEngine();
    const parsed = ce.parse(latex);

    const [xMinIn, xMaxIn] = xRange;
    const xMin = Number.isFinite(xMinIn) ? xMinIn : -25;
    const xMax = Number.isFinite(xMaxIn) ? xMaxIn : 25;
    const xs = linspace(xMin, xMax, Math.max(2, nSamples));

    const points: Array<[number, number]> = [];
    for (const xRaw of xs) {
      const x = Math.min(clampX, Math.max(-clampX, xRaw));
      try {
        const yBox = parsed
          .subs({ x: ce.number(x) })
          .evaluate()
          .N();
        const yVal = yBox.re;
        const y =
          typeof yVal === 'number' && Number.isFinite(yVal)
            ? Math.min(clampY, Math.max(-clampY, yVal))
            : NaN;
        if (Number.isFinite(y)) points.push([x, y]);
      } catch {
        // Skip invalid points
      }
    }

    const response: EvalResponse = { points };
    (self as any).postMessage(response);
  } catch (err) {
    const response: EvalResponse = { points: [] };
    (self as any).postMessage(response);
  }
};

export {};
