import { useEffect, useRef, useState } from 'react';
import functionPlot, { FunctionPlotDatumScope } from 'function-plot';
import { latexToMathJson, compileExpression } from '../../math';

interface FunctionPlotProps {
  latex: string;
}

export const FunctionPlot : React.FC<FunctionPlotProps> = ({ latex }) =>{
  const ref = useRef<HTMLDivElement>(null);
  const [samples, setSamples] = useState<number>(1000);
  useEffect(() => {
    if (!ref.current) return;

    // Clear previous plot
    ref.current.innerHTML = '';

    // Validate range
    let [xMin, xMax] = [-10, 10];
    if (xMin >= xMax) {
      xMin = -10;
      xMax = 10;
    }

    // Validate empty
    if (!latex || latex.trim() === '') {
      ref.current.innerHTML = 'Enter a valid function';
      return;
    }

    // Parse LaTeX
    const mj = latexToMathJson(latex);
    if (!mj) {
      ref.current.innerHTML = 'Invalid LaTeX';
      return;
    }

    // compiler function, used as callback
    const f = compileExpression(mj);

    // Safe wrapper function that takes f and returns valid function-plot
    const safeFn = (scope: FunctionPlotDatumScope) => {
      try {
        // Clamp x to avoid huge numbers
        const SAFE_X = 1e6;
        const SAFE_Y = 1e6;

        const x = Math.min(SAFE_X, Math.max(-SAFE_X, scope.x));
        const y = f(x);

        // Return clamped finite number or 0
        return Number.isFinite(y) ? Math.min(SAFE_Y, Math.max(-SAFE_Y, y)) : 0;
      } catch {
        return 0; // never throw
      }
    };

    try {
      functionPlot({
        target: ref.current,
        width: 800,
        height: 500,
        xAxis: { domain: [xMin, xMax] },
        yAxis: { domain: undefined }, // auto
        grid: true,
        data: [
          {
            fn: safeFn,
            graphType: 'polyline',
            sampler: 'builtIn',
            nSamples: samples,
          },
        ],
      });
    } catch (err) {
      console.error('Function-plot failed', err);
      ref.current.innerHTML = 'Could not render function';
    }
  }, [latex, samples]);

  return <div ref={ref}></div>;
}

FunctionPlot.displayName = "FunctionPlot"