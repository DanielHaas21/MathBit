import { CSSProperties, useEffect, useRef, useState } from 'react';
import functionPlot, { FunctionPlotDatumScope } from 'function-plot';
import { latexToMathJson, compileExpression } from '../../math';

interface FunctionPlotProps {
  latex: string;
  className?: string;
  style?: CSSProperties;
  xRange?: [number, number];
  yRange?: [number, number];
}

export const FunctionPlot: React.FC<FunctionPlotProps> = ({ latex, className, style, xRange, yRange }) => {
  const ref = useRef<HTMLDivElement>(null);
  const [samples] = useState<number>(1000);
  const [size, setSize] = useState<{ width: number; height: number }>({ width: 0, height: 0 });

  // Observe container size to render responsively
  useEffect(() => {
    const el = ref.current;
    if (!el) return;

    const updateSize = () => {
      const w = el.clientWidth || 800;
      const h = el.clientHeight || 500;
      setSize((prev) => (prev.width !== w || prev.height !== h ? { width: w, height: h } : prev));
    };

    updateSize();

    // Prefer ResizeObserver for precise container-based resizing
    let ro: ResizeObserver | undefined;
    if ((window as any).ResizeObserver) {
      ro = new ResizeObserver(() => updateSize());
      ro.observe(el);
    } else {
      // Fallback to window resize
      window.addEventListener('resize', updateSize);
    }
    return () => {
      ro?.disconnect();
      window.removeEventListener('resize', updateSize);
    };
  }, []);

  useEffect(() => {
    if (!ref.current) return;

    // Clear previous plot
    ref.current.innerHTML = '';

    // Validate and apply axis ranges (zoom level)
    let [xMin, xMax] = xRange ?? [-25, 25];
    if (!Number.isFinite(xMin) || !Number.isFinite(xMax) || xMin >= xMax) {
      xMin = -25;
      xMax = 25;
    }

    // Prepare data: always render grid; add function only when valid
    let data: any[] = [];
    if (latex && latex.trim() !== '') {
      const mj = latexToMathJson(latex);
      if (mj) {
        const f = compileExpression(mj);
        const safeFn = (scope: FunctionPlotDatumScope) => {
          try {
            const SAFE_X = 1e6;
            const SAFE_Y = 1e6;
            const x = Math.min(SAFE_X, Math.max(-SAFE_X, scope.x));
            const y = f(x);
            return Number.isFinite(y) ? Math.min(SAFE_Y, Math.max(-SAFE_Y, y)) : 0;
          } catch {
            return 0;
          }
        };
        data = [
          {
            fn: safeFn,
            graphType: 'polyline',
            sampler: 'builtIn',
            nSamples: samples,
          },
        ];
      }
    }

    // If no valid function, include a subtle baseline so function-plot still renders
    if (data.length === 0) {
      data = [
        {
          fn: () => 0,
          graphType: 'polyline',
          sampler: 'builtIn',
          nSamples: 2,
          color: 'rgba(0,0,0,0.1)',
        },
      ];
    }

    try {
      functionPlot({
        target: ref.current,
        width: size.width || ref.current.clientWidth || 800,
        height: size.height || ref.current.clientHeight || 500,
        xAxis: { domain: [xMin, xMax] },
        yAxis: { domain: yRange }, // auto when undefined
        grid: true,
        data: [...data],
      });
    } catch (err) {
      console.error('Function-plot failed', err);
      // Keep background silent on errors
    }
  }, [latex, samples, size]);

  return <div ref={ref} className={className} style={style}></div>;
};

FunctionPlot.displayName = 'FunctionPlot';
