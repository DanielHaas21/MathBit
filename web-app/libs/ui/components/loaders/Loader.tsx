import React from 'react';
import { cva, VariantProps } from 'class-variance-authority';
import { cn } from '../../utils';

const LoaderVariants = cva('', {
  variants: {
    animation: {
      spin: 'animate-ring-spin',
      progressPulse: 'animate-ring-progress'
    }
  },
  defaultVariants: {
    animation: 'spin'
  }
});

type LoaderVariantProps = VariantProps<typeof LoaderVariants>;

export interface LoaderProps extends LoaderVariantProps {
  size?: number;
  strokeWidth?: number;
  color?: string;
  trackColor?: string;
  arcLength?: number;
  speed?: number;
}

export const Loader: React.FC<LoaderProps> = ({
  size = 202,
  strokeWidth = 15,
  color = '#1A5787',
  trackColor = '#DDDFE1',
  arcLength = 0.25,
  animation = 'spin',
  speed = animation === 'spin' ? 1400 : 2500
}) => {
  const radius = (size - strokeWidth) / 2;
  const circumference = 2 * Math.PI * radius;
  const dashLength = arcLength * circumference;

  return (
    <svg
      width={size}
      height={size}
      viewBox={`0 0 ${size} ${size}`}
      xmlns="http://www.w3.org/2000/svg"
    >
      <circle
        cx={size / 2}
        cy={size / 2}
        r={radius}
        stroke={trackColor}
        strokeWidth={strokeWidth}
        fill="none"
      />
      <circle
        className={cn(LoaderVariants({ animation }))}
        cx={size / 2}
        cy={size / 2}
        r={radius}
        stroke={color}
        strokeWidth={strokeWidth}
        fill="none"
        strokeDasharray={`${dashLength} ${circumference}`}
        strokeDashoffset="0"
        strokeLinecap="round"
        style={{ transformOrigin: '50% 50%', animationDuration: `${speed}ms` }}
      />
      <style>{`
        @keyframes ring-spin {
          100% {
            transform: rotate(360deg);
          }
        }


        @keyframes ring-progress {
          0% {
            stroke-dasharray: 0 ${circumference};
            stroke-dashoffset: 0;
          }
          33% {
            stroke-dasharray: ${circumference} ${circumference};
            stroke-dashoffset: 0;
          }
          66% {
            stroke-dasharray: ${circumference} ${circumference};
            stroke-dashoffset: 0;
          }
          100% {
            stroke-dasharray: 0 ${circumference};
            stroke-dashoffset: -${circumference};
          }
        }

        .animate-ring-spin {
          animation: ring-spin ${speed}ms linear infinite;
        }

        .animate-ring-progress {
           transform: rotate(270deg);
          animation: ring-progress ${speed}ms ease-in-out infinite;
        }
      `}</style>
    </svg>
  );
};
