'use client';
import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import { IconName } from '../icons/names';
import { Icon } from './Icon';
import { useMotionValue, animate } from 'framer-motion';

const WidgetVariants = cva(
  `
  min-h-[100px]
  hover:shadow-xl
  transition
  p-6
  border border-white-950 rounded-xl
  relative
  flex items-center
  justify-center
    `,
  {
    variants: {
      variant: {
        white:
          'bg-white-50 hover:bg-[linear-gradient(-135deg,_theme(colors.white.50),_theme(colors.brand-blue.50),_theme(colors.white.50))]',
        grey: 'bg-background-inputs hover:bg-white-300 hover:[&>div:first-child]:bg-primary-base',
        primary: 'bg-primary-base text-text-white hover:bg-brand-blue-700',
        warn: 'bg-warn-bg text-warn-text border-warn-text',
        error: ' bg-error-bg text-error-text border-error-text',
        success: 'bg-success-bg text-success-text border-success-text'
      },
      size: {
        sm: ' min-w-[200px]',
        md: ' min-w-[300px]',
        lg: ' min-w-[400px]',
        fill: 'w-full'
      },
      contentAlignment: {
        left: 'justify-start',
        center: 'justify-center',
        right: 'justify-end'
      }
    },
    defaultVariants: {
      size: 'md',
      contentAlignment: 'center',
      variant: 'grey'
    }
  }
);

type WidgetVariantProps = VariantProps<typeof WidgetVariants>;

export interface WidgetProps extends WidgetVariantProps {
  className?: string;
  titleClassName?: string;
  title?: string | React.ReactNode;
  subtitle?: string | React.ReactNode;
  subtitleClassName?: string;
  children?: React.ReactNode;
  corner?: React.ReactNode | IconName | 'none';
  cornerClassName?: string;
  circle?: boolean;
}

const WidgetBase = React.forwardRef<HTMLDivElement, WidgetProps>(
  (
    {
      className,
      titleClassName,
      title,
      subtitle,
      subtitleClassName,
      corner = 'magnifying-glass',
      cornerClassName,
      children,
      size,
      contentAlignment,
      variant,
      circle = true
    },
    ref
  ) => {
    return (
      <div
        ref={ref}
        className={cn(
          WidgetVariants({ size, contentAlignment, variant }),
          className,
          typeof corner === 'string' &&
            corner === 'none' &&
            ' hover:[&>div:first-child]:bg-transparent'
        )}
      >
        {typeof corner === 'string' && corner !== 'none' ? (
          <div
            className={cn(
              'transition absolute right-2 top-2 w-[20px] h-[20px] flex items-center justify-center rounded-full',
              variant === 'primary'
                ? 'bg-brand-blue-700 group-hover:bg-brand-blue-600'
                : 'bg-black-100 group-hover:bg-primary-base hover:text-text-white',
              !circle && 'bg-transparent group-hover:bg-transparent'
            )}
          >
            <Icon
              name={corner as IconName}
              className={cn(
                'transition text-[10px]',
                cornerClassName,
                circle && 'group-hover:text-text-white'
              )}
            ></Icon>
          </div>
        ) : corner === 'none' ? (
          <></>
        ) : (
          <div className="transition absolute right-2 top-2 flex items-center justify-center">
            {corner}
          </div>
        )}
        <div className="flex gap-2">
          {children}
          <div className="w-fit">
            <h2 className={cn('font-bold text-xl', titleClassName)}>{title}</h2>
            <h3 className={cn('font-normal text-sm', subtitleClassName)}>{subtitle}</h3>
          </div>
        </div>
      </div>
    );
  }
);

WidgetBase.displayName = 'WidgetBase';

export interface CounterProps {
  number: number;
  duration?: number;
  children?: string;
}
/**
 * Utility subcomponent for the Widget that makes a smooth counter animation of a number
 * @param {number} number - animated number
 * @returns
 */
const Counter: React.FC<CounterProps> = ({ number, children, duration = 0.5 }) => {
  const motionValue = useMotionValue(0);
  const [currentValue, setCurrentValue] = React.useState('0');

  const decimalPlaces = React.useMemo(() => {
    const decimals = number.toString().split('.')[1];
    return decimals ? Math.min(decimals.length, 2) : 0; // max 2 decimals so it doesnt look ugly
  }, [number]);

  React.useEffect(() => {
    const controls = animate(motionValue, number, {
      duration,
      ease: 'easeInOut',
      onUpdate: (latest) => {
        const formattedValue =
          decimalPlaces > 0 ? latest.toFixed(decimalPlaces) : Math.round(latest).toString();
        setCurrentValue(formattedValue);
      }
    });

    return controls.stop;
  }, [number, duration, motionValue, decimalPlaces]);

  return (
    <p>
      {currentValue}
      {children}
    </p>
  );
};

Counter.displayName = 'WidgetCounter';

const Widget = Object.assign(WidgetBase, {
  Counter: Counter
});

export { Widget };
