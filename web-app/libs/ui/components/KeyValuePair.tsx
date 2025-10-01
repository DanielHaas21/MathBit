import * as React from 'react';
import { cn } from '../utils';
import { cva, type VariantProps } from 'class-variance-authority';

const KeyValuePairVariants = cva(`flex`, {
  variants: {
    orientation: {
      horizontal: 'flex-row gap-2',
      vertical: 'flex-col gap-1'
    },
    size: {
      sm: 'text-sm',
      md: 'text-md',
      lg: 'text-lg',
      xl: 'text-xl'
    }
  },
  defaultVariants: {
    orientation: 'vertical',
    size: 'md'
  }
});

type KeyValuePairVaraintProps = VariantProps<typeof KeyValuePairVariants>;

interface KeyValuePairProps extends KeyValuePairVaraintProps {
  className?: string;
  label?: string;
  value?: React.ReactNode;
  divider?: boolean;
  valueClassName?: string;
  labelClassName?: string;
}

export const KeyValuePair: React.FC<KeyValuePairProps> = ({
  className,
  label,
  value,
  valueClassName,
  labelClassName,
  orientation,
  divider = false,
  size
}) => {
  return (
    <div className={cn(className, KeyValuePairVariants({ orientation, size }))}>
      <h2 className={cn('font-light text-text-grey', labelClassName)}>
        {label} {divider && `:${' '}`}
      </h2>
      <span className={cn(valueClassName)}>{value}</span>
    </div>
  );
};

KeyValuePair.displayName = 'KeyValuePair';
