import { cva, type VariantProps } from 'class-variance-authority';
import * as React from 'react';
import { cn } from '../utils';

const DividerVariants = cva('', {
  variants: {
    variant: {
      light: 'bg-black-100',
      dark: 'bg-black-800'
    },
    orientation: {
      horizontal: '',
      vertical: ''
    },
    size: {
      sm: '',
      half: '',
      lg: '',
      full: ''
    }
  },
  compoundVariants: [
    {
      orientation: 'vertical',
      size: 'sm',
      class: 'h-1/4'
    },
    {
      orientation: 'vertical',
      size: 'lg',
      class: 'h-3/4'
    },
    {
      orientation: 'vertical',
      size: 'half',
      class: 'h-[50%]'
    },
    {
      orientation: 'vertical',
      size: 'full',
      class: 'h-[100%]'
    },
    {
      orientation: 'horizontal',
      size: 'sm',
      class: 'w-1/4'
    },
    {
      orientation: 'horizontal',
      size: 'lg',
      class: 'w-3/4'
    },
    {
      orientation: 'horizontal',
      size: 'half',
      class: 'w-[50%]'
    },
    {
      orientation: 'horizontal',
      size: 'full',
      class: 'w-[100%]'
    }
  ],
  defaultVariants: {
    orientation: 'horizontal',
    size: 'full',
    variant: 'dark'
  }
});

type DividerVariantsProps = VariantProps<typeof DividerVariants>;

export interface DividerProps extends DividerVariantsProps {
  className?: string;
  thickness?: 'sm' | 'md' | 'lg' | 'xl';
}

export const Divider = React.forwardRef<HTMLDivElement, DividerProps>(
  ({ className, size, thickness = 'md', variant, orientation = 'horizontal' }, ref) => {
    const thicknessValue = {
      sm: '1px',
      md: '2px',
      lg: '3px',
      xl: '4px'
    }[thickness];

    return (
      <div
        ref={ref}
        style={{
          height: orientation === 'horizontal' ? thicknessValue : undefined,
          width: orientation === 'vertical' ? thicknessValue : undefined
        }}
        className={cn(DividerVariants({ size, variant, orientation }), className)}
      />
    );
  }
);

Divider.displayName = 'Divider';
