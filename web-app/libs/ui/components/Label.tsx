import * as React from 'react';
import { cn } from '../utils';
import { cva, type VariantProps } from 'class-variance-authority';

type BaseLabelAttributes = Pick<
  React.LabelHTMLAttributes<HTMLLabelElement>,
  'className' | 'htmlFor' | 'children' | 'id' | 'onClick' | 'onBlur' | 'onFocus'
>;

const labelVariants = cva(
  `
     
  `,
  {
    variants: {
      size: {
        xs: 'h-5 text-xs',
        sm: 'h-6 text-sm',
        md: 'h-7 text-base',
        lg: 'h-8 text-lg',
        xl: 'h-10 text-xl',
      },
    },
    defaultVariants: {
      size: 'md',
    },
  }
);

type LabelVariants = VariantProps<typeof labelVariants>;

export interface LabelProps extends BaseLabelAttributes, LabelVariants {}

export const Label = React.forwardRef<HTMLLabelElement, LabelProps>(
  ({ className, size, ...props }, ref) => {
    return <label className={cn(labelVariants({ size, className }))} ref={ref} {...props} />;
  }
);

Label.displayName = 'Label';
