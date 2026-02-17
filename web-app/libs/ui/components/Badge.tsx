import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';

type BaseBadgeAttributes = Pick<React.HTMLAttributes<HTMLDivElement>, 'className' | 'children'>;

const BadgeVariants = cva(
  `
  inline-flex justify-evenly items-center border-primary-500  rounded-full flex-row flex-nowrap gap-2   
  `,
  {
    variants: {
      size: {
        sm: 'h-5 p-2 text-sm text-nowrap',
        md: 'h-6 p-3 text-md text-nowrap',
        lg: 'h-7 p-4 text-lg text-nowrap',
      },
      color: {
        primary: 'bg-brand-blue-50 text-brand-blue-700',
        secondary: 'bg-brand-orange-50 text-brand-orange-700', // barely used
        warning: 'bg-warn-bg text-warn-text',
        error: 'bg-error-bg text-error-text',
        success: 'bg-success-bg text-success-text',
        neutral: '', // TBA
      },
      outline: {
        primary: 'bg-white-50 border-2 border-brand-blue-200 text-brand-blue-700',
        secondary: 'bg-white-50 border-2 border-secondary-200 text-brand-orange-700', // barely used
        warning: 'bg-white-50 text-warn-text border-2 border-warn-text',
        error: 'bg-white-50 text-error-text border-2 border-error-text',
        success: 'bg-white-50 text-success-text border-2 border-success-text',
        neutral: 'bg-white-50', // TBA
      },
    },
    defaultVariants: {
      size: 'md',
      color: 'primary',
    },
  }
);

type BadgeVariantProps = VariantProps<typeof BadgeVariants>;

export interface BadgeProps extends BaseBadgeAttributes, BadgeVariantProps {
  children?: React.ReactNode;
}

export const Badge = React.forwardRef<HTMLDivElement, BadgeProps>(
  ({ className, outline, color, children, size, ...props }, ref) => {
    return (
      <div ref={ref} className={cn(BadgeVariants({ size, color, outline }), className)} {...props}>
        {children}
      </div>
    );
  }
);

Badge.displayName = 'Badge';
