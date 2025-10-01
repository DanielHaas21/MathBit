import { cva, type VariantProps } from 'class-variance-authority';
import * as React from 'react';
import { cn } from '../utils';
import { useTranslation } from '../provider';

const buttonVariants = cva(
  `inline-flex items-center justify-center 
   !cursor-pointer
   whitespace-nowrap font-semibold
   focus-visible:outline focus-visible:outline-2 
   focus-visible:outline-offset-2
   disabled:text-white-50 disabled:cursor-not-allowed disabled:select-none
   transition-all 
   `,
  {
    variants: {
      variant: {
        primary:
          'bg-primary-base text-white-50 hover:bg-primary-hover active:bg-brand-blue-700 disabled:bg-primary-disabled',
        error:
          'bg-error-text text-white-50 hover:bg-error-icon active:bg-error-icon disabled:bg-error-bg',
        warning:
          'bg-warn-text text-white-50 hover:bg-warn-icon active:bg-warn-icon disabled:bg-warn-bg',
        success:
          'bg-success-text text-white-50 hover:bg-success-icon active:bg-success-icon disabled:bg-success-bg',
        neutral: '' //TBA
      },
      outline: {
        primary:
          'border border-brand-blue-800 bg-transparent text-primary-base hover:bg-primary-base active:bg-brand-blue-700 hover:text-white-50',
        error:
          'border border-error-text bg-transparent text-error-text hover:bg-error-text active:bg-error-icon hover:text-white-50',
        warning:
          'border border-warn-text bg-transparent text-warn-text hover:bg-warn-text active:bg-warn-icon hover:text-white-50',
        success:
          'border border-success-text bg-transparent text-success-text hover:bg-success-text active:bg-success-icon hover:text-white-50',
        neutral: ''
      },
      size: {
        sm: 'h-8 px-4 text-sm rounded-md',
        md: 'h-9 px-4 text-base rounded-lg',
        lg: 'h-10 px-5 text-xl rounded-lg',
        xl: 'h-12 px-6 text-2xl rounded-xl',
        full: 'w-full h-10 rounded-lg'
      }
    },
    defaultVariants: {
      variant: 'primary',
      size: 'md'
    }
  }
);

type ButtonVariants = VariantProps<typeof buttonVariants>;

type BaseButtonAttributes = Pick<
  React.ButtonHTMLAttributes<HTMLButtonElement>,
  'onClick' | 'className' | 'disabled' | 'children'
>;

export interface ButtonProps extends BaseButtonAttributes, ButtonVariants {
  locKey?: string | null;
}

export const Button = React.forwardRef<HTMLButtonElement, ButtonProps>((props, ref) => {
  const { variant, size, className, outline, locKey, disabled, children, onClick } = props;
  const t = useTranslation();

  return (
    <button
      className={cn(buttonVariants({ variant, size, className, outline }))}
      disabled={disabled}
      onClick={onClick}
      ref={ref}
    >
      {locKey ? t(`button.${locKey}`) : children}
    </button>
  );
});

Button.displayName = 'Button';
